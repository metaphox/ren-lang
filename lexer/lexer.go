// Package lexer implements the Ren language lexer (tokeniser).
//
// The lexer converts a Ren source string into a flat stream of [ast.Token] values.
// Call [New] to create a lexer and then call [Lexer.NextToken] repeatedly until
// you receive a token with Type == [ast.EOF].
//
// Design notes:
//   - Single-pass, character-by-character scanning using a read position cursor.
//   - No global state; every [Lexer] is independent.
//   - Line and column numbers are tracked for every token (1-based).
//   - Comments (// …) are consumed silently — no token is emitted.
//   - Identifiers are scanned first and then classified as keywords via
//     [ast.LookupIdent]; this keeps the main switch statement small.
//   - Multi-character operators (.., ..=, ==, !=, <=, >=, ->, ++) require one
//     character of look-ahead and are handled by peekChar.
package lexer

import (
	"github.com/metaphox/ren-lang/ast"
)

// Lexer holds all state required to tokenise a single Ren source string.
// Create one with [New]; never copy a Lexer after first use.
type Lexer struct {
	input   string // the full source text
	pos     int    // current read position (index of ch)
	readPos int    // next read position (pos + 1)
	ch      byte   // current character under examination

	line    int // current 1-based line number
	col     int // 1-based column of ch
	tokCol  int // column at the start of the token being scanned
}

// New creates a [Lexer] that tokenises the given input string.
// The lexer is positioned at the first character; call [Lexer.NextToken]
// immediately to begin scanning.
func New(input string) *Lexer {
	l := &Lexer{
		input: input,
		line:  1,
		col:   0,
	}
	l.readChar() // prime: set l.ch = input[0]
	return l
}

// NextToken returns the next token from the input.
//
// Whitespace (spaces, tabs, carriage returns, newlines) is skipped before each
// token. Comments (// …) are also skipped entirely. When the input is exhausted,
// NextToken returns a token with Type == [ast.EOF] on every subsequent call.
func (l *Lexer) NextToken() ast.Token {
	l.skipWhitespaceAndComments()

	tok := ast.Token{Line: l.line, Col: l.col}

	switch l.ch {
	// ── End of input ────────────────────────────────────────────────────────
	case 0:
		tok = l.makeToken(ast.EOF, "")

	// ── String literal ──────────────────────────────────────────────────────
	case '"':
		tok = l.readString()

	// ── Single-character delimiters ─────────────────────────────────────────
	case '{':
		tok = l.makeToken(ast.LBRACE, "{")
	case '}':
		tok = l.makeToken(ast.RBRACE, "}")
	case '(':
		tok = l.makeToken(ast.LPAREN, "(")
	case ')':
		tok = l.makeToken(ast.RPAREN, ")")
	case '[':
		tok = l.makeToken(ast.LBRACKET, "[")
	case ']':
		tok = l.makeToken(ast.RBRACKET, "]")
	case ',':
		tok = l.makeToken(ast.COMMA, ",")
	case ':':
		tok = l.makeToken(ast.COLON, ":")
	case '%':
		tok = l.makeToken(ast.PERCENT, "%")
	case '/':
		tok = l.makeToken(ast.SLASH, "/")

	// ── Operators that may be one or two characters ─────────────────────────
	case '+':
		if l.peekChar() == '+' {
			l.readChar()
			tok = l.makeToken(ast.CONCAT, "++")
		} else {
			tok = l.makeToken(ast.PLUS, "+")
		}
	case '-':
		if l.peekChar() == '>' {
			l.readChar()
			tok = l.makeToken(ast.ARROW, "->")
		} else {
			tok = l.makeToken(ast.MINUS, "-")
		}
	case '*':
		tok = l.makeToken(ast.ASTERISK, "*")
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			tok = l.makeToken(ast.EQ, "==")
		} else {
			tok = l.makeToken(ast.ASSIGN, "=")
		}
	case '!':
		if l.peekChar() == '=' {
			l.readChar()
			tok = l.makeToken(ast.NEQ, "!=")
		} else {
			tok = l.makeToken(ast.ILLEGAL, string(l.ch))
		}
	case '<':
		if l.peekChar() == '=' {
			l.readChar()
			tok = l.makeToken(ast.LTE, "<=")
		} else {
			tok = l.makeToken(ast.LT, "<")
		}
	case '>':
		if l.peekChar() == '=' {
			l.readChar()
			tok = l.makeToken(ast.GTE, ">=")
		} else {
			tok = l.makeToken(ast.GT, ">")
		}

	// ── Dot: could be DOT, RANGE (..), or RANGE_INCL (..=) ─────────────────
	case '.':
		if l.peekChar() == '.' {
			l.readChar() // consume second '.'
			if l.peekChar() == '=' {
				l.readChar() // consume '='
				tok = l.makeToken(ast.RANGE_INCL, "..=")
			} else {
				tok = l.makeToken(ast.RANGE, "..")
			}
		} else {
			tok = l.makeToken(ast.DOT, ".")
		}

	// ── Identifiers and keywords ─────────────────────────────────────────────
	default:
		if isLetter(l.ch) {
			return l.readIdentifier()
		} else if isDigit(l.ch) {
			return l.readNumber()
		} else {
			tok = l.makeToken(ast.ILLEGAL, string(l.ch))
		}
	}

	l.readChar() // advance past the last character of this token
	return tok
}

// ── Internal helpers ──────────────────────────────────────────────────────────

// readChar advances the lexer by one character.
// When the input is exhausted l.ch is set to 0 (the null byte sentinel for EOF).
// Line and column counters are updated here; col is 1-based.
func (l *Lexer) readChar() {
	if l.readPos >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPos]
	}
	l.pos = l.readPos
	l.readPos++

	// Track position. Newlines bump the line counter and reset the column.
	if l.ch == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col++
	}
}

// peekChar returns the next character without consuming it.
// Returns 0 when the end of input has been reached.
func (l *Lexer) peekChar() byte {
	if l.readPos >= len(l.input) {
		return 0
	}
	return l.input[l.readPos]
}

// makeToken constructs a token at the current source position with the given
// type and literal string.
// It does NOT advance the cursor — the caller is responsible for calling
// readChar after constructing single-character tokens.
func (l *Lexer) makeToken(tt ast.TokenType, literal string) ast.Token {
	return ast.Token{Type: tt, Literal: literal, Line: l.line, Col: l.col}
}

// skipWhitespaceAndComments advances past all whitespace characters and any
// line comments (// … \n) before the next meaningful token.
func (l *Lexer) skipWhitespaceAndComments() {
	for {
		switch l.ch {
		case ' ', '\t', '\r', '\n':
			l.readChar()
		case '/':
			// A second '/' means a line comment — skip to end of line.
			if l.peekChar() == '/' {
				for l.ch != '\n' && l.ch != 0 {
					l.readChar()
				}
			} else {
				return // lone '/' is the division operator — stop skipping
			}
		default:
			return
		}
	}
}

// readIdentifier scans an identifier or keyword starting at the current position.
// It returns the token immediately (without calling readChar) because readNumber
// and readIdentifier leave the cursor ON the last character consumed, and
// NextToken's final readChar() would then skip a character.
//
// Actually: readIdentifier returns early (before the trailing readChar in
// NextToken) so it must NOT call readChar at the end; the cursor is already
// positioned on the first non-identifier character.
func (l *Lexer) readIdentifier() ast.Token {
	startCol := l.col
	startLine := l.line
	start := l.pos

	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}

	literal := l.input[start:l.pos]
	tt := ast.LookupIdent(literal)
	return ast.Token{Type: tt, Literal: literal, Line: startLine, Col: startCol}
}

// readNumber scans an integer or floating-point literal starting at the current
// position. A decimal point followed by more digits turns the token into a FLOAT;
// a lone decimal point (no digits after it) is left for the next call to handle
// as a DOT or RANGE token.
//
// Like readIdentifier, this returns early and does NOT call readChar at the end —
// the cursor is already on the first non-digit, non-dot character.
func (l *Lexer) readNumber() ast.Token {
	startCol := l.col
	startLine := l.line
	start := l.pos
	tt := ast.INT

	for isDigit(l.ch) {
		l.readChar()
	}

	// Check for a decimal point followed by at least one digit → FLOAT.
	// We must NOT consume '.' if the character after it is another '.' (range operator).
	if l.ch == '.' && isDigit(l.peekChar()) {
		tt = ast.FLOAT
		l.readChar() // consume '.'
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	literal := l.input[start:l.pos]
	return ast.Token{Type: tt, Literal: literal, Line: startLine, Col: startCol}
}

// readString scans a double-quoted string literal. The opening '"' has already
// been set as l.ch when this method is called.
//
// Recognised escape sequences: \n  \t  \\  \"
// Any other backslash sequence is passed through as-is (backslash + character).
//
// If the string is not closed before a newline or EOF, an ILLEGAL token is
// returned containing whatever text was scanned up to that point.
func (l *Lexer) readString() ast.Token {
	startCol := l.col
	startLine := l.line

	l.readChar() // skip opening '"'

	var buf []byte
	for {
		switch l.ch {
		case '"':
			// Closing quote found — build the token.
			tok := ast.Token{
				Type:    ast.STRING,
				Literal: string(buf),
				Line:    startLine,
				Col:     startCol,
			}
			l.readChar() // consume closing '"'
			return tok

		case '\\':
			// Escape sequence.
			l.readChar() // consume backslash, now l.ch is the escaped character
			switch l.ch {
			case 'n':
				buf = append(buf, '\n')
			case 't':
				buf = append(buf, '\t')
			case '\\':
				buf = append(buf, '\\')
			case '"':
				buf = append(buf, '"')
			default:
				// Unknown escape: pass through literally (backslash + char).
				buf = append(buf, '\\', l.ch)
			}
			l.readChar()

		case '\n', 0:
			// Unterminated string — return ILLEGAL with what we have.
			return ast.Token{
				Type:    ast.ILLEGAL,
				Literal: string(buf),
				Line:    startLine,
				Col:     startCol,
			}

		default:
			buf = append(buf, l.ch)
			l.readChar()
		}
	}
}

// isLetter reports whether b is a valid identifier-start or identifier-continue
// character. Ren identifiers follow the pattern [a-zA-Z_][a-zA-Z0-9_]*.
func isLetter(b byte) bool {
	return (b >= 'a' && b <= 'z') ||
		(b >= 'A' && b <= 'Z') ||
		b == '_'
}

// isDigit reports whether b is an ASCII decimal digit (0–9).
func isDigit(b byte) bool {
	return b >= '0' && b <= '9'
}
