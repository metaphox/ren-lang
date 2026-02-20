// Package lexer_test contains integration-style tests for the Ren lexer.
//
// Tests are organised by category:
//   - TestLexer_Keywords        — all 27 Ren keywords
//   - TestLexer_Operators       — every operator including multi-char ones
//   - TestLexer_Literals_Int    — decimal integer literals
//   - TestLexer_Literals_Float  — floating-point literals and edge cases
//   - TestLexer_Literals_String — strings, escape sequences, unterminated strings
//   - TestLexer_Identifiers     — plain identifiers and ident-vs-keyword boundary
//   - TestLexer_Comments        — line comments are skipped, adjacent tokens returned
//   - TestLexer_Position        — line and column tracking across newlines
//   - TestLexer_Program         — end-to-end snippet from the Ren spec
package lexer_test

import (
	"testing"

	"github.com/metaphox/ren-lang/ast"
	"github.com/metaphox/ren-lang/lexer"
)

// tokenCase is a single (type, literal) expectation used in table-driven tests.
type tokenCase struct {
	expectedType    ast.TokenType
	expectedLiteral string
}

// runCases calls NextToken for each case in want and fails the test on mismatch.
func runCases(t *testing.T, input string, want []tokenCase) {
	t.Helper()
	l := lexer.New(input)
	for i, tc := range want {
		tok := l.NextToken()
		if tok.Type != tc.expectedType {
			t.Errorf("case %d: type mismatch — got %d, want %d (literal %q)", i, tok.Type, tc.expectedType, tok.Literal)
		}
		if tok.Literal != tc.expectedLiteral {
			t.Errorf("case %d: literal mismatch — got %q, want %q", i, tok.Literal, tc.expectedLiteral)
		}
	}
}

// ── Keywords ──────────────────────────────────────────────────────────────────

// TestLexer_Keywords verifies that every Ren keyword is recognised and that a
// trailing identifier is classified as IDENT, not as a keyword.
func TestLexer_Keywords(t *testing.T) {
	input := `let mut fn return if else match while for in break continue
struct enum type impl trait mod use pub and or not true false self Self`

	want := []tokenCase{
		{ast.LET, "let"},
		{ast.MUT, "mut"},
		{ast.FN, "fn"},
		{ast.RETURN, "return"},
		{ast.IF, "if"},
		{ast.ELSE, "else"},
		{ast.MATCH, "match"},
		{ast.WHILE, "while"},
		{ast.FOR, "for"},
		{ast.IN, "in"},
		{ast.BREAK, "break"},
		{ast.CONTINUE, "continue"},
		{ast.STRUCT, "struct"},
		{ast.ENUM, "enum"},
		{ast.TYPE, "type"},
		{ast.IMPL, "impl"},
		{ast.TRAIT, "trait"},
		{ast.MOD, "mod"},
		{ast.USE, "use"},
		{ast.PUB, "pub"},
		{ast.AND, "and"},
		{ast.OR, "or"},
		{ast.NOT, "not"},
		{ast.TRUE, "true"},
		{ast.FALSE, "false"},
		{ast.SELF, "self"},
		{ast.SELF_TYPE, "Self"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// TestLexer_KeywordBoundary checks that keyword prefixes used as identifiers are
// not mis-classified. E.g. "letter" must not be split into LET + "ter".
func TestLexer_KeywordBoundary(t *testing.T) {
	input := `letter mutate forEach`
	want := []tokenCase{
		{ast.IDENT, "letter"},
		{ast.IDENT, "mutate"},
		{ast.IDENT, "forEach"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── Operators ────────────────────────────────────────────────────────────────

// TestLexer_Operators verifies every operator, including multi-character ones.
func TestLexer_Operators(t *testing.T) {
	input := `+ - * / % = == != < > <= >= -> ++ .. ..= { } ( ) [ ] , : .`
	want := []tokenCase{
		{ast.PLUS, "+"},
		{ast.MINUS, "-"},
		{ast.ASTERISK, "*"},
		{ast.SLASH, "/"},
		{ast.PERCENT, "%"},
		{ast.ASSIGN, "="},
		{ast.EQ, "=="},
		{ast.NEQ, "!="},
		{ast.LT, "<"},
		{ast.GT, ">"},
		{ast.LTE, "<="},
		{ast.GTE, ">="},
		{ast.ARROW, "->"},
		{ast.CONCAT, "++"},
		{ast.RANGE, ".."},
		{ast.RANGE_INCL, "..="},
		{ast.LBRACE, "{"},
		{ast.RBRACE, "}"},
		{ast.LPAREN, "("},
		{ast.RPAREN, ")"},
		{ast.LBRACKET, "["},
		{ast.RBRACKET, "]"},
		{ast.COMMA, ","},
		{ast.COLON, ":"},
		{ast.DOT, "."},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// TestLexer_RangeDot checks that the range operators are correctly distinguished
// from a lone dot: 0..10 should yield INT RANGE INT, not INT DOT DOT INT.
func TestLexer_RangeDot(t *testing.T) {
	input := `0..10 0..=10 x.y`
	want := []tokenCase{
		{ast.INT, "0"},
		{ast.RANGE, ".."},
		{ast.INT, "10"},
		{ast.INT, "0"},
		{ast.RANGE_INCL, "..="},
		{ast.INT, "10"},
		{ast.IDENT, "x"},
		{ast.DOT, "."},
		{ast.IDENT, "y"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── Integer literals ──────────────────────────────────────────────────────────

// TestLexer_Literals_Int checks decimal integer scanning.
func TestLexer_Literals_Int(t *testing.T) {
	input := `0 42 1000 99`
	want := []tokenCase{
		{ast.INT, "0"},
		{ast.INT, "42"},
		{ast.INT, "1000"},
		{ast.INT, "99"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// TestLexer_Literals_IntBeforeRange ensures that an integer immediately followed
// by ".." is scanned as INT + RANGE, not as a FLOAT.
func TestLexer_Literals_IntBeforeRange(t *testing.T) {
	input := `5..10`
	want := []tokenCase{
		{ast.INT, "5"},
		{ast.RANGE, ".."},
		{ast.INT, "10"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── Float literals ────────────────────────────────────────────────────────────

// TestLexer_Literals_Float checks floating-point literal scanning.
func TestLexer_Literals_Float(t *testing.T) {
	input := `3.14 0.5 100.0`
	want := []tokenCase{
		{ast.FLOAT, "3.14"},
		{ast.FLOAT, "0.5"},
		{ast.FLOAT, "100.0"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── String literals ───────────────────────────────────────────────────────────

// TestLexer_Literals_String checks simple and escape-sequence string scanning.
func TestLexer_Literals_String(t *testing.T) {
	// The raw input contains literal backslash sequences because we want to
	// verify that the lexer interprets them, not the Go string parser.
	input := `"hello" "world\n" "tab\there" "quote\"end" "back\\slash"`
	want := []tokenCase{
		{ast.STRING, "hello"},
		{ast.STRING, "world\n"},
		{ast.STRING, "tab\there"},
		{ast.STRING, "quote\"end"},
		{ast.STRING, "back\\slash"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// TestLexer_Literals_StringEmpty verifies that an empty string literal produces
// a STRING token with an empty literal.
func TestLexer_Literals_StringEmpty(t *testing.T) {
	runCases(t, `""`, []tokenCase{
		{ast.STRING, ""},
		{ast.EOF, ""},
	})
}

// TestLexer_Literals_StringUnterminated verifies that a string with no closing
// quote before EOF produces an ILLEGAL token.
func TestLexer_Literals_StringUnterminated(t *testing.T) {
	runCases(t, `"oops`, []tokenCase{
		{ast.ILLEGAL, "oops"},
		{ast.EOF, ""},
	})
}

// TestLexer_Literals_StringUnterminatedNewline verifies that a string cut short
// by a newline (not a valid Ren string) produces ILLEGAL.
func TestLexer_Literals_StringUnterminatedNewline(t *testing.T) {
	runCases(t, "\"hello\nworld\"", []tokenCase{
		{ast.ILLEGAL, "hello"},
		// After the ILLEGAL the lexer resumes at the newline, which is skipped as whitespace.
		{ast.IDENT, "world"},
		// The trailing '"' opens a new string that hits EOF without closing — another ILLEGAL.
		{ast.ILLEGAL, ""},
		{ast.EOF, ""},
	})
}

// ── Identifiers ───────────────────────────────────────────────────────────────

// TestLexer_Identifiers checks plain identifier scanning.
func TestLexer_Identifiers(t *testing.T) {
	input := `foo bar_baz _internal CamelCase x`
	want := []tokenCase{
		{ast.IDENT, "foo"},
		{ast.IDENT, "bar_baz"},
		{ast.IDENT, "_internal"},
		{ast.IDENT, "CamelCase"},
		{ast.IDENT, "x"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── Comments ──────────────────────────────────────────────────────────────────

// TestLexer_Comments verifies that line comments are skipped entirely, including
// a comment that appears between two real tokens.
func TestLexer_Comments(t *testing.T) {
	input := `// this is a comment
let x = 42 // another comment
let y = 0`

	want := []tokenCase{
		{ast.LET, "let"},
		{ast.IDENT, "x"},
		{ast.ASSIGN, "="},
		{ast.INT, "42"},
		{ast.LET, "let"},
		{ast.IDENT, "y"},
		{ast.ASSIGN, "="},
		{ast.INT, "0"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}

// ── Position tracking ─────────────────────────────────────────────────────────

// TestLexer_Position verifies that tokens carry correct line and column numbers.
func TestLexer_Position(t *testing.T) {
	input := "let x\nfn y"
	l := lexer.New(input)

	type posCase struct {
		lit  string
		line int
		col  int
	}
	cases := []posCase{
		{"let", 1, 1},
		{"x", 1, 5},
		{"fn", 2, 1},
		{"y", 2, 4},
	}

	for i, c := range cases {
		tok := l.NextToken()
		if tok.Literal != c.lit {
			t.Errorf("case %d: literal — got %q, want %q", i, tok.Literal, c.lit)
		}
		if tok.Line != c.line {
			t.Errorf("case %d (%q): line — got %d, want %d", i, c.lit, tok.Line, c.line)
		}
		if tok.Col != c.col {
			t.Errorf("case %d (%q): col — got %d, want %d", i, c.lit, tok.Col, c.col)
		}
	}
}

// ── End-to-end program snippet ────────────────────────────────────────────────

// TestLexer_Program tokenises the FizzBuzz function from the Ren spec (§6) and
// verifies the complete token stream. This exercises keywords, identifiers,
// operators, integer literals, string literals, and delimiters in combination.
func TestLexer_Program(t *testing.T) {
	input := `
fn fizzbuzz(n: Int) -> String {
    if n % 15 == 0      { "FizzBuzz" }
    else if n % 3 == 0  { "Fizz" }
    else if n % 5 == 0  { "Buzz" }
    else                { int_to_string(n) }
}`

	want := []tokenCase{
		{ast.FN, "fn"},
		{ast.IDENT, "fizzbuzz"},
		{ast.LPAREN, "("},
		{ast.IDENT, "n"},
		{ast.COLON, ":"},
		{ast.IDENT, "Int"},
		{ast.RPAREN, ")"},
		{ast.ARROW, "->"},
		{ast.IDENT, "String"},
		{ast.LBRACE, "{"},

		{ast.IF, "if"},
		{ast.IDENT, "n"},
		{ast.PERCENT, "%"},
		{ast.INT, "15"},
		{ast.EQ, "=="},
		{ast.INT, "0"},
		{ast.LBRACE, "{"},
		{ast.STRING, "FizzBuzz"},
		{ast.RBRACE, "}"},

		{ast.ELSE, "else"},
		{ast.IF, "if"},
		{ast.IDENT, "n"},
		{ast.PERCENT, "%"},
		{ast.INT, "3"},
		{ast.EQ, "=="},
		{ast.INT, "0"},
		{ast.LBRACE, "{"},
		{ast.STRING, "Fizz"},
		{ast.RBRACE, "}"},

		{ast.ELSE, "else"},
		{ast.IF, "if"},
		{ast.IDENT, "n"},
		{ast.PERCENT, "%"},
		{ast.INT, "5"},
		{ast.EQ, "=="},
		{ast.INT, "0"},
		{ast.LBRACE, "{"},
		{ast.STRING, "Buzz"},
		{ast.RBRACE, "}"},

		{ast.ELSE, "else"},
		{ast.LBRACE, "{"},
		{ast.IDENT, "int_to_string"},
		{ast.LPAREN, "("},
		{ast.IDENT, "n"},
		{ast.RPAREN, ")"},
		{ast.RBRACE, "}"},

		{ast.RBRACE, "}"},
		{ast.EOF, ""},
	}
	runCases(t, input, want)
}
