// Package ast defines the token types and the Token struct used by the Ren lexer and parser.
//
// Tokens are the smallest meaningful units of a Ren source file. Every token carries its
// type, the exact literal text it was scanned from, and its source position (line + column).
// Position is 1-based: the first character of a file is Line 1, Col 1.
package ast

// TokenType identifies the category of a scanned token.
// The zero value (0) is reserved and not a valid token.
type TokenType int

const (
	// ── Special ────────────────────────────────────────────────────────────────

	// ILLEGAL represents a character or sequence the lexer could not recognise,
	// such as an unterminated string literal or an unexpected byte value.
	ILLEGAL TokenType = iota
	// EOF marks the end of the input stream. The parser stops when it sees EOF.
	EOF

	// ── Literals ───────────────────────────────────────────────────────────────

	// IDENT is an identifier: [a-zA-Z_][a-zA-Z0-9_]*
	// Identifiers that match a keyword are re-classified to their keyword type
	// by the lexer before the token is returned.
	IDENT
	// INT is a decimal integer literal, e.g. 0, 42, 1_000 (underscores not in v0.1).
	INT
	// FLOAT is a decimal floating-point literal, e.g. 3.14, 0.5.
	// The literal must contain a '.' character.
	FLOAT
	// STRING is a double-quoted UTF-8 string literal, e.g. "hello\nworld".
	// Recognised escape sequences: \n  \t  \\  \"
	// An unterminated string (no closing '"' before newline or EOF) produces ILLEGAL.
	STRING

	// ── Keywords ───────────────────────────────────────────────────────────────

	// LET introduces an immutable binding: let x = 42
	LET
	// MUT introduces a mutable binding: mut count = 0
	MUT
	// FN introduces a function declaration or lambda expression: fn add(a: Int, b: Int) -> Int
	FN
	// RETURN performs an early return from a function: return None
	RETURN
	// IF begins a conditional expression: if x > 0 { ... }
	IF
	// ELSE is the else branch of an if expression.
	ELSE
	// MATCH begins a pattern-matching expression: match shape { ... }
	MATCH
	// WHILE begins a conditional loop: while i < 10 { ... }
	WHILE
	// FOR begins an iterator loop: for item in list { ... }
	FOR
	// IN separates the binding from the iterator in a for loop: for i in 0..10
	IN
	// BREAK exits the nearest enclosing loop.
	BREAK
	// CONTINUE skips to the next iteration of the nearest enclosing loop.
	CONTINUE
	// STRUCT introduces a product-type declaration: struct Point { x: Float }
	STRUCT
	// ENUM introduces a sum-type (ADT) declaration: enum Shape { Circle(Float) }
	ENUM
	// TYPE introduces a type alias: type UserId = Int
	TYPE
	// IMPL attaches method implementations to a type: impl Point { ... }
	IMPL
	// TRAIT defines a named interface: trait Show { fn show(self) -> String }
	TRAIT
	// MOD defines a named module namespace: mod math { ... }
	MOD
	// USE brings names from a module into the current scope: use math.sqrt
	USE
	// PUB marks an item as publicly visible outside its module.
	PUB
	// AND is the logical-AND operator (replaces &&): if a and b
	AND
	// OR is the logical-OR operator (replaces ||): if a or b
	OR
	// NOT is the logical-NOT operator (replaces !): if not suspended
	NOT
	// TRUE is the boolean literal true.
	TRUE
	// FALSE is the boolean literal false.
	FALSE
	// SELF (lowercase) refers to the receiver instance inside an impl block.
	SELF
	// SELF_TYPE (Self, uppercase) refers to the implementing type inside a trait.
	SELF_TYPE

	// ── Arithmetic operators ────────────────────────────────────────────────────

	// PLUS is the addition operator: a + b
	PLUS
	// MINUS is subtraction or unary negation: a - b  /  -x
	MINUS
	// ASTERISK is the multiplication operator: a * b
	// It is also used as a glob in use statements: use math.*
	ASTERISK
	// SLASH is the division operator: a / b
	SLASH
	// PERCENT is the remainder operator: n % 2
	PERCENT

	// ── Comparison operators ────────────────────────────────────────────────────

	// EQ is the equality operator: a == b
	EQ
	// NEQ is the inequality operator: a != b
	NEQ
	// LT is the less-than operator: a < b
	LT
	// GT is the greater-than operator: a > b
	GT
	// LTE is the less-than-or-equal operator: a <= b
	LTE
	// GTE is the greater-than-or-equal operator: a >= b
	GTE

	// ── Other operators ─────────────────────────────────────────────────────────

	// ASSIGN is the assignment operator: count = count + 1
	ASSIGN
	// ARROW is the function-return-type and match-arm separator: -> Int
	ARROW
	// CONCAT is the string concatenation operator: s1 ++ s2
	CONCAT
	// RANGE is the exclusive range operator: 0..10  (produces 0 through 9)
	RANGE
	// RANGE_INCL is the inclusive range operator: 0..=10  (produces 0 through 10)
	RANGE_INCL

	// ── Delimiters ──────────────────────────────────────────────────────────────

	// LBRACE is the left curly brace: {
	LBRACE
	// RBRACE is the right curly brace: }
	RBRACE
	// LPAREN is the left parenthesis: (
	LPAREN
	// RPAREN is the right parenthesis: )
	RPAREN
	// LBRACKET is the left square bracket: [
	LBRACKET
	// RBRACKET is the right square bracket: ]
	RBRACKET
	// COMMA is the argument and field separator: ,
	COMMA
	// COLON is the type annotation separator: x: Int
	COLON
	// DOT is the field access operator: point.x
	// Note: consecutive dots are scanned as RANGE or RANGE_INCL, not as DOT.
	DOT
)

// keywords maps the literal text of every Ren keyword to its TokenType.
// The lexer consults this map when it finishes scanning an identifier.
var keywords = map[string]TokenType{
	"let":      LET,
	"mut":      MUT,
	"fn":       FN,
	"return":   RETURN,
	"if":       IF,
	"else":     ELSE,
	"match":    MATCH,
	"while":    WHILE,
	"for":      FOR,
	"in":       IN,
	"break":    BREAK,
	"continue": CONTINUE,
	"struct":   STRUCT,
	"enum":     ENUM,
	"type":     TYPE,
	"impl":     IMPL,
	"trait":    TRAIT,
	"mod":      MOD,
	"use":      USE,
	"pub":      PUB,
	"and":      AND,
	"or":       OR,
	"not":      NOT,
	"true":     TRUE,
	"false":    FALSE,
	"self":     SELF,
	"Self":     SELF_TYPE,
}

// LookupIdent checks whether ident is a reserved keyword and returns the
// corresponding TokenType. If ident is not a keyword, IDENT is returned.
func LookupIdent(ident string) TokenType {
	if tt, ok := keywords[ident]; ok {
		return tt
	}
	return IDENT
}

// Token is a single lexical unit produced by the Ren lexer.
//
// Fields:
//   - Type    — the category of this token (see TokenType constants)
//   - Literal — the exact source text that was scanned
//   - Line    — 1-based source line number
//   - Col     — 1-based column of the first character of this token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Col     int
}

// String returns a human-readable representation of the token, useful for
// debugging and error messages. It does not replicate the exact Ren source.
func (t Token) String() string {
	return t.Literal
}
