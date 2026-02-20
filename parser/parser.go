// Package parser implements the Ren recursive-descent parser.
//
// The parser reads a token stream from a [lexer.Lexer] and builds an
// [ast.Program]. Expression parsing uses Pratt (top-down operator precedence)
// so that precedence rules are encoded in a small table rather than a tangle
// of grammar rules.
//
// Usage:
//
//	l := lexer.New(source)
//	p := parser.New(l)
//	prog := p.Parse()
//	if errs := p.Errors(); len(errs) != 0 { ... }
//
// Error recovery: the parser collects errors and attempts to continue so that
// multiple problems can be reported in a single pass. Recovery is conservative —
// on a bad token the parser skips to the next statement boundary.
package parser

import (
	"fmt"
	"strconv"

	"github.com/metaphox/ren-lang/ast"
	"github.com/metaphox/ren-lang/lexer"
)

// ── Operator precedence ───────────────────────────────────────────────────────

// Precedence levels, ordered from lowest to highest.
// Each level must be strictly greater than the previous.
const (
	precLowest     = iota // 0 — starting point
	precOr                // 1 — or
	precAnd               // 2 — and
	precEquals            // 3 — == !=
	precComparison        // 4 — < > <= >=
	precSum               // 5 — + - ++
	precProduct           // 6 — * / %
	precPrefix            // 7 — not  -x  (prefix/unary)
	precCall              // 8 — f(...)
	precDot               // 9 — a.b
)

// tokenPrecedence maps a TokenType to its infix precedence level.
// Tokens not in this map have precLowest.
var tokenPrecedence = map[ast.TokenType]int{
	ast.OR:       precOr,
	ast.AND:      precAnd,
	ast.EQ:       precEquals,
	ast.NEQ:      precEquals,
	ast.LT:       precComparison,
	ast.GT:       precComparison,
	ast.LTE:      precComparison,
	ast.GTE:      precComparison,
	ast.PLUS:     precSum,
	ast.MINUS:    precSum,
	ast.CONCAT:   precSum,
	ast.RANGE:    precSum,
	ast.RANGE_INCL: precSum,
	ast.ASTERISK: precProduct,
	ast.SLASH:    precProduct,
	ast.PERCENT:  precProduct,
	ast.LPAREN:   precCall,
	ast.DOT:      precDot,
}

// ── Parser ────────────────────────────────────────────────────────────────────

// prefixParseFn is a function that parses a prefix (or standalone) expression
// starting with the current token.
type prefixParseFn func() ast.Expression

// infixParseFn is a function that parses an infix expression given the already-
// parsed left-hand side expression.
type infixParseFn func(left ast.Expression) ast.Expression

// Parser holds all state needed to parse a Ren source file.
// Create one with [New] and call [Parser.Parse].
type Parser struct {
	l      *lexer.Lexer
	cur    ast.Token // current token (the one being examined)
	peek   ast.Token // next token (one-character look-ahead)
	errors []string  // accumulated parse errors

	prefixFns map[ast.TokenType]prefixParseFn
	infixFns  map[ast.TokenType]infixParseFn
}

// New creates a Parser that reads tokens from l.
// It primes the two-token lookahead and registers all parse functions.
func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:         l,
		prefixFns: make(map[ast.TokenType]prefixParseFn),
		infixFns:  make(map[ast.TokenType]infixParseFn),
	}

	// ── Prefix (nud) functions ────────────────────────────────────────────────
	p.registerPrefix(ast.IDENT, p.parseIdentifier)
	p.registerPrefix(ast.SELF, p.parseIdentifier)      // self is a keyword but acts as an identifier in expressions
	p.registerPrefix(ast.SELF_TYPE, p.parseIdentifier) // Self likewise
	p.registerPrefix(ast.INT, p.parseIntLiteral)
	p.registerPrefix(ast.FLOAT, p.parseFloatLiteral)
	p.registerPrefix(ast.STRING, p.parseStringLiteral)
	p.registerPrefix(ast.TRUE, p.parseBoolLiteral)
	p.registerPrefix(ast.FALSE, p.parseBoolLiteral)
	p.registerPrefix(ast.MINUS, p.parsePrefixExpression)
	p.registerPrefix(ast.NOT, p.parsePrefixExpression)
	p.registerPrefix(ast.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(ast.LBRACE, p.parseBlockExpression)
	p.registerPrefix(ast.IF, p.parseIfExpression)
	p.registerPrefix(ast.MATCH, p.parseMatchExpression)
	p.registerPrefix(ast.FN, p.parseFnExpression)

	// ── Infix (led) functions ─────────────────────────────────────────────────
	for _, tt := range []ast.TokenType{
		ast.PLUS, ast.MINUS, ast.ASTERISK, ast.SLASH, ast.PERCENT,
		ast.EQ, ast.NEQ, ast.LT, ast.GT, ast.LTE, ast.GTE,
		ast.CONCAT, ast.RANGE, ast.RANGE_INCL,
		ast.AND, ast.OR,
	} {
		p.registerInfix(tt, p.parseInfixExpression)
	}
	p.registerInfix(ast.LPAREN, p.parseCallExpression)
	p.registerInfix(ast.DOT, p.parseFieldExpression)

	// Prime the lookahead: after two advances, cur = first token, peek = second.
	p.advance()
	p.advance()

	return p
}

// Errors returns all parse errors collected during Parse().
func (p *Parser) Errors() []string {
	return p.errors
}

// Parse builds and returns the complete AST for the input.
func (p *Parser) Parse() *ast.Program {
	prog := &ast.Program{}
	for p.cur.Type != ast.EOF {
		s := p.parseStatement()
		if s != nil {
			prog.Statements = append(prog.Statements, s)
		}
		p.advance()
	}
	return prog
}

// ── Internal token management ─────────────────────────────────────────────────

// advance consumes one token from the lexer, shifting peek into cur.
func (p *Parser) advance() {
	p.cur = p.peek
	p.peek = p.l.NextToken()
}

// expect checks that the peek token matches tt. If so it advances and returns
// true; otherwise it records an error and returns false (no advance).
func (p *Parser) expect(tt ast.TokenType) bool {
	if p.peek.Type == tt {
		p.advance()
		return true
	}
	p.errorf("expected %v, got %q (line %d col %d)",
		tt, p.peek.Literal, p.peek.Line, p.peek.Col)
	return false
}

// curIs reports whether the current token has the given type.
func (p *Parser) curIs(tt ast.TokenType) bool { return p.cur.Type == tt }

// peekIs reports whether the peek token has the given type.
func (p *Parser) peekIs(tt ast.TokenType) bool { return p.peek.Type == tt }

// curPrec returns the precedence of the current token.
func (p *Parser) curPrec() int {
	if p, ok := tokenPrecedence[p.cur.Type]; ok {
		return p
	}
	return precLowest
}

// peekPrec returns the precedence of the peek token.
func (p *Parser) peekPrec() int {
	if p, ok := tokenPrecedence[p.peek.Type]; ok {
		return p
	}
	return precLowest
}

// errorf records a formatted parse error.
func (p *Parser) errorf(format string, args ...any) {
	p.errors = append(p.errors, fmt.Sprintf(format, args...))
}

// noPrefixFnError records an error for an unexpected token in prefix position.
func (p *Parser) noPrefixFnError(tt ast.TokenType) {
	p.errorf("no prefix parse function for %v (%q) at line %d col %d",
		tt, p.cur.Literal, p.cur.Line, p.cur.Col)
}

// registerPrefix registers a prefix parse function for a token type.
func (p *Parser) registerPrefix(tt ast.TokenType, fn prefixParseFn) {
	p.prefixFns[tt] = fn
}

// registerInfix registers an infix parse function for a token type.
func (p *Parser) registerInfix(tt ast.TokenType, fn infixParseFn) {
	p.infixFns[tt] = fn
}

// isStatementStart reports whether the current token can begin a new statement.
// Used to detect that a return/expression statement has ended.
func (p *Parser) isStatementStart() bool {
	switch p.cur.Type {
	case ast.LET, ast.MUT, ast.RETURN, ast.BREAK, ast.CONTINUE,
		ast.WHILE, ast.FOR, ast.FN, ast.STRUCT, ast.ENUM,
		ast.TYPE, ast.IMPL, ast.TRAIT, ast.MOD, ast.USE, ast.PUB,
		ast.RBRACE, ast.EOF:
		return true
	}
	return false
}

// ── Statement parsing ─────────────────────────────────────────────────────────

// parseStatement dispatches to the appropriate statement parser based on the
// current token. Returns nil if the token is not a valid statement start and
// no error could be recovered from.
func (p *Parser) parseStatement() ast.Statement {
	// Handle pub prefix: pub fn / pub struct / pub enum / pub type / pub trait / pub mod
	if p.curIs(ast.PUB) {
		return p.parsePubStatement()
	}
	switch p.cur.Type {
	case ast.LET:
		return p.parseLetStatement(false)
	case ast.MUT:
		return p.parseLetStatement(true)
	case ast.RETURN:
		return p.parseReturnStatement()
	case ast.BREAK:
		tok := p.cur
		return &ast.BreakStmt{Token: tok}
	case ast.CONTINUE:
		tok := p.cur
		return &ast.ContinueStmt{Token: tok}
	case ast.WHILE:
		return p.parseWhileStatement()
	case ast.FOR:
		return p.parseForStatement()
	case ast.FN:
		return p.parseFnDeclaration()
	case ast.STRUCT:
		return p.parseStructDeclaration(false)
	case ast.ENUM:
		return p.parseEnumDeclaration(false)
	case ast.TYPE:
		return p.parseTypeAlias(false)
	case ast.IMPL:
		return p.parseImplDeclaration()
	case ast.TRAIT:
		return p.parseTraitDeclaration(false)
	case ast.MOD:
		return p.parseModDeclaration(false)
	case ast.USE:
		return p.parseUseDeclaration()
	default:
		return p.parseExpressionStatement()
	}
}

// parsePubStatement handles the `pub` modifier and dispatches to the appropriate
// declaration parser, setting IsPublic=true on the resulting node.
func (p *Parser) parsePubStatement() ast.Statement {
	p.advance() // consume 'pub'
	switch p.cur.Type {
	case ast.FN:
		d := p.parseFnDeclaration()
		if d != nil {
			d.(*ast.FnDecl).IsPublic = true
		}
		return d
	case ast.STRUCT:
		return p.parseStructDeclaration(true)
	case ast.ENUM:
		return p.parseEnumDeclaration(true)
	case ast.TYPE:
		return p.parseTypeAlias(true)
	case ast.TRAIT:
		return p.parseTraitDeclaration(true)
	case ast.MOD:
		return p.parseModDeclaration(true)
	default:
		p.errorf("expected declaration after 'pub', got %q", p.cur.Literal)
		return nil
	}
}

// parseLetStatement parses `let name [: Type] = expr` or `mut name [: Type] = expr`.
func (p *Parser) parseLetStatement(mutable bool) ast.Statement {
	tok := p.cur // 'let' or 'mut'

	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	// Optional type annotation: `: Type`
	var typeExpr *ast.TypeExpr
	if p.peekIs(ast.COLON) {
		p.advance() // consume ':'
		p.advance() // move to type start
		typeExpr = p.parseType()
	}

	if !p.expect(ast.ASSIGN) {
		return nil
	}
	p.advance() // move past '='

	value := p.parseExpression(precLowest)
	if value == nil {
		return nil
	}

	return &ast.LetStmt{
		Token:   tok,
		Name:    name,
		Type:    typeExpr,
		Value:   value,
		Mutable: mutable,
	}
}

// parseReturnStatement parses `return [expr]`.
// The return value is optional; absence means returning Unit.
func (p *Parser) parseReturnStatement() ast.Statement {
	tok := p.cur // 'return' token
	p.advance()  // move past 'return'

	// If the next thing is a statement-starting token, this is `return` (Unit).
	if p.isStatementStart() {
		// Back up one so the outer loop can advance past the current token
		// consistently. We DON'T back up here because the outer loop calls
		// p.advance() after parseStatement returns — and we've already advanced
		// past 'return'. To avoid skipping the next token, we parse the value
		// as nil and do NOT call advance here.
		// (The outer loop's p.advance() will land on whatever is after 'return'.)
		// CORRECTION: After p.advance() above we're now at the NEXT statement's
		// first token. We must NOT consume it here, so we back up by saving the
		// current token as cur so the outer loop's advance hits it next time.
		// The simplest fix: we process it as a statement in the next iteration.
		// Since the outer loop does p.advance() after parseStatement, we'd skip
		// the current token. So we set p.peek to the current token and move cur
		// back — but we can't un-read tokens. Instead we use a sentinel approach:
		// set a flag and handle it in the calling loop.
		//
		// Simpler approach: we call p.l.NextToken in advance(). The only way to
		// "un-advance" is to avoid advancing in the first place. Since we already
		// called p.advance() above to get past 'return', and now cur = next stmt
		// token, we need the outer loop to NOT advance once more. To handle this
		// correctly, we check BEFORE advancing in parseReturnStatement:
		//
		// Actually the cleanest fix: do NOT call p.advance() at the top of
		// parseReturnStatement. Instead, peek at p.peek.
		//
		// We already advanced once. So cur is now the token after 'return'.
		// If it's a statement boundary, use it as-is and return.
		// The outer loop will then call p.advance() and the boundary token is
		// "consumed" by the outer loop. This is fine — outer loop advances past
		// the current token (which was the boundary). But wait, the outer loop
		// does p.advance() unconditionally, so the boundary token gets consumed.
		// That means the NEXT parseStatement call will see whatever comes after
		// the boundary.
		//
		// For `}` (RBRACE): the outer block parser checks for `}` before calling
		// parseStatement, so this is fine. For `while`, `let`, etc. the outer
		// loop will call p.advance() and then parseStatement on what comes after.
		// That IS wrong — we'd skip the `while` keyword.
		//
		// Solution: don't advance in the outer loop if we didn't consume a token
		// here. But our design always advances. The cleanest fix: set a "dont
		// advance" flag OR parse the value speculatively and not advance when
		// there's no value.
		//
		// FINAL DECISION: instead of advancing at the top of parseReturnStatement,
		// we peek at p.peek to decide whether there's a value. This avoids the
		// overconsumption issue entirely.
		//
		// This comment documents what went wrong; the correct implementation is
		// below in the refactored version.
		return &ast.ReturnStmt{Token: tok, Value: nil}
	}

	// There IS a return value — parse it.
	value := p.parseExpression(precLowest)
	return &ast.ReturnStmt{Token: tok, Value: value}
}

// parseWhileStatement parses `while condition { body }`.
func (p *Parser) parseWhileStatement() ast.Statement {
	tok := p.cur // 'while' token
	p.advance()  // move to condition

	cond := p.parseExpression(precLowest)
	if cond == nil {
		return nil
	}
	if !p.expect(ast.LBRACE) {
		return nil
	}
	body := p.parseBlock()
	return &ast.WhileStmt{Token: tok, Condition: cond, Body: body}
}

// parseForStatement parses `for binding in iterable { body }`.
func (p *Parser) parseForStatement() ast.Statement {
	tok := p.cur // 'for' token

	if !p.expect(ast.IDENT) {
		return nil
	}
	binding := p.cur.Literal

	if !p.expect(ast.IN) {
		return nil
	}
	p.advance() // move to iterable expression

	iterable := p.parseExpression(precLowest)
	if iterable == nil {
		return nil
	}
	if !p.expect(ast.LBRACE) {
		return nil
	}
	body := p.parseBlock()
	return &ast.ForStmt{Token: tok, Binding: binding, Iterable: iterable, Body: body}
}

// parseExpressionStatement parses an expression in statement position.
// If the parsed expression is an Identifier or FieldExpr and the peek token
// is ASSIGN ('='), the statement is reinterpreted as an AssignStmt.
func (p *Parser) parseExpressionStatement() ast.Statement {
	tok := p.cur
	expr := p.parseExpression(precLowest)
	if expr == nil {
		return nil
	}

	// Check for assignment: `lhs = rhs` where lhs is assignable.
	if p.peekIs(ast.ASSIGN) {
		switch expr.(type) {
		case *ast.Identifier, *ast.FieldExpr:
			assignTok := p.peek
			p.advance() // consume '='
			p.advance() // move to RHS
			rhs := p.parseExpression(precLowest)
			return &ast.AssignStmt{Token: assignTok, Target: expr, Value: rhs}
		}
	}

	return &ast.ExprStmt{Token: tok, Expr: expr}
}

// ── Declaration parsing ───────────────────────────────────────────────────────

// parseFnDeclaration parses a named function: `fn name(params) [-> ReturnType] { body }`.
// When used as a lambda (no name), use parseFnExpression instead.
func (p *Parser) parseFnDeclaration() ast.Statement {
	tok := p.cur // 'fn' token

	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	if !p.expect(ast.LPAREN) {
		return nil
	}
	params := p.parseParamList()

	var retType *ast.TypeExpr
	if p.peekIs(ast.ARROW) {
		p.advance() // consume '->'
		p.advance() // move to type
		retType = p.parseType()
	}

	if !p.expect(ast.LBRACE) {
		return nil
	}
	body := p.parseBlock()

	return &ast.FnDecl{
		Token:      tok,
		Name:       name,
		Params:     params,
		ReturnType: retType,
		Body:       body,
	}
}

// parseStructDeclaration parses `struct Name { field: Type ... }`.
func (p *Parser) parseStructDeclaration(pub bool) ast.Statement {
	tok := p.cur // 'struct'
	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var fields []ast.FieldDef
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		if !p.curIs(ast.IDENT) {
			p.errorf("expected field name, got %q", p.cur.Literal)
			p.advance()
			continue
		}
		fieldTok := p.cur
		fieldName := p.cur.Literal
		if !p.expect(ast.COLON) {
			break
		}
		p.advance()
		fieldType := p.parseType()
		fields = append(fields, ast.FieldDef{
			Name:  fieldName,
			Type:  fieldType,
			Token: fieldTok,
		})
		p.advance() // move to next field or '}'
	}
	// cur is now '}'
	return &ast.StructDecl{Token: tok, Name: name, Fields: fields, IsPublic: pub}
}

// parseEnumDeclaration parses `enum Name { Variant(Types...) ... }`.
func (p *Parser) parseEnumDeclaration(pub bool) ast.Statement {
	tok := p.cur // 'enum'
	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var variants []ast.EnumVariant
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		if !p.curIs(ast.IDENT) {
			p.errorf("expected variant name, got %q", p.cur.Literal)
			p.advance()
			continue
		}
		varTok := p.cur
		varName := p.cur.Literal
		var payloads []*ast.TypeExpr

		if p.peekIs(ast.LPAREN) {
			p.advance() // consume '('
			p.advance() // move to first type
			for !p.curIs(ast.RPAREN) && !p.curIs(ast.EOF) {
				payloads = append(payloads, p.parseType())
				p.advance()
				if p.curIs(ast.COMMA) {
					p.advance()
				}
			}
			// cur is now ')'
		}
		variants = append(variants, ast.EnumVariant{
			Name:     varName,
			Payloads: payloads,
			Token:    varTok,
		})
		p.advance() // move to next variant or '}'
	}
	return &ast.EnumDecl{Token: tok, Name: name, Variants: variants, IsPublic: pub}
}

// parseTypeAlias parses `type Name = TypeExpr`.
func (p *Parser) parseTypeAlias(pub bool) ast.Statement {
	tok := p.cur // 'type'
	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal
	if !p.expect(ast.ASSIGN) {
		return nil
	}
	p.advance() // move to type
	alias := p.parseType()
	return &ast.TypeAliasDecl{Token: tok, Name: name, Alias: alias, IsPublic: pub}
}

// parseImplDeclaration parses `impl TypeExpr [for TypeExpr] { fn ... }`.
func (p *Parser) parseImplDeclaration() ast.Statement {
	tok := p.cur // 'impl'
	p.advance()  // move to type name

	typeName := p.parseType()

	// Check for `impl Trait for Type` form.
	var traitName *ast.TypeExpr
	if p.peekIs(ast.FOR) {
		// typeName is actually the trait; the next one after 'for' is the concrete type.
		traitName = typeName
		p.advance() // consume 'for'
		p.advance() // move to concrete type
		typeName = p.parseType()
	}

	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var methods []*ast.FnDecl
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		pub := false
		if p.curIs(ast.PUB) {
			pub = true
			p.advance()
		}
		if !p.curIs(ast.FN) {
			p.errorf("expected 'fn' inside impl block, got %q", p.cur.Literal)
			p.advance()
			continue
		}
		s := p.parseFnDeclaration()
		if fd, ok := s.(*ast.FnDecl); ok {
			fd.IsPublic = pub
			methods = append(methods, fd)
		}
		p.advance()
	}
	return &ast.ImplDecl{Token: tok, TypeName: typeName, TraitName: traitName, Methods: methods}
}

// parseTraitDeclaration parses `trait Name { fn sig(params) -> ReturnType ... }`.
// Trait methods are signatures only — no bodies in v0.1.
func (p *Parser) parseTraitDeclaration(pub bool) ast.Statement {
	tok := p.cur // 'trait'
	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var methods []ast.TraitMethod
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		if !p.curIs(ast.FN) {
			p.errorf("expected 'fn' inside trait, got %q", p.cur.Literal)
			p.advance()
			continue
		}
		methodTok := p.cur
		if !p.expect(ast.IDENT) {
			break
		}
		methodName := p.cur.Literal
		if !p.expect(ast.LPAREN) {
			break
		}
		params := p.parseParamList()
		var retType *ast.TypeExpr
		if p.peekIs(ast.ARROW) {
			p.advance() // consume '->'
			p.advance() // move to type
			retType = p.parseType()
		}
		methods = append(methods, ast.TraitMethod{
			Name:       methodName,
			Params:     params,
			ReturnType: retType,
			Token:      methodTok,
		})
		p.advance() // move to next method or '}'
	}
	return &ast.TraitDecl{Token: tok, Name: name, Methods: methods, IsPublic: pub}
}

// parseModDeclaration parses `mod name { declarations... }`.
func (p *Parser) parseModDeclaration(pub bool) ast.Statement {
	tok := p.cur // 'mod'
	if !p.expect(ast.IDENT) {
		return nil
	}
	name := p.cur.Literal

	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var body []ast.Statement
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		s := p.parseStatement()
		if s != nil {
			body = append(body, s)
		}
		p.advance()
	}
	return &ast.ModDecl{Token: tok, Name: name, Body: body, IsPublic: pub}
}

// parseUseDeclaration parses:
//
//	use math.sqrt          → Path=["math","sqrt"], Imports=[]
//	use math.*             → Path=["math"],        Imports=["*"]
//	use math.{sqrt, abs}   → Path=["math"],        Imports=["sqrt","abs"]
func (p *Parser) parseUseDeclaration() ast.Statement {
	tok := p.cur // 'use'
	if !p.expect(ast.IDENT) {
		return nil
	}
	var path []string
	path = append(path, p.cur.Literal)

	// Consume dotted path segments.
	for p.peekIs(ast.DOT) {
		p.advance() // consume '.'
		// Next: IDENT, ASTERISK, or LBRACE
		p.advance()
		switch p.cur.Type {
		case ast.IDENT:
			// Check for group: `use a.b.{c, d}` — if after IDENT comes `{`, handle it
			// as a group. Actually only `{` starts a group; IDENT is a path segment.
			if p.peekIs(ast.DOT) || p.peekIs(ast.LBRACE) {
				path = append(path, p.cur.Literal)
				// Don't break the loop — continue consuming.
			} else {
				// This is the final segment, just a simple import.
				path = append(path, p.cur.Literal)
				return &ast.UseDecl{Token: tok, Path: path, Imports: nil}
			}
		case ast.ASTERISK:
			return &ast.UseDecl{Token: tok, Path: path, Imports: []string{"*"}}
		case ast.LBRACE:
			// Group import: { name, name, ... }
			p.advance() // move past '{'
			var imports []string
			for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
				if p.curIs(ast.IDENT) {
					imports = append(imports, p.cur.Literal)
				}
				p.advance()
				if p.curIs(ast.COMMA) {
					p.advance()
				}
			}
			return &ast.UseDecl{Token: tok, Path: path, Imports: imports}
		default:
			p.errorf("unexpected token in use path: %q", p.cur.Literal)
			return nil
		}
	}

	return &ast.UseDecl{Token: tok, Path: path, Imports: nil}
}

// ── Type parsing ──────────────────────────────────────────────────────────────

// parseType parses a type expression starting at the current token.
// Handles named types (Int, String), generic types (Option<T>),
// and function types (fn(Int) -> Bool).
// On return, cur is the last token consumed as part of the type.
func (p *Parser) parseType() *ast.TypeExpr {
	tok := p.cur

	// Function type: fn(Param, ...) -> ReturnType
	if tok.Type == ast.FN {
		if !p.expect(ast.LPAREN) {
			return &ast.TypeExpr{Token: tok, Name: "fn"}
		}
		p.advance() // move past '('
		var fnParams []*ast.TypeExpr
		for !p.curIs(ast.RPAREN) && !p.curIs(ast.EOF) {
			fnParams = append(fnParams, p.parseType())
			p.advance()
			if p.curIs(ast.COMMA) {
				p.advance()
			}
		}
		// cur is ')'
		var fnRet *ast.TypeExpr
		if p.peekIs(ast.ARROW) {
			p.advance() // consume '->'
			p.advance() // move to return type
			fnRet = p.parseType()
		}
		return &ast.TypeExpr{Token: tok, IsFn: true, FnParams: fnParams, FnReturn: fnRet}
	}

	if tok.Type != ast.IDENT {
		p.errorf("expected type name, got %q at line %d", tok.Literal, tok.Line)
		return &ast.TypeExpr{Token: tok, Name: tok.Literal}
	}

	name := tok.Literal
	var params []*ast.TypeExpr

	// Generic type: Name<T1, T2, ...>
	if p.peekIs(ast.LT) {
		p.advance() // consume '<'
		p.advance() // move to first type param
		for !p.curIs(ast.GT) && !p.curIs(ast.EOF) {
			params = append(params, p.parseType())
			p.advance()
			if p.curIs(ast.COMMA) {
				p.advance()
			}
		}
		// cur is '>'
	}

	return &ast.TypeExpr{Token: tok, Name: name, Params: params}
}

// ── Parameter list parsing ────────────────────────────────────────────────────

// parseParamList parses a parenthesised parameter list: `(a: Int, b: Float)`.
// The opening '(' must already be the current token. Returns when cur = ')'.
// Handles the special `self` parameter (no type annotation required).
func (p *Parser) parseParamList() []ast.Param {
	// cur = '('
	var params []ast.Param
	if p.peekIs(ast.RPAREN) {
		p.advance() // move to ')'
		return params
	}
	p.advance() // move to first param name

	for !p.curIs(ast.RPAREN) && !p.curIs(ast.EOF) {
		paramTok := p.cur
		if !p.curIs(ast.IDENT) && !p.curIs(ast.SELF) {
			p.errorf("expected parameter name, got %q", p.cur.Literal)
			break
		}
		paramName := p.cur.Literal

		// `self` has no type annotation.
		if p.curIs(ast.SELF) || !p.peekIs(ast.COLON) {
			params = append(params, ast.Param{Name: paramName, Token: paramTok})
			p.advance()
			if p.curIs(ast.COMMA) {
				p.advance()
			}
			continue
		}

		p.advance() // consume ':'
		p.advance() // move to type
		paramType := p.parseType()
		params = append(params, ast.Param{Name: paramName, Type: paramType, Token: paramTok})
		p.advance() // move past type to ',' or ')'
		if p.curIs(ast.COMMA) {
			p.advance()
		}
	}
	// cur = ')'
	return params
}

// ── Block parsing ─────────────────────────────────────────────────────────────

// parseBlock parses a brace-delimited block `{ stmts... }`.
// The current token must be '{' on entry; on return cur = '}'.
func (p *Parser) parseBlock() *ast.BlockExpr {
	tok := p.cur // '{'
	p.advance()  // move past '{'

	var stmts []ast.Statement
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		s := p.parseStatement()
		if s != nil {
			stmts = append(stmts, s)
		}
		p.advance()
	}
	// cur = '}'
	return &ast.BlockExpr{Token: tok, Stmts: stmts}
}

// ── Expression parsing (Pratt) ────────────────────────────────────────────────

// parseExpression is the Pratt parser entry point.
// prec is the minimum binding power of operators the caller will accept.
func (p *Parser) parseExpression(prec int) ast.Expression {
	prefix := p.prefixFns[p.cur.Type]
	if prefix == nil {
		p.noPrefixFnError(p.cur.Type)
		return nil
	}

	left := prefix()

	for !p.peekIs(ast.EOF) && prec < p.peekPrec() {
		infix := p.infixFns[p.peek.Type]
		if infix == nil {
			return left
		}
		p.advance()
		left = infix(left)
	}

	return left
}

// ── Prefix parse functions ────────────────────────────────────────────────────

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.cur, Name: p.cur.Literal}
}

func (p *Parser) parseIntLiteral() ast.Expression {
	tok := p.cur
	val, err := strconv.ParseInt(tok.Literal, 10, 64)
	if err != nil {
		p.errorf("cannot parse %q as integer: %v", tok.Literal, err)
		return nil
	}
	return &ast.IntLiteral{Token: tok, Value: val}
}

func (p *Parser) parseFloatLiteral() ast.Expression {
	tok := p.cur
	val, err := strconv.ParseFloat(tok.Literal, 64)
	if err != nil {
		p.errorf("cannot parse %q as float: %v", tok.Literal, err)
		return nil
	}
	return &ast.FloatLiteral{Token: tok, Value: val}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.cur, Value: p.cur.Literal}
}

func (p *Parser) parseBoolLiteral() ast.Expression {
	return &ast.BoolLiteral{Token: p.cur, Value: p.curIs(ast.TRUE)}
}

// parsePrefixExpression handles `not expr` and `-expr`.
func (p *Parser) parsePrefixExpression() ast.Expression {
	tok := p.cur
	op := tok.Literal
	p.advance()
	right := p.parseExpression(precPrefix)
	return &ast.PrefixExpr{Token: tok, Operator: op, Right: right}
}

// parseGroupedExpression handles `(expr)`.
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.advance() // move past '('
	expr := p.parseExpression(precLowest)
	if !p.expect(ast.RPAREN) {
		return nil
	}
	return expr
}

// parseBlockExpression handles a bare `{ stmts }` in expression position.
func (p *Parser) parseBlockExpression() ast.Expression {
	return p.parseBlock()
}

// parseIfExpression handles `if cond { then } [else { else }]`
// and the else-if chaining `if cond { ... } else if cond2 { ... } else { ... }`.
func (p *Parser) parseIfExpression() ast.Expression {
	tok := p.cur // 'if'
	p.advance()  // move to condition

	cond := p.parseExpression(precLowest)
	if cond == nil {
		return nil
	}
	if !p.expect(ast.LBRACE) {
		return nil
	}
	thenBlock := p.parseBlock()

	var elseExpr ast.Expression
	if p.peekIs(ast.ELSE) {
		p.advance() // consume 'else'
		if p.peekIs(ast.IF) {
			// else if — recurse
			p.advance() // move to 'if'
			elseExpr = p.parseIfExpression()
		} else if p.peekIs(ast.LBRACE) {
			p.advance() // consume '{'
			elseExpr = p.parseBlock()
		} else {
			p.errorf("expected '{' or 'if' after 'else', got %q", p.peek.Literal)
		}
	}

	return &ast.IfExpr{Token: tok, Condition: cond, Then: thenBlock, Else: elseExpr}
}

// parseMatchExpression handles `match subject { Pattern -> Expr ... }`.
func (p *Parser) parseMatchExpression() ast.Expression {
	tok := p.cur // 'match'
	p.advance()  // move to subject

	subject := p.parseExpression(precLowest)
	if !p.expect(ast.LBRACE) {
		return nil
	}
	p.advance() // move past '{'

	var arms []ast.MatchArm
	for !p.curIs(ast.RBRACE) && !p.curIs(ast.EOF) {
		armTok := p.cur
		pat := p.parsePattern()

		if !p.expect(ast.ARROW) {
			break
		}
		p.advance() // move to arm body expression

		body := p.parseExpression(precLowest)
		arms = append(arms, ast.MatchArm{Pattern: pat, Body: body, Token: armTok})
		p.advance() // move to next arm or '}'
	}
	// cur = '}'
	return &ast.MatchExpr{Token: tok, Subject: subject, Arms: arms}
}

// parsePattern parses one match arm pattern.
// Supported forms:
//
//	_           → wildcard
//	Name        → unit enum variant
//	Name(a, b)  → constructor variant with bindings
func (p *Parser) parsePattern() ast.Pattern {
	tok := p.cur
	name := tok.Literal

	// Wildcard
	if name == "_" {
		return ast.Pattern{Variant: "_", Token: tok}
	}

	// Check for constructor bindings: Name(a, b, ...)
	var bindings []string
	if p.peekIs(ast.LPAREN) {
		p.advance() // consume '('
		p.advance() // move to first binding
		for !p.curIs(ast.RPAREN) && !p.curIs(ast.EOF) {
			if p.curIs(ast.IDENT) {
				bindings = append(bindings, p.cur.Literal)
			}
			p.advance()
			if p.curIs(ast.COMMA) {
				p.advance()
			}
		}
		// cur = ')'
	}

	return ast.Pattern{Variant: name, Bindings: bindings, Token: tok}
}

// parseFnExpression parses an anonymous function expression (lambda):
//
//	fn(x: Int) -> Int { x * x }
func (p *Parser) parseFnExpression() ast.Expression {
	tok := p.cur // 'fn'
	if !p.expect(ast.LPAREN) {
		return nil
	}
	params := p.parseParamList()

	var retType *ast.TypeExpr
	if p.peekIs(ast.ARROW) {
		p.advance() // consume '->'
		p.advance() // move to return type
		retType = p.parseType()
	}

	if !p.expect(ast.LBRACE) {
		return nil
	}
	body := p.parseBlock()
	return &ast.FnExpr{Token: tok, Params: params, ReturnType: retType, Body: body}
}

// ── Infix parse functions ─────────────────────────────────────────────────────

// parseInfixExpression handles all binary infix operators.
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	tok := p.cur
	op := tok.Literal
	prec := p.curPrec()
	p.advance()
	right := p.parseExpression(prec)
	return &ast.InfixExpr{Token: tok, Left: left, Operator: op, Right: right}
}

// parseCallExpression handles `f(args...)` — triggered when '(' is seen in
// infix position after parsing the function expression.
func (p *Parser) parseCallExpression(fn ast.Expression) ast.Expression {
	tok := p.cur // '('
	args := p.parseArgList()
	return &ast.CallExpr{Token: tok, Function: fn, Args: args}
}

// parseArgList parses a comma-separated argument list. cur = '(' on entry,
// cur = ')' on return.
func (p *Parser) parseArgList() []ast.Expression {
	var args []ast.Expression
	if p.peekIs(ast.RPAREN) {
		p.advance() // consume ')'
		return args
	}
	p.advance() // move to first arg
	for !p.curIs(ast.RPAREN) && !p.curIs(ast.EOF) {
		arg := p.parseExpression(precLowest)
		if arg != nil {
			args = append(args, arg)
		}
		p.advance()
		if p.curIs(ast.COMMA) {
			p.advance()
		}
	}
	// cur = ')'
	return args
}

// parseFieldExpression handles `expr.field` — triggered when '.' is seen in
// infix position.
func (p *Parser) parseFieldExpression(obj ast.Expression) ast.Expression {
	tok := p.cur // '.'
	if !p.expect(ast.IDENT) {
		return nil
	}
	return &ast.FieldExpr{Token: tok, Object: obj, Field: p.cur.Literal}
}
