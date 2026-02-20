// Package parser_test contains tests for the Ren recursive-descent parser.
//
// Tests are written first (TDD) and cover every grammar construct defined in
// SPEC.md v0.1. Each test parses a snippet, inspects the returned AST via
// type assertions, and fails with a descriptive message on mismatch.
//
// Test categories:
//   - Statements:   let, mut, return, break, continue, assign, while, for
//   - Expressions:  literals, prefix, infix (with precedence), if, block,
//                   match, call, field access, fn literal, range
//   - Declarations: fn, struct, enum, type alias, impl, trait, mod, use
//   - Programs:     FizzBuzz and factorial from SPEC.md §6
package parser_test

import (
	"fmt"
	"testing"

	"github.com/metaphox/ren-lang/ast"
	"github.com/metaphox/ren-lang/lexer"
	"github.com/metaphox/ren-lang/parser"
)

// ── Helpers ───────────────────────────────────────────────────────────────────

// parse runs the full parser on input and fails the test if any parse errors
// were collected or if the number of top-level statements doesn't match want.
func parse(t *testing.T, input string, wantStmts int) *ast.Program {
	t.Helper()
	l := lexer.New(input)
	p := parser.New(l)
	prog := p.Parse()

	errs := p.Errors()
	if len(errs) > 0 {
		t.Errorf("parser produced %d error(s):", len(errs))
		for _, e := range errs {
			t.Errorf("  %s", e)
		}
		t.FailNow()
	}
	if len(prog.Statements) != wantStmts {
		t.Fatalf("expected %d statements, got %d", wantStmts, len(prog.Statements))
	}
	return prog
}

// firstStmt is a convenience wrapper that returns the first statement after
// calling parse with wantStmts=1.
func firstStmt(t *testing.T, input string) ast.Statement {
	t.Helper()
	return parse(t, input, 1).Statements[0]
}

// expectNoErrors is like parse but does not assert statement count.
func expectNoErrors(t *testing.T, input string) *ast.Program {
	t.Helper()
	l := lexer.New(input)
	p := parser.New(l)
	prog := p.Parse()
	errs := p.Errors()
	if len(errs) > 0 {
		t.Errorf("parser produced %d error(s):", len(errs))
		for _, e := range errs {
			t.Errorf("  %s", e)
		}
		t.FailNow()
	}
	return prog
}

// assertIdent checks that expr is an *ast.Identifier with the given name.
func assertIdent(t *testing.T, expr ast.Expression, name string) *ast.Identifier {
	t.Helper()
	id, ok := expr.(*ast.Identifier)
	if !ok {
		t.Fatalf("expected *ast.Identifier, got %T", expr)
	}
	if id.Name != name {
		t.Fatalf("identifier name: got %q, want %q", id.Name, name)
	}
	return id
}

// assertIntLit checks that expr is an *ast.IntLiteral with the given value.
func assertIntLit(t *testing.T, expr ast.Expression, val int64) {
	t.Helper()
	lit, ok := expr.(*ast.IntLiteral)
	if !ok {
		t.Fatalf("expected *ast.IntLiteral, got %T (%s)", expr, expr.String())
	}
	if lit.Value != val {
		t.Fatalf("IntLiteral value: got %d, want %d", lit.Value, val)
	}
}

// assertInfix checks that expr is an *ast.InfixExpr with the given operator.
func assertInfix(t *testing.T, expr ast.Expression, op string) *ast.InfixExpr {
	t.Helper()
	inf, ok := expr.(*ast.InfixExpr)
	if !ok {
		t.Fatalf("expected *ast.InfixExpr, got %T", expr)
	}
	if inf.Operator != op {
		t.Fatalf("infix operator: got %q, want %q", inf.Operator, op)
	}
	return inf
}

// exprOf extracts the Expression from an ExprStmt, failing the test otherwise.
func exprOf(t *testing.T, stmt ast.Statement) ast.Expression {
	t.Helper()
	es, ok := stmt.(*ast.ExprStmt)
	if !ok {
		t.Fatalf("expected *ast.ExprStmt, got %T", stmt)
	}
	return es.Expr
}

// ── Let / Mut statements ──────────────────────────────────────────────────────

func TestParser_LetStatement(t *testing.T) {
	s := firstStmt(t, `let x = 42`)
	ls, ok := s.(*ast.LetStmt)
	if !ok {
		t.Fatalf("expected *ast.LetStmt, got %T", s)
	}
	if ls.Name != "x" {
		t.Errorf("name: got %q, want %q", ls.Name, "x")
	}
	if ls.Mutable {
		t.Error("expected Mutable=false")
	}
	assertIntLit(t, ls.Value, 42)
}

func TestParser_LetStatementWithType(t *testing.T) {
	s := firstStmt(t, `let name: String = "Tao"`)
	ls := s.(*ast.LetStmt)
	if ls.Name != "name" {
		t.Errorf("name: got %q", ls.Name)
	}
	if ls.Type == nil || ls.Type.Name != "String" {
		t.Errorf("type annotation: got %v", ls.Type)
	}
	str, ok := ls.Value.(*ast.StringLiteral)
	if !ok || str.Value != "Tao" {
		t.Errorf("value: got %v", ls.Value)
	}
}

func TestParser_MutStatement(t *testing.T) {
	s := firstStmt(t, `mut count = 0`)
	ls, ok := s.(*ast.LetStmt)
	if !ok {
		t.Fatalf("expected *ast.LetStmt, got %T", s)
	}
	if !ls.Mutable {
		t.Error("expected Mutable=true")
	}
	if ls.Name != "count" {
		t.Errorf("name: got %q", ls.Name)
	}
	assertIntLit(t, ls.Value, 0)
}

// ── Return / Break / Continue ─────────────────────────────────────────────────

func TestParser_ReturnStatement(t *testing.T) {
	s := firstStmt(t, `return 42`)
	rs, ok := s.(*ast.ReturnStmt)
	if !ok {
		t.Fatalf("expected *ast.ReturnStmt, got %T", s)
	}
	assertIntLit(t, rs.Value, 42)
}

func TestParser_ReturnUnit(t *testing.T) {
	s := firstStmt(t, `return`)
	rs, ok := s.(*ast.ReturnStmt)
	if !ok {
		t.Fatalf("expected *ast.ReturnStmt, got %T", s)
	}
	if rs.Value != nil {
		t.Errorf("expected nil value, got %v", rs.Value)
	}
}

func TestParser_BreakStatement(t *testing.T) {
	s := firstStmt(t, `break`)
	if _, ok := s.(*ast.BreakStmt); !ok {
		t.Fatalf("expected *ast.BreakStmt, got %T", s)
	}
}

func TestParser_ContinueStatement(t *testing.T) {
	s := firstStmt(t, `continue`)
	if _, ok := s.(*ast.ContinueStmt); !ok {
		t.Fatalf("expected *ast.ContinueStmt, got %T", s)
	}
}

// ── Assignment ────────────────────────────────────────────────────────────────

func TestParser_AssignStatement(t *testing.T) {
	s := firstStmt(t, `count = count + 1`)
	as, ok := s.(*ast.AssignStmt)
	if !ok {
		t.Fatalf("expected *ast.AssignStmt, got %T", s)
	}
	assertIdent(t, as.Target, "count")
	rhs := assertInfix(t, as.Value, "+")
	assertIdent(t, rhs.Left, "count")
	assertIntLit(t, rhs.Right, 1)
}

func TestParser_AssignFieldStatement(t *testing.T) {
	s := firstStmt(t, `self.x = 3.14`)
	as, ok := s.(*ast.AssignStmt)
	if !ok {
		t.Fatalf("expected *ast.AssignStmt, got %T", s)
	}
	fe, ok := as.Target.(*ast.FieldExpr)
	if !ok {
		t.Fatalf("expected *ast.FieldExpr target, got %T", as.Target)
	}
	assertIdent(t, fe.Object, "self")
	if fe.Field != "x" {
		t.Errorf("field: got %q, want %q", fe.Field, "x")
	}
}

// ── While / For ───────────────────────────────────────────────────────────────

func TestParser_WhileStatement(t *testing.T) {
	s := firstStmt(t, `while i < 10 { i = i + 1 }`)
	ws, ok := s.(*ast.WhileStmt)
	if !ok {
		t.Fatalf("expected *ast.WhileStmt, got %T", s)
	}
	cond := assertInfix(t, ws.Condition, "<")
	assertIdent(t, cond.Left, "i")
	assertIntLit(t, cond.Right, 10)
	if len(ws.Body.Stmts) != 1 {
		t.Errorf("body statements: got %d, want 1", len(ws.Body.Stmts))
	}
}

func TestParser_ForStatement(t *testing.T) {
	s := firstStmt(t, `for i in 0..10 { print(i) }`)
	fs, ok := s.(*ast.ForStmt)
	if !ok {
		t.Fatalf("expected *ast.ForStmt, got %T", s)
	}
	if fs.Binding != "i" {
		t.Errorf("binding: got %q, want %q", fs.Binding, "i")
	}
	rng := assertInfix(t, fs.Iterable, "..")
	assertIntLit(t, rng.Left, 0)
	assertIntLit(t, rng.Right, 10)
}

func TestParser_ForInclusive(t *testing.T) {
	s := firstStmt(t, `for i in 0..=10 { print(i) }`)
	fs := s.(*ast.ForStmt)
	rng := assertInfix(t, fs.Iterable, "..=")
	assertIntLit(t, rng.Left, 0)
	assertIntLit(t, rng.Right, 10)
}

// ── Literals ──────────────────────────────────────────────────────────────────

func TestParser_IntLiteral(t *testing.T) {
	assertIntLit(t, exprOf(t, firstStmt(t, `42`)), 42)
}

func TestParser_FloatLiteral(t *testing.T) {
	s := firstStmt(t, `3.14`)
	fl, ok := exprOf(t, s).(*ast.FloatLiteral)
	if !ok {
		t.Fatalf("expected *ast.FloatLiteral, got %T", exprOf(t, s))
	}
	if fl.Value != 3.14 {
		t.Errorf("float value: got %f, want 3.14", fl.Value)
	}
}

func TestParser_StringLiteral(t *testing.T) {
	s := firstStmt(t, `"hello"`)
	sl, ok := exprOf(t, s).(*ast.StringLiteral)
	if !ok {
		t.Fatalf("expected *ast.StringLiteral, got %T", exprOf(t, s))
	}
	if sl.Value != "hello" {
		t.Errorf("string value: got %q", sl.Value)
	}
}

func TestParser_BoolLiterals(t *testing.T) {
	for _, tc := range []struct {
		input string
		val   bool
	}{
		{"true", true},
		{"false", false},
	} {
		s := firstStmt(t, tc.input)
		bl, ok := exprOf(t, s).(*ast.BoolLiteral)
		if !ok {
			t.Fatalf("expected *ast.BoolLiteral, got %T", exprOf(t, s))
		}
		if bl.Value != tc.val {
			t.Errorf("bool value: got %v, want %v", bl.Value, tc.val)
		}
	}
}

func TestParser_Identifier(t *testing.T) {
	assertIdent(t, exprOf(t, firstStmt(t, `foo`)), "foo")
}

// ── Prefix expressions ────────────────────────────────────────────────────────

func TestParser_PrefixNot(t *testing.T) {
	s := firstStmt(t, `not x`)
	pe, ok := exprOf(t, s).(*ast.PrefixExpr)
	if !ok {
		t.Fatalf("expected *ast.PrefixExpr, got %T", exprOf(t, s))
	}
	if pe.Operator != "not" {
		t.Errorf("operator: got %q", pe.Operator)
	}
	assertIdent(t, pe.Right, "x")
}

func TestParser_PrefixMinus(t *testing.T) {
	s := firstStmt(t, `-5`)
	pe, ok := exprOf(t, s).(*ast.PrefixExpr)
	if !ok {
		t.Fatalf("expected *ast.PrefixExpr, got %T", exprOf(t, s))
	}
	if pe.Operator != "-" {
		t.Errorf("operator: got %q", pe.Operator)
	}
	assertIntLit(t, pe.Right, 5)
}

// ── Infix expressions and precedence ─────────────────────────────────────────

func TestParser_InfixAdd(t *testing.T) {
	inf := assertInfix(t, exprOf(t, firstStmt(t, `a + b`)), "+")
	assertIdent(t, inf.Left, "a")
	assertIdent(t, inf.Right, "b")
}

// TestParser_Precedence verifies that * binds tighter than +.
// `a + b * c` must parse as `a + (b * c)`, not `(a + b) * c`.
func TestParser_Precedence(t *testing.T) {
	inf := assertInfix(t, exprOf(t, firstStmt(t, `a + b * c`)), "+")
	assertIdent(t, inf.Left, "a")
	mul := assertInfix(t, inf.Right, "*")
	assertIdent(t, mul.Left, "b")
	assertIdent(t, mul.Right, "c")
}

// TestParser_PrecedenceComparison checks that comparison binds tighter than and.
func TestParser_PrecedenceLogical(t *testing.T) {
	// `a == b and c != d` → `(a == b) and (c != d)`
	inf := assertInfix(t, exprOf(t, firstStmt(t, `a == b and c != d`)), "and")
	assertInfix(t, inf.Left, "==")
	assertInfix(t, inf.Right, "!=")
}

func TestParser_StringConcat(t *testing.T) {
	inf := assertInfix(t, exprOf(t, firstStmt(t, `s1 ++ s2`)), "++")
	assertIdent(t, inf.Left, "s1")
	assertIdent(t, inf.Right, "s2")
}

// ── If expressions ────────────────────────────────────────────────────────────

func TestParser_IfOnly(t *testing.T) {
	s := firstStmt(t, `if x > 0 { print(x) }`)
	ie, ok := exprOf(t, s).(*ast.IfExpr)
	if !ok {
		t.Fatalf("expected *ast.IfExpr, got %T", exprOf(t, s))
	}
	assertInfix(t, ie.Condition, ">")
	if ie.Else != nil {
		t.Error("expected nil else branch")
	}
}

func TestParser_IfElse(t *testing.T) {
	s := firstStmt(t, `if x > 0 { x } else { 0 }`)
	ie := exprOf(t, s).(*ast.IfExpr)
	if ie.Else == nil {
		t.Fatal("expected else branch, got nil")
	}
	elseBlock, ok := ie.Else.(*ast.BlockExpr)
	if !ok {
		t.Fatalf("else: expected *ast.BlockExpr, got %T", ie.Else)
	}
	if len(elseBlock.Stmts) != 1 {
		t.Errorf("else stmts: got %d", len(elseBlock.Stmts))
	}
}

func TestParser_IfElseIf(t *testing.T) {
	input := `if a { 1 } else if b { 2 } else { 3 }`
	ie := exprOf(t, firstStmt(t, input)).(*ast.IfExpr)
	// else branch should be another IfExpr, not a BlockExpr
	elseIf, ok := ie.Else.(*ast.IfExpr)
	if !ok {
		t.Fatalf("else: expected *ast.IfExpr (else-if chain), got %T", ie.Else)
	}
	if _, ok := elseIf.Else.(*ast.BlockExpr); !ok {
		t.Fatalf("else-if else: expected *ast.BlockExpr, got %T", elseIf.Else)
	}
}

// ── Function call / field access ──────────────────────────────────────────────

func TestParser_CallExpression(t *testing.T) {
	s := firstStmt(t, `add(1, 2)`)
	ce, ok := exprOf(t, s).(*ast.CallExpr)
	if !ok {
		t.Fatalf("expected *ast.CallExpr, got %T", exprOf(t, s))
	}
	assertIdent(t, ce.Function, "add")
	if len(ce.Args) != 2 {
		t.Fatalf("args: got %d, want 2", len(ce.Args))
	}
	assertIntLit(t, ce.Args[0], 1)
	assertIntLit(t, ce.Args[1], 2)
}

func TestParser_CallNoArgs(t *testing.T) {
	ce := exprOf(t, firstStmt(t, `read_line()`)).(*ast.CallExpr)
	assertIdent(t, ce.Function, "read_line")
	if len(ce.Args) != 0 {
		t.Errorf("args: got %d, want 0", len(ce.Args))
	}
}

func TestParser_FieldAccess(t *testing.T) {
	fe := exprOf(t, firstStmt(t, `point.x`)).(*ast.FieldExpr)
	assertIdent(t, fe.Object, "point")
	if fe.Field != "x" {
		t.Errorf("field: got %q, want %q", fe.Field, "x")
	}
}

func TestParser_ChainedFieldAccess(t *testing.T) {
	// a.b.c → FieldExpr{ Object: FieldExpr{a, b}, Field: c }
	outer := exprOf(t, firstStmt(t, `a.b.c`)).(*ast.FieldExpr)
	if outer.Field != "c" {
		t.Errorf("outer field: got %q", outer.Field)
	}
	inner := outer.Object.(*ast.FieldExpr)
	assertIdent(t, inner.Object, "a")
	if inner.Field != "b" {
		t.Errorf("inner field: got %q", inner.Field)
	}
}

func TestParser_MethodCall(t *testing.T) {
	// point.distance_from_origin()
	ce := exprOf(t, firstStmt(t, `point.distance_from_origin()`)).(*ast.CallExpr)
	fe, ok := ce.Function.(*ast.FieldExpr)
	if !ok {
		t.Fatalf("expected FieldExpr as callee, got %T", ce.Function)
	}
	assertIdent(t, fe.Object, "point")
	if fe.Field != "distance_from_origin" {
		t.Errorf("method: got %q", fe.Field)
	}
}

// ── Match expression ──────────────────────────────────────────────────────────

func TestParser_MatchExpression(t *testing.T) {
	input := `match shape {
    Circle(r)       -> 3
    Rectangle(w, h) -> 4
    North           -> 5
}`
	me := exprOf(t, firstStmt(t, input)).(*ast.MatchExpr)
	assertIdent(t, me.Subject, "shape")
	if len(me.Arms) != 3 {
		t.Fatalf("arms: got %d, want 3", len(me.Arms))
	}
	// arm 0: Circle(r)
	arm0 := me.Arms[0]
	if arm0.Pattern.Variant != "Circle" {
		t.Errorf("arm0 variant: got %q", arm0.Pattern.Variant)
	}
	if len(arm0.Pattern.Bindings) != 1 || arm0.Pattern.Bindings[0] != "r" {
		t.Errorf("arm0 bindings: got %v", arm0.Pattern.Bindings)
	}
	// arm 1: Rectangle(w, h)
	arm1 := me.Arms[1]
	if len(arm1.Pattern.Bindings) != 2 {
		t.Errorf("arm1 bindings: got %v", arm1.Pattern.Bindings)
	}
	// arm 2: unit variant North
	arm2 := me.Arms[2]
	if arm2.Pattern.Variant != "North" {
		t.Errorf("arm2 variant: got %q", arm2.Pattern.Variant)
	}
	if len(arm2.Pattern.Bindings) != 0 {
		t.Errorf("arm2 bindings: expected empty, got %v", arm2.Pattern.Bindings)
	}
}

func TestParser_MatchWildcard(t *testing.T) {
	input := `match x { _ -> 0 }`
	me := exprOf(t, firstStmt(t, input)).(*ast.MatchExpr)
	if me.Arms[0].Pattern.Variant != "_" {
		t.Errorf("wildcard: got %q", me.Arms[0].Pattern.Variant)
	}
}

// ── Function literal (lambda) ─────────────────────────────────────────────────

func TestParser_FnLiteral(t *testing.T) {
	input := `let square = fn(x: Int) -> Int { x * x }`
	ls := firstStmt(t, input).(*ast.LetStmt)
	fe, ok := ls.Value.(*ast.FnExpr)
	if !ok {
		t.Fatalf("expected *ast.FnExpr, got %T", ls.Value)
	}
	if len(fe.Params) != 1 || fe.Params[0].Name != "x" {
		t.Errorf("params: %v", fe.Params)
	}
	if fe.ReturnType == nil || fe.ReturnType.Name != "Int" {
		t.Errorf("return type: %v", fe.ReturnType)
	}
}

// ── Function declaration ──────────────────────────────────────────────────────

func TestParser_FnDeclaration(t *testing.T) {
	input := `fn add(a: Int, b: Int) -> Int { a + b }`
	s := firstStmt(t, input)
	fd, ok := s.(*ast.FnDecl)
	if !ok {
		t.Fatalf("expected *ast.FnDecl, got %T", s)
	}
	if fd.Name != "add" {
		t.Errorf("name: got %q", fd.Name)
	}
	if len(fd.Params) != 2 {
		t.Fatalf("params: got %d", len(fd.Params))
	}
	if fd.Params[0].Name != "a" || fd.Params[0].Type.Name != "Int" {
		t.Errorf("param0: %+v", fd.Params[0])
	}
	if fd.Params[1].Name != "b" || fd.Params[1].Type.Name != "Int" {
		t.Errorf("param1: %+v", fd.Params[1])
	}
	if fd.ReturnType == nil || fd.ReturnType.Name != "Int" {
		t.Errorf("return type: %v", fd.ReturnType)
	}
	if len(fd.Body.Stmts) != 1 {
		t.Errorf("body stmts: got %d", len(fd.Body.Stmts))
	}
}

func TestParser_FnNoReturn(t *testing.T) {
	// fn with no explicit return type → ReturnType is nil (means Unit)
	fd := firstStmt(t, `fn main() { }`).(*ast.FnDecl)
	if fd.ReturnType != nil {
		t.Errorf("expected nil return type, got %v", fd.ReturnType)
	}
}

func TestParser_FnSelf(t *testing.T) {
	// self parameter doesn't have a type annotation
	input := `fn show(self) -> String { "ok" }`
	fd := firstStmt(t, input).(*ast.FnDecl)
	if len(fd.Params) != 1 {
		t.Fatalf("params: got %d", len(fd.Params))
	}
	if fd.Params[0].Name != "self" {
		t.Errorf("param name: got %q", fd.Params[0].Name)
	}
}

// ── Struct declaration ────────────────────────────────────────────────────────

func TestParser_StructDeclaration(t *testing.T) {
	input := `struct Point { x: Float  y: Float }`
	sd := firstStmt(t, input).(*ast.StructDecl)
	if sd.Name != "Point" {
		t.Errorf("name: got %q", sd.Name)
	}
	if len(sd.Fields) != 2 {
		t.Fatalf("fields: got %d", len(sd.Fields))
	}
	if sd.Fields[0].Name != "x" || sd.Fields[0].Type.Name != "Float" {
		t.Errorf("field0: %+v", sd.Fields[0])
	}
	if sd.Fields[1].Name != "y" || sd.Fields[1].Type.Name != "Float" {
		t.Errorf("field1: %+v", sd.Fields[1])
	}
}

// ── Enum declaration ──────────────────────────────────────────────────────────

func TestParser_EnumDeclaration(t *testing.T) {
	input := `enum Shape { Circle(Float)  Rectangle(Float, Float)  Triangle(Float, Float) }`
	ed := firstStmt(t, input).(*ast.EnumDecl)
	if ed.Name != "Shape" {
		t.Errorf("name: got %q", ed.Name)
	}
	if len(ed.Variants) != 3 {
		t.Fatalf("variants: got %d", len(ed.Variants))
	}
	if ed.Variants[0].Name != "Circle" || len(ed.Variants[0].Payloads) != 1 {
		t.Errorf("variant0: %+v", ed.Variants[0])
	}
	if ed.Variants[1].Name != "Rectangle" || len(ed.Variants[1].Payloads) != 2 {
		t.Errorf("variant1: %+v", ed.Variants[1])
	}
}

func TestParser_EnumUnitVariants(t *testing.T) {
	input := `enum Direction { North  South  East  West }`
	ed := firstStmt(t, input).(*ast.EnumDecl)
	if len(ed.Variants) != 4 {
		t.Fatalf("variants: got %d", len(ed.Variants))
	}
	for _, v := range ed.Variants {
		if len(v.Payloads) != 0 {
			t.Errorf("variant %q: expected no payloads", v.Name)
		}
	}
}

// ── Type alias ────────────────────────────────────────────────────────────────

func TestParser_TypeAlias(t *testing.T) {
	ta := firstStmt(t, `type UserId = Int`).(*ast.TypeAliasDecl)
	if ta.Name != "UserId" {
		t.Errorf("name: got %q", ta.Name)
	}
	if ta.Alias.Name != "Int" {
		t.Errorf("alias: got %q", ta.Alias.Name)
	}
}

// ── Impl declaration ──────────────────────────────────────────────────────────

func TestParser_ImplDeclaration(t *testing.T) {
	input := `impl Point {
    fn distance(self) -> Float { 0.0 }
}`
	id := firstStmt(t, input).(*ast.ImplDecl)
	if id.TypeName.Name != "Point" {
		t.Errorf("type: got %q", id.TypeName.Name)
	}
	if id.TraitName != nil {
		t.Error("expected nil trait name")
	}
	if len(id.Methods) != 1 {
		t.Errorf("methods: got %d", len(id.Methods))
	}
}

func TestParser_ImplForTrait(t *testing.T) {
	input := `impl Show for Point {
    fn show(self) -> String { "point" }
}`
	id := firstStmt(t, input).(*ast.ImplDecl)
	if id.TraitName == nil || id.TraitName.Name != "Show" {
		t.Errorf("trait: got %v", id.TraitName)
	}
	if id.TypeName.Name != "Point" {
		t.Errorf("type: got %q", id.TypeName.Name)
	}
}

// ── Trait declaration ─────────────────────────────────────────────────────────

func TestParser_TraitDeclaration(t *testing.T) {
	input := `trait Show { fn show(self) -> String }`
	td := firstStmt(t, input).(*ast.TraitDecl)
	if td.Name != "Show" {
		t.Errorf("name: got %q", td.Name)
	}
	if len(td.Methods) != 1 {
		t.Fatalf("methods: got %d", len(td.Methods))
	}
	if td.Methods[0].Name != "show" {
		t.Errorf("method name: got %q", td.Methods[0].Name)
	}
}

// ── Mod declaration ───────────────────────────────────────────────────────────

func TestParser_ModDeclaration(t *testing.T) {
	input := `mod math { pub fn sqrt(x: Float) -> Float { x } }`
	md := firstStmt(t, input).(*ast.ModDecl)
	if md.Name != "math" {
		t.Errorf("name: got %q", md.Name)
	}
	if len(md.Body) != 1 {
		t.Errorf("body: got %d statements", len(md.Body))
	}
}

// ── Use declaration ───────────────────────────────────────────────────────────

func TestParser_UseSingle(t *testing.T) {
	ud := firstStmt(t, `use math.sqrt`).(*ast.UseDecl)
	if fmt.Sprintf("%v", ud.Path) != "[math sqrt]" {
		t.Errorf("path: got %v", ud.Path)
	}
}

func TestParser_UseGlob(t *testing.T) {
	ud := firstStmt(t, `use math.*`).(*ast.UseDecl)
	if len(ud.Imports) != 1 || ud.Imports[0] != "*" {
		t.Errorf("imports: got %v", ud.Imports)
	}
}

func TestParser_UseGroup(t *testing.T) {
	ud := firstStmt(t, `use math.{sqrt, abs}`).(*ast.UseDecl)
	if len(ud.Imports) != 2 {
		t.Fatalf("imports: got %d", len(ud.Imports))
	}
	if ud.Imports[0] != "sqrt" || ud.Imports[1] != "abs" {
		t.Errorf("imports: got %v", ud.Imports)
	}
}

// ── Generic types ─────────────────────────────────────────────────────────────

func TestParser_GenericType(t *testing.T) {
	input := `fn f(x: Option<Int>) -> Result<Int, String> { x }`
	fd := firstStmt(t, input).(*ast.FnDecl)
	p0 := fd.Params[0].Type
	if p0.Name != "Option" || len(p0.Params) != 1 || p0.Params[0].Name != "Int" {
		t.Errorf("param type: %v", p0)
	}
	ret := fd.ReturnType
	if ret.Name != "Result" || len(ret.Params) != 2 {
		t.Errorf("return type: %v", ret)
	}
}

// ── Pub modifier ──────────────────────────────────────────────────────────────

func TestParser_PubFn(t *testing.T) {
	fd := firstStmt(t, `pub fn sqrt(x: Float) -> Float { x }`).(*ast.FnDecl)
	if !fd.IsPublic {
		t.Error("expected IsPublic=true")
	}
}

// ── Integration: FizzBuzz from SPEC.md §6 ────────────────────────────────────

func TestParser_Program_FizzBuzz(t *testing.T) {
	input := `
fn fizzbuzz(n: Int) -> String {
    if n % 15 == 0      { "FizzBuzz" }
    else if n % 3 == 0  { "Fizz" }
    else if n % 5 == 0  { "Buzz" }
    else                { int_to_string(n) }
}`
	prog := parse(t, input, 1)
	fd, ok := prog.Statements[0].(*ast.FnDecl)
	if !ok {
		t.Fatalf("expected *ast.FnDecl, got %T", prog.Statements[0])
	}
	if fd.Name != "fizzbuzz" {
		t.Errorf("name: got %q", fd.Name)
	}
	// body has one statement: the if/else chain expression
	if len(fd.Body.Stmts) != 1 {
		t.Errorf("body stmts: got %d", len(fd.Body.Stmts))
	}
	ie, ok := exprOf(t, fd.Body.Stmts[0]).(*ast.IfExpr)
	if !ok {
		t.Fatalf("body expr: expected *ast.IfExpr, got %T", exprOf(t, fd.Body.Stmts[0]))
	}
	// condition: n % 15 == 0
	cond := assertInfix(t, ie.Condition, "==")
	mod := assertInfix(t, cond.Left, "%")
	assertIdent(t, mod.Left, "n")
	assertIntLit(t, mod.Right, 15)
}

// ── Integration: factorial from SPEC.md §6 ───────────────────────────────────

func TestParser_Program_Factorial(t *testing.T) {
	input := `
fn factorial(n: Int) -> Int {
    if n <= 1 { 1 }
    else      { n * factorial(n - 1) }
}`
	fd := parse(t, input, 1).Statements[0].(*ast.FnDecl)
	if fd.Name != "factorial" {
		t.Errorf("name: got %q", fd.Name)
	}
	ie := exprOf(t, fd.Body.Stmts[0]).(*ast.IfExpr)
	cond := assertInfix(t, ie.Condition, "<=")
	assertIdent(t, cond.Left, "n")
	assertIntLit(t, cond.Right, 1)
	// else branch: n * factorial(n - 1)
	elseBlock := ie.Else.(*ast.BlockExpr)
	mul := assertInfix(t, exprOf(t, elseBlock.Stmts[0]), "*")
	assertIdent(t, mul.Left, "n")
	call, ok := mul.Right.(*ast.CallExpr)
	if !ok {
		t.Fatalf("rhs: expected *ast.CallExpr, got %T", mul.Right)
	}
	assertIdent(t, call.Function, "factorial")
}

// ── Integration: main loop from SPEC.md §6 ───────────────────────────────────

func TestParser_Program_Main(t *testing.T) {
	input := `
fn main() {
    mut i = 1
    while i <= 100 {
        print(fizzbuzz(i))
        i = i + 1
    }
    print(int_to_string(factorial(10)))
}`
	fd := parse(t, input, 1).Statements[0].(*ast.FnDecl)
	if fd.Name != "main" {
		t.Errorf("name: got %q", fd.Name)
	}
	if len(fd.Body.Stmts) != 3 {
		t.Errorf("body stmts: got %d, want 3 (mut, while, print)", len(fd.Body.Stmts))
	}
	// stmt 0: mut i = 1
	ls := fd.Body.Stmts[0].(*ast.LetStmt)
	if !ls.Mutable || ls.Name != "i" {
		t.Errorf("let stmt: %+v", ls)
	}
	// stmt 1: while
	if _, ok := fd.Body.Stmts[1].(*ast.WhileStmt); !ok {
		t.Errorf("stmt1: expected *ast.WhileStmt, got %T", fd.Body.Stmts[1])
	}
	// stmt 2: print(...)
	ce := exprOf(t, fd.Body.Stmts[2]).(*ast.CallExpr)
	assertIdent(t, ce.Function, "print")
}
