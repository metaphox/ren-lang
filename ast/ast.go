// Package ast defines the Abstract Syntax Tree (AST) node types for the Ren language.
//
// Every source construct has a corresponding node type. The hierarchy is:
//
//	Node (interface)
//	  Statement (interface)
//	    Declaration (interface) — top-level items
//	      FnDecl, StructDecl, EnumDecl, TypeAliasDecl
//	      ImplDecl, TraitDecl, ModDecl, UseDecl
//	    LetStmt, ReturnStmt, BreakStmt, ContinueStmt
//	    WhileStmt, ForStmt, AssignStmt, ExprStmt
//	  Expression (interface)
//	    Identifier, IntLiteral, FloatLiteral, StringLiteral, BoolLiteral
//	    PrefixExpr, InfixExpr, BlockExpr
//	    IfExpr, MatchExpr, FnExpr, CallExpr, FieldExpr
//
// Positional information (line + column) is stored on the Token field present
// in every node. Callers should use node.Pos() to obtain position.
package ast

import "fmt"

// ── Interfaces ────────────────────────────────────────────────────────────────

// Node is the root interface for every element in the Ren AST.
// Every node carries the token at which it starts (for error reporting).
type Node interface {
	// TokenLiteral returns the literal string of the token that began this node.
	TokenLiteral() string
	// String returns a compact, human-readable representation of the node.
	// It is intended for debugging and test output, not pretty-printing.
	String() string
}

// Statement is a Node that produces no value when executed in isolation.
// Declarations and control-flow constructs are statements.
type Statement interface {
	Node
	statementNode()
}

// Expression is a Node that evaluates to a value.
// Ren is expression-oriented: if, match, and block `{}` are all expressions.
type Expression interface {
	Node
	expressionNode()
}

// Declaration is a Statement that introduces a named entity at module scope.
// All declarations are also statements so they may appear inside mod blocks.
type Declaration interface {
	Statement
	declarationNode()
}

// ── Top-level program ─────────────────────────────────────────────────────────

// Program is the root AST node produced by the parser.
// A Ren source file is a flat list of top-level statements; declarations are
// the most common, but any statement is syntactically legal at the top level.
type Program struct {
	Statements []Statement
}

// TokenLiteral returns the literal of the first statement's starting token,
// or "" for an empty program.
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// String returns all statements concatenated, useful for snapshot testing.
func (p *Program) String() string {
	out := ""
	for _, s := range p.Statements {
		out += s.String() + "\n"
	}
	return out
}

// ── Support types ─────────────────────────────────────────────────────────────

// Param represents a single function parameter: name: Type.
type Param struct {
	Name  string // parameter identifier
	Type  *TypeExpr
	Token Token // the identifier token
}

// FieldDef represents one field in a struct: name: Type.
type FieldDef struct {
	Name  string
	Type  *TypeExpr
	Token Token
}

// EnumVariant represents one variant of an enum.
// Payloads holds the types carried by a constructor variant (may be empty).
//
//	Circle(Float)       → Name="Circle", Payloads=[Float]
//	North               → Name="North",  Payloads=[]
type EnumVariant struct {
	Name     string
	Payloads []*TypeExpr
	Token    Token
}

// TraitMethod represents a method signature inside a trait body.
// Trait methods in v0.1 have no default implementations.
type TraitMethod struct {
	Name       string
	Params     []Param
	ReturnType *TypeExpr // nil means Unit
	Token      Token
}

// MatchArm is one arm of a match expression: Pattern -> Expression.
type MatchArm struct {
	Pattern Pattern
	Body    Expression
	Token   Token // the token at the start of the pattern
}

// Pattern is the left-hand side of a match arm.
// Ren v0.1 supports three pattern forms:
//   - Wildcard  ("_")                → Variant="_",   Bindings=nil
//   - Unit variant ("North")         → Variant="North", Bindings=nil
//   - Constructor ("Circle(r)")      → Variant="Circle", Bindings=["r"]
type Pattern struct {
	Variant  string   // enum variant name, or "_" for wildcard
	Bindings []string // bound variable names (empty for unit/wildcard)
	Token    Token
}

// TypeExpr represents a type annotation in the source, e.g. Int, Option<T>.
// For function types (fn(Int) -> Bool), IsFn is true and FnParams/FnReturn
// are populated; Name and Params are unused.
type TypeExpr struct {
	Name     string      // "Int", "String", "Option", etc.
	Params   []*TypeExpr // generic parameters (e.g. [T] in Option<T>)
	IsFn     bool        // true for function type annotations
	FnParams []*TypeExpr // parameter types for function types
	FnReturn *TypeExpr   // return type for function types (nil = Unit)
	Token    Token
}

// String returns a compact representation of the type, e.g. "Option<Int>".
func (te *TypeExpr) String() string {
	if te == nil {
		return "Unit"
	}
	if te.IsFn {
		ret := "Unit"
		if te.FnReturn != nil {
			ret = te.FnReturn.String()
		}
		params := ""
		for i, p := range te.FnParams {
			if i > 0 {
				params += ", "
			}
			params += p.String()
		}
		return fmt.Sprintf("fn(%s) -> %s", params, ret)
	}
	if len(te.Params) == 0 {
		return te.Name
	}
	params := ""
	for i, p := range te.Params {
		if i > 0 {
			params += ", "
		}
		params += p.String()
	}
	return fmt.Sprintf("%s<%s>", te.Name, params)
}

// ── Declarations ──────────────────────────────────────────────────────────────

// FnDecl is a named function declaration at module or impl scope.
//
//	fn add(a: Int, b: Int) -> Int { a + b }
type FnDecl struct {
	Token      Token     // the 'fn' token
	Name       string    // function name
	Params     []Param   // parameter list
	ReturnType *TypeExpr // nil means Unit
	Body       *BlockExpr
	IsPublic   bool // true when prefixed with 'pub'
}

func (d *FnDecl) statementNode()   {}
func (d *FnDecl) declarationNode() {}
func (d *FnDecl) TokenLiteral() string { return d.Token.Literal }
func (d *FnDecl) String() string {
	return fmt.Sprintf("fn %s(...) -> %s %s", d.Name, d.ReturnType.String(), d.Body.String())
}

// StructDecl introduces a product type.
//
//	struct Point { x: Float  y: Float }
type StructDecl struct {
	Token    Token // the 'struct' token
	Name     string
	Fields   []FieldDef
	IsPublic bool
}

func (d *StructDecl) statementNode()   {}
func (d *StructDecl) declarationNode() {}
func (d *StructDecl) TokenLiteral() string { return d.Token.Literal }
func (d *StructDecl) String() string       { return fmt.Sprintf("struct %s { ... }", d.Name) }

// EnumDecl introduces a sum type (algebraic data type).
//
//	enum Shape { Circle(Float)  Rectangle(Float, Float) }
type EnumDecl struct {
	Token    Token // the 'enum' token
	Name     string
	Variants []EnumVariant
	IsPublic bool
}

func (d *EnumDecl) statementNode()   {}
func (d *EnumDecl) declarationNode() {}
func (d *EnumDecl) TokenLiteral() string { return d.Token.Literal }
func (d *EnumDecl) String() string       { return fmt.Sprintf("enum %s { ... }", d.Name) }

// TypeAliasDecl creates a type alias.
//
//	type UserId = Int
type TypeAliasDecl struct {
	Token    Token // the 'type' token
	Name     string
	Alias    *TypeExpr
	IsPublic bool
}

func (d *TypeAliasDecl) statementNode()   {}
func (d *TypeAliasDecl) declarationNode() {}
func (d *TypeAliasDecl) TokenLiteral() string { return d.Token.Literal }
func (d *TypeAliasDecl) String() string {
	return fmt.Sprintf("type %s = %s", d.Name, d.Alias.String())
}

// ImplDecl attaches method implementations to a type.
//
//	impl Point { fn distance_from_origin(self) -> Float { ... } }
//	impl Show for Point { fn show(self) -> String { ... } }
type ImplDecl struct {
	Token     Token // the 'impl' token
	TypeName  *TypeExpr
	TraitName *TypeExpr // non-nil when 'impl Trait for Type'
	Methods   []*FnDecl
}

func (d *ImplDecl) statementNode()   {}
func (d *ImplDecl) declarationNode() {}
func (d *ImplDecl) TokenLiteral() string { return d.Token.Literal }
func (d *ImplDecl) String() string {
	if d.TraitName != nil {
		return fmt.Sprintf("impl %s for %s { ... }", d.TraitName.String(), d.TypeName.String())
	}
	return fmt.Sprintf("impl %s { ... }", d.TypeName.String())
}

// TraitDecl defines a named interface.
//
//	trait Show { fn show(self) -> String }
type TraitDecl struct {
	Token    Token // the 'trait' token
	Name     string
	Methods  []TraitMethod
	IsPublic bool
}

func (d *TraitDecl) statementNode()   {}
func (d *TraitDecl) declarationNode() {}
func (d *TraitDecl) TokenLiteral() string { return d.Token.Literal }
func (d *TraitDecl) String() string       { return fmt.Sprintf("trait %s { ... }", d.Name) }

// ModDecl defines a named module namespace.
//
//	mod math { pub fn sqrt(x: Float) -> Float { ... } }
type ModDecl struct {
	Token    Token // the 'mod' token
	Name     string
	Body     []Statement
	IsPublic bool
}

func (d *ModDecl) statementNode()   {}
func (d *ModDecl) declarationNode() {}
func (d *ModDecl) TokenLiteral() string { return d.Token.Literal }
func (d *ModDecl) String() string       { return fmt.Sprintf("mod %s { ... }", d.Name) }

// UseDecl brings names from a module into the current scope.
//
//	use math.sqrt
//	use math.{sqrt, abs}
//	use math.*
type UseDecl struct {
	Token   Token    // the 'use' token
	Path    []string // e.g. ["math", "sqrt"]
	Imports []string // empty = import the final segment; "*" = glob; else named list
}

func (d *UseDecl) statementNode()   {}
func (d *UseDecl) declarationNode() {}
func (d *UseDecl) TokenLiteral() string { return d.Token.Literal }
func (d *UseDecl) String() string {
	path := ""
	for i, p := range d.Path {
		if i > 0 {
			path += "."
		}
		path += p
	}
	return fmt.Sprintf("use %s", path)
}

// ── Statements ────────────────────────────────────────────────────────────────

// LetStmt declares a binding.
//
//	let x = 42          → Mutable=false
//	mut count = 0       → Mutable=true
//	let name: String = "Tao"  → with explicit type
type LetStmt struct {
	Token    Token     // 'let' or 'mut'
	Name     string    // binding name
	Type     *TypeExpr // optional explicit type annotation (nil = inferred)
	Value    Expression
	Mutable  bool
}

func (s *LetStmt) statementNode()    {}
func (s *LetStmt) TokenLiteral() string { return s.Token.Literal }
func (s *LetStmt) String() string {
	kw := "let"
	if s.Mutable {
		kw = "mut"
	}
	return fmt.Sprintf("%s %s = %s", kw, s.Name, s.Value.String())
}

// AssignStmt assigns a new value to an existing mutable binding or field.
//
//	count = count + 1
//	self.x = dx
type AssignStmt struct {
	Token  Token      // the '=' token
	Target Expression // Identifier or FieldExpr
	Value  Expression
}

func (s *AssignStmt) statementNode()    {}
func (s *AssignStmt) TokenLiteral() string { return s.Token.Literal }
func (s *AssignStmt) String() string {
	return fmt.Sprintf("%s = %s", s.Target.String(), s.Value.String())
}

// ReturnStmt performs an early return from a function.
//
//	return None
//	return        (returns Unit)
type ReturnStmt struct {
	Token Token
	Value Expression // nil when returning Unit
}

func (s *ReturnStmt) statementNode()    {}
func (s *ReturnStmt) TokenLiteral() string { return s.Token.Literal }
func (s *ReturnStmt) String() string {
	if s.Value == nil {
		return "return"
	}
	return fmt.Sprintf("return %s", s.Value.String())
}

// BreakStmt exits the nearest enclosing loop.
type BreakStmt struct {
	Token Token
}

func (s *BreakStmt) statementNode()    {}
func (s *BreakStmt) TokenLiteral() string { return s.Token.Literal }
func (s *BreakStmt) String() string       { return "break" }

// ContinueStmt skips to the next loop iteration.
type ContinueStmt struct {
	Token Token
}

func (s *ContinueStmt) statementNode()    {}
func (s *ContinueStmt) TokenLiteral() string { return s.Token.Literal }
func (s *ContinueStmt) String() string       { return "continue" }

// WhileStmt is a conditional loop.
//
//	while i < 10 { print(i); i = i + 1 }
type WhileStmt struct {
	Token     Token // the 'while' token
	Condition Expression
	Body      *BlockExpr
}

func (s *WhileStmt) statementNode()    {}
func (s *WhileStmt) TokenLiteral() string { return s.Token.Literal }
func (s *WhileStmt) String() string {
	return fmt.Sprintf("while %s %s", s.Condition.String(), s.Body.String())
}

// ForStmt is an iterator loop.
//
//	for item in list { print(item) }
//	for i in 0..10  { print(i) }
type ForStmt struct {
	Token    Token // the 'for' token
	Binding  string
	Iterable Expression
	Body     *BlockExpr
}

func (s *ForStmt) statementNode()    {}
func (s *ForStmt) TokenLiteral() string { return s.Token.Literal }
func (s *ForStmt) String() string {
	return fmt.Sprintf("for %s in %s %s", s.Binding, s.Iterable.String(), s.Body.String())
}

// ExprStmt wraps an expression that appears in statement position.
// The last ExprStmt in a block is the block's implicit return value.
type ExprStmt struct {
	Token Token      // the first token of the expression
	Expr  Expression
}

func (s *ExprStmt) statementNode()    {}
func (s *ExprStmt) TokenLiteral() string { return s.Token.Literal }
func (s *ExprStmt) String() string       { return s.Expr.String() }

// ── Expressions ───────────────────────────────────────────────────────────────

// Identifier is a reference to a named binding, function, or type.
type Identifier struct {
	Token Token
	Name  string
}

func (e *Identifier) expressionNode()  {}
func (e *Identifier) TokenLiteral() string { return e.Token.Literal }
func (e *Identifier) String() string       { return e.Name }

// IntLiteral is a decimal integer literal value.
type IntLiteral struct {
	Token Token
	Value int64
}

func (e *IntLiteral) expressionNode()  {}
func (e *IntLiteral) TokenLiteral() string { return e.Token.Literal }
func (e *IntLiteral) String() string       { return e.Token.Literal }

// FloatLiteral is a 64-bit IEEE 754 floating-point literal.
type FloatLiteral struct {
	Token Token
	Value float64
}

func (e *FloatLiteral) expressionNode()  {}
func (e *FloatLiteral) TokenLiteral() string { return e.Token.Literal }
func (e *FloatLiteral) String() string       { return e.Token.Literal }

// StringLiteral is an evaluated string literal (escape sequences already processed).
type StringLiteral struct {
	Token Token
	Value string
}

func (e *StringLiteral) expressionNode()  {}
func (e *StringLiteral) TokenLiteral() string { return e.Token.Literal }
func (e *StringLiteral) String() string       { return fmt.Sprintf("%q", e.Value) }

// BoolLiteral is the boolean literal true or false.
type BoolLiteral struct {
	Token Token
	Value bool
}

func (e *BoolLiteral) expressionNode()  {}
func (e *BoolLiteral) TokenLiteral() string { return e.Token.Literal }
func (e *BoolLiteral) String() string       { return e.Token.Literal }

// PrefixExpr is a unary prefix expression: not x  or  -5.
// Operator is the literal string of the operator token ("not" or "-").
type PrefixExpr struct {
	Token    Token  // the operator token
	Operator string // "not" or "-"
	Right    Expression
}

func (e *PrefixExpr) expressionNode()  {}
func (e *PrefixExpr) TokenLiteral() string { return e.Token.Literal }
func (e *PrefixExpr) String() string {
	return fmt.Sprintf("(%s %s)", e.Operator, e.Right.String())
}

// InfixExpr is a binary infix expression: left op right.
// Operator is the literal string of the operator token.
type InfixExpr struct {
	Token    Token // the operator token
	Left     Expression
	Operator string // "+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "++", "..", "..=", "and", "or"
	Right    Expression
}

func (e *InfixExpr) expressionNode()  {}
func (e *InfixExpr) TokenLiteral() string { return e.Token.Literal }
func (e *InfixExpr) String() string {
	return fmt.Sprintf("(%s %s %s)", e.Left.String(), e.Operator, e.Right.String())
}

// BlockExpr is a brace-delimited sequence of statements.
// The value of the block is the value of its final ExprStmt, or Unit if the
// last statement is not an ExprStmt (or the block is empty).
//
//	{ let x = 5; x + 1 }
type BlockExpr struct {
	Token Token // the '{' token
	Stmts []Statement
}

func (e *BlockExpr) expressionNode()  {}
func (e *BlockExpr) TokenLiteral() string { return e.Token.Literal }
func (e *BlockExpr) String() string {
	out := "{ "
	for _, s := range e.Stmts {
		out += s.String() + "; "
	}
	return out + "}"
}

// IfExpr is a conditional expression. Both branches must produce the same type.
// ElseBlock is nil when there is no else branch (result is Unit in that case).
//
//	if x > 0 { x } else { -x }
//	if b == 0 { return None }
type IfExpr struct {
	Token     Token // the 'if' token
	Condition Expression
	Then      *BlockExpr
	Else      Expression // *BlockExpr or *IfExpr (else if chain), or nil
}

func (e *IfExpr) expressionNode()  {}
func (e *IfExpr) TokenLiteral() string { return e.Token.Literal }
func (e *IfExpr) String() string {
	out := fmt.Sprintf("if %s %s", e.Condition.String(), e.Then.String())
	if e.Else != nil {
		out += " else " + e.Else.String()
	}
	return out
}

// MatchExpr is an exhaustive pattern-matching expression.
//
//	match shape {
//	    Circle(r)       -> 3.14159 * r * r
//	    Rectangle(w, h) -> w * h
//	}
type MatchExpr struct {
	Token   Token // the 'match' token
	Subject Expression
	Arms    []MatchArm
}

func (e *MatchExpr) expressionNode()  {}
func (e *MatchExpr) TokenLiteral() string { return e.Token.Literal }
func (e *MatchExpr) String() string {
	return fmt.Sprintf("match %s { ... }", e.Subject.String())
}

// FnExpr is an anonymous function (lambda) expression.
// The syntax is identical to a named FnDecl without the name.
//
//	let square = fn(x: Int) -> Int { x * x }
type FnExpr struct {
	Token      Token // the 'fn' token
	Params     []Param
	ReturnType *TypeExpr // nil means Unit
	Body       *BlockExpr
}

func (e *FnExpr) expressionNode()  {}
func (e *FnExpr) TokenLiteral() string { return e.Token.Literal }
func (e *FnExpr) String() string {
	return fmt.Sprintf("fn(...) -> %s %s", e.ReturnType.String(), e.Body.String())
}

// CallExpr is a function or method call.
//
//	add(1, 2)
//	int_to_string(n)
type CallExpr struct {
	Token    Token // the '(' token
	Function Expression
	Args     []Expression
}

func (e *CallExpr) expressionNode()  {}
func (e *CallExpr) TokenLiteral() string { return e.Token.Literal }
func (e *CallExpr) String() string {
	args := ""
	for i, a := range e.Args {
		if i > 0 {
			args += ", "
		}
		args += a.String()
	}
	return fmt.Sprintf("%s(%s)", e.Function.String(), args)
}

// FieldExpr is a field access on a struct or enum value.
//
//	point.x
//	self.radius
type FieldExpr struct {
	Token  Token // the '.' token
	Object Expression
	Field  string
}

func (e *FieldExpr) expressionNode()  {}
func (e *FieldExpr) TokenLiteral() string { return e.Token.Literal }
func (e *FieldExpr) String() string {
	return fmt.Sprintf("%s.%s", e.Object.String(), e.Field)
}
