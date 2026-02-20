# TASK.md — Ren Compiler Implementation

**Status:** Not started  
**Language:** Go  
**Spec:** See `SPEC.md` (v0.1)

---

## Goal

Build a working compiler for the Ren programming language (v0.1) in Go.
The compiler should take a `.ren` source file and produce executable output.

---

## Suggested Architecture

```
ren-lang/
├── SPEC.md             ← language spec (do not modify without versioning)
├── TASK.md             ← this file
├── go.mod
├── main.go             ← entry point: `ren <file.ren>`
├── lexer/
│   └── lexer.go        ← tokenizer
├── parser/
│   └── parser.go       ← AST builder
├── ast/
│   └── ast.go          ← AST node definitions
├── typechecker/
│   └── typechecker.go  ← static type checking
├── codegen/
│   └── codegen.go      ← code generation (see target below)
└── examples/
    ├── fizzbuzz.ren
    └── factorial.ren
```

---

## Implementation Phases

### Phase 1 — Lexer
Tokenize `.ren` source into a flat token stream.

Tokens to handle:
- Keywords: `let`, `mut`, `fn`, `return`, `if`, `else`, `match`, `while`, `for`, `in`,
  `break`, `continue`, `struct`, `enum`, `type`, `impl`, `trait`, `mod`, `use`, `pub`,
  `and`, `or`, `not`, `true`, `false`, `Self`, `self`
- Literals: integer, float, string (double-quoted, UTF-8)
- Identifiers
- Operators: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `++`, `..`, `..=`, `->`, `=`
- Delimiters: `{`, `}`, `(`, `)`, `[`, `]`, `,`, `:`, `.`, `*` (glob in `use`)
- Comments: `//` line comments (ignored)

### Phase 2 — Parser → AST
Recursive descent parser producing a typed AST.

Key constructs:
- Top-level: `fn`, `struct`, `enum`, `type`, `trait`, `impl`, `mod`, `use`
- Expressions: literals, identifiers, binary ops, unary ops, function calls, field access,
  `if`/`else`, `match`, block expressions
- Statements: `let`/`mut` bindings, assignments, `return`, `break`, `continue`, `while`, `for`

### Phase 3 — Type Checker
Walk the AST and enforce:
- All bindings have resolvable types (explicit or inferred)
- `if`/`else` branches produce the same type
- `match` is exhaustive over `enum` variants
- No mutation of non-`mut` bindings
- `Option<T>` and `Result<T, E>` are built-in — no null allowed
- Function call signatures match

### Phase 4 — Code Generation
**Target: Go source output** (transpile Ren → Go, then invoke `go build`)

Rationale: Go is the implementation language; generating Go avoids needing a full runtime
and leverages Go's GC, stdlib, and toolchain.

Mapping:
| Ren                  | Go                            |
|----------------------|-------------------------------|
| `let x = 42`         | `x := 42`                     |
| `mut count = 0`      | `count := 0`                  |
| `fn f(a: Int) -> Int`| `func f(a int64) int64`       |
| `struct Point`       | `type Point struct`           |
| `enum Shape`         | `type Shape interface{}` + tagged structs |
| `Option<T>`          | pointer or wrapper struct     |
| `Result<T,E>`        | `(T, error)` or wrapper struct|
| `match`              | `switch` with type assertions |
| `print(...)`         | `fmt.Println(...)`            |
| `panic(...)`         | `panic(...)`                  |
| `and`/`or`/`not`     | `&&`/`\|\|`/`!`               |

### Phase 5 — CLI & Examples
- `main.go`: parses args, drives pipeline, invokes `go build` on generated output
- Write `examples/fizzbuzz.ren` and `examples/factorial.ren` (both in SPEC.md §6)
- End-to-end test: `ren examples/fizzbuzz.ren` produces correct output

---

## Key Constraints (from SPEC.md)

- Immutable by default — `mut` is explicit
- No null — `Option<T>` only
- Errors are values — `Result<T, E>` only, no exceptions
- `if` is always an expression (both branches must type-check)
- `match` must be exhaustive
- No operator overloading
- No implicit type conversions

---

## Open Questions (deferred — see SPEC.md §9)

- Generics beyond `Option<T>` / `Result<T, E>`
- Ownership/memory model (GC via Go transpile covers v0.1)
- Concurrency
- String interpolation
- Multi-file module resolution
- Integer overflow behavior

---

## How to Pick This Up

1. Read `SPEC.md` in full
2. Initialize Go module: `go mod init github.com/metaphox/ren-lang`
3. Implement phases in order: Lexer → Parser → TypeChecker → CodeGen → CLI
4. Test with `examples/fizzbuzz.ren` and `examples/factorial.ren`
5. Update this file as phases complete

---

*Created by Lucy (session 2026-02-20). Tao confirmed Go as implementation language.*
