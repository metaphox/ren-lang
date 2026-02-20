# Ren Language Specification

**Version:** 0.1 (initial design)
**File extension:** `.ren`
**Status:** Pre-compiler, design phase

---

## 1. Overview

Ren (仁) is a statically typed, expression-oriented programming language with three non-negotiable commitments:

1. **Immutable by default** — mutability is explicit and opt-in
2. **No null** — absence is represented by `Option<T>`
3. **Errors are values** — failure is represented by `Result<T, E>`, not exceptions

The name is the Chinese character for benevolence. The language tries to be kind to the programmer by eliminating entire categories of runtime errors at the design level.

---

## 2. Type System

- **Static typing** with local type inference
- Types must be written at function signatures; the compiler infers them in expression bodies
- **Algebraic data types** via `struct` (product types) and `enum` (sum types)
- Built-in parameterized types: `Option<T>` and `Result<T, E>`
- No subtype inheritance — composition via `trait` + `impl`
- `Unit` (the empty tuple `()`) is the return type for functions with no meaningful value

### Primitive types

| Type     | Description                        |
|----------|------------------------------------|
| `Int`    | 64-bit signed integer              |
| `Float`  | 64-bit IEEE 754 double             |
| `Bool`   | `true` or `false`                  |
| `String` | UTF-8 immutable string             |
| `Unit`   | Empty type, written `()`           |

---

## 3. Keywords

### 3.1 Bindings

#### `let`
Declares an immutable binding. Immutability is the default; mutation must be explicit.

```ren
let x = 42
let name: String = "Tao"
```

Type annotation is optional when the type can be inferred.

#### `mut`
Modifier that makes a binding mutable. Syntactically adjacent to `let` at the declaration site, making mutability visually loud exactly where it matters.

```ren
mut count = 0
count = count + 1
```

---

### 3.2 Functions

#### `fn`
Declares a function. The last expression in the body is the implicit return value. Functions are first-class values.

```ren
fn add(a: Int, b: Int) -> Int {
    a + b
}

let square = fn(x: Int) -> Int { x * x }
```

`->` separates the parameter list from the return type. Lambda syntax is identical to named function syntax minus the name.

#### `return`
Explicit early return. Not needed for the final expression — only for breaking out of a function before reaching the end.

```ren
fn safe_divide(a: Int, b: Int) -> Option<Int> {
    if b == 0 { return None }
    Some(a / b)
}
```

---

### 3.3 Control Flow

#### `if` / `else`
Conditional expression. Always an expression — it always produces a value. Braces are mandatory. This eliminates the need for a ternary operator.

```ren
let label = if score > 90 { "A" } else { "B" }

if x > 0 {
    print("positive")
} else if x < 0 {
    print("negative")
} else {
    print("zero")
}
```

Both branches must produce the same type. An `if` without `else` produces `Unit`.

#### `match`
Pattern matching over a value. Exhaustive — the compiler errors if any case is unhandled. Each arm uses `->` to produce a value. `match` is an expression.

```ren
match shape {
    Circle(r)       -> 3.14159 * r * r
    Rectangle(w, h) -> w * h
    Triangle(b, h)  -> 0.5 * b * h
}
```

Match arms are evaluated in order. The compiler verifies all variants of an `enum` are covered.

#### `while`
Conditional loop. Executes the body as long as the condition is `true`. Produces `Unit`.

```ren
mut i = 0
while i < 10 {
    print(i)
    i = i + 1
}
```

#### `for` / `in`
Iterator loop over any value implementing the `Iterable` trait. No C-style `for(init; cond; step)` — that form encourages off-by-one errors and mixes three unrelated concerns in one line.

```ren
for item in list {
    print(item)
}

for i in 0..10 {
    print(i)
}
```

`0..10` is an exclusive range (0 to 9). `0..=10` is inclusive.

#### `break`
Exits the nearest enclosing `while` or `for` loop immediately.

```ren
while true {
    let input = read_line()
    if input == "quit" { break }
    process(input)
}
```

#### `continue`
Skips the remainder of the current loop iteration and proceeds to the next.

```ren
for n in 0..100 {
    if n % 2 == 0 { continue }
    print(n)
}
```

---

### 3.4 Data Types

#### `struct`
Product type — a named record with typed fields. Value semantics. No inheritance.

```ren
struct Point {
    x: Float
    y: Float
}

let p = Point { x: 1.0, y: 2.0 }
let px = p.x
```

#### `enum`
Sum type (algebraic data type). Each variant may carry zero or more typed values. The primary tool for representing alternatives without null.

```ren
enum Shape {
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float)
}

enum Direction {
    North
    South
    East
    West
}
```

Variants without payloads are unit variants (no parentheses).

#### Built-in generic enums

```ren
enum Option<T> {
    Some(T)
    None
}

enum Result<T, E> {
    Ok(T)
    Err(E)
}
```

These are part of the language core. There is no null type.

#### `type`
Type alias — gives a semantic name to an existing type. The type system treats the alias and the original as equivalent.

```ren
type UserId   = Int
type Email    = String
type Callback = fn(Int) -> Bool
```

---

### 3.5 Behavior

#### `impl`
Attaches methods to a type. Data definition (`struct`/`enum`) is separate from behavior (`impl`). `self` refers to the receiver instance.

```ren
impl Point {
    fn distance_from_origin(self) -> Float {
        sqrt(self.x * self.x + self.y * self.y)
    }

    fn translate(self, dx: Float, dy: Float) -> Point {
        Point { x: self.x + dx, y: self.y + dy }
    }
}
```

Methods return new values by default (immutable). To mutate `self`, it must be declared `mut self`.

#### `trait`
Defines a named interface — a set of method signatures a type must implement. No default implementations in v0.1. Trait satisfaction is explicit via `impl <Trait> for <Type>`.

```ren
trait Show {
    fn show(self) -> String
}

trait Eq {
    fn eq(self, other: Self) -> Bool
}

impl Show for Point {
    fn show(self) -> String {
        "(" ++ float_to_string(self.x) ++ ", " ++ float_to_string(self.y) ++ ")"
    }
}
```

---

### 3.6 Modules

#### `mod`
Defines a named module — a namespace for types, functions, and constants. All items are private by default.

```ren
mod math {
    pub fn sqrt(x: Float) -> Float { ... }
    pub fn abs(x: Float) -> Float { ... }

    fn internal_helper(x: Float) -> Float { ... }
}
```

#### `use`
Brings names from a module into the current scope.

```ren
use math.sqrt
use math.{sqrt, abs}
use math.*
```

#### `pub`
Visibility modifier. Marks an item as accessible outside its module. Default is private.

---

### 3.7 Logical Operators

| Keyword | Meaning       | Replaces |
|---------|---------------|----------|
| `and`   | Logical AND   | `&&`     |
| `or`    | Logical OR    | `\|\|`   |
| `not`   | Logical NOT   | `!`      |

Words are used instead of symbols to eliminate precedence ambiguity and improve readability.

```ren
if age >= 18 and not suspended {
    admit()
}
```

### 3.8 Boolean Literals

`true` and `false` — type `Bool`.

---

## 4. Operators

| Operator | Types         | Meaning              |
|----------|---------------|----------------------|
| `+`      | Int, Float    | Addition             |
| `-`      | Int, Float    | Subtraction / unary negation |
| `*`      | Int, Float    | Multiplication       |
| `/`      | Int, Float    | Division             |
| `%`      | Int           | Remainder            |
| `==`     | Any (Eq)      | Equality             |
| `!=`     | Any (Eq)      | Inequality           |
| `<` `>` `<=` `>=` | Ord types | Comparison    |
| `++`     | String        | String concatenation |
| `..`     | Int           | Exclusive range      |
| `..=`    | Int           | Inclusive range      |

No operator overloading in v0.1.

---

## 5. Notable Omissions

| Absent               | Rationale                                              |
|----------------------|--------------------------------------------------------|
| `null` / `nil`       | Replaced by `Option<T>`. Null references are a design error. |
| `class`              | `struct` + `impl` + `trait` covers everything without inheritance complexity. |
| `try` / `catch`      | Replaced by `Result<T, E>` + `match`. Exceptions make control flow invisible. |
| `void`               | Replaced by `Unit`. Every function has a type.        |
| `?:` ternary         | `if` is already an expression.                        |
| Operator overloading | Hides semantics behind innocent-looking syntax. Not in v0.1. |
| `goto`               | No.                                                   |
| Implicit conversions | All casts are explicit. No `Int` → `Float` coercion.  |

---

## 6. Turing Completeness

Ren is Turing complete. The minimal kernel required:

- **Mutable state:** `let` + `mut`
- **Conditional branching:** `if` / `else`
- **Unbounded iteration:** `while`
- **Function definition + recursion:** `fn`
- **I/O:** built-in `print` and `read_line`

### Demonstration

```ren
// Iterative FizzBuzz — proves loops and conditionals
fn fizzbuzz(n: Int) -> String {
    if n % 15 == 0      { "FizzBuzz" }
    else if n % 3 == 0  { "Fizz" }
    else if n % 5 == 0  { "Buzz" }
    else                { int_to_string(n) }
}

// Recursive factorial — proves recursion
fn factorial(n: Int) -> Int {
    if n <= 1 { 1 }
    else      { n * factorial(n - 1) }
}

fn main() {
    mut i = 1
    while i <= 100 {
        print(fizzbuzz(i))
        i = i + 1
    }

    print(int_to_string(factorial(10)))
}
```

---

## 7. Built-in Functions (v0.1)

These are provided by the runtime before a standard library exists.

| Function                    | Signature                        | Description                  |
|-----------------------------|----------------------------------|------------------------------|
| `print`                     | `fn(String) -> Unit`             | Print to stdout with newline |
| `read_line`                 | `fn() -> String`                 | Read a line from stdin       |
| `int_to_string`             | `fn(Int) -> String`              | Format Int as String         |
| `float_to_string`           | `fn(Float) -> String`            | Format Float as String       |
| `string_to_int`             | `fn(String) -> Option<Int>`      | Parse Int, None on failure   |
| `string_to_float`           | `fn(String) -> Option<Float>`    | Parse Float, None on failure |
| `sqrt`                      | `fn(Float) -> Float`             | Square root                  |
| `panic`                     | `fn(String) -> Unit`             | Abort with message           |

---

## 8. Program Entry Point

Every Ren program must define a `main` function:

```ren
fn main() -> Unit {
    ...
}
```

---

## 9. Open Questions (deferred to compiler phase)

- Generics beyond `Option<T>` and `Result<T, E>` — syntax and constraints
- Lifetime / ownership model — garbage collected vs. reference counting vs. ownership (decision needed before compiler)
- Concurrency primitives — not in v0.1
- String interpolation syntax
- Multi-file module resolution
- Integer overflow behavior — panic vs. wrapping vs. saturating

---

*This document tracks the Ren v0.1 specification. Changes require explicit versioning.*
