# Sub-Expression Typing: Ile vs MLScript

## Problem Statement

Both Ile and MLScript are based on the MLStruct type system, which was designed to
type **top-level declarations**. When transpiling to a target language (Go for Ile,
JS for MLScript), the compiler needs to know the types of **sub-expressions** too -
for example, to determine the Go type of a variable binding inside a function body,
or to decide how to box/unbox values.

This document compares how Ile and MLScript solve this problem.

## MLScript's Approach: Types Don't Leave the Typer

MLScript's codegen (`JSBackend.scala`) **does not use inferred types at all for most
expressions**. It re-traverses the original, untyped AST and generates JS directly:

```scala
// JSBackend.scala:116
protected def translateTerm(term: Term)(implicit scope: Scope): JSExpr = term match {
  case Var(name) => translateVar(name, false)
  case Lam(params, body) =>
    val patterns = translateParams(params)
    val lamScope = Scope("Lam", patterns flatMap { _.bindings }, scope)
    JSArrowFn(patterns, lamScope.tempVars `with` translateTerm(body)(lamScope))
  case Asc(trm, _) => translateTerm(trm)  // type annotation erased
  // ...
}
```

This works because JavaScript is dynamically typed. The codegen only needs type
information in a few cases:

- **Pattern matching**: to decide whether to emit `instanceof` checks or trait
  membership tests (looked up via `Scope.getType(name)`)
- **Top-level display**: to show inferred types to the user (uses `expandType` on
  `TypeScheme` values stored in the `Ctx`)

The `Typer` returns `SimpleType` values from `typeTerm()` but these are **not cached
against AST nodes**. They flow through function returns during inference, get
constrained, and are discarded. Only top-level `TypeScheme` bindings survive in the
`Ctx`.

## Ile's Approach: Cache SimpleTypes, Expand on Demand

Ile transpiles to **Go**, a statically typed language. Every variable, every
intermediate expression needs a concrete Go type. The backend calls `TypeOf` on
arbitrary sub-expressions dozens of times (record literals, function arguments,
let bindings, pattern match scrutinees, etc.):

```go
// backend/expressions.go
fnType, ok := tp.types.TypeOf(e).(*ir.FnType)
// backend/declarations.go
declType := tp.types.TypeOf(decl.E)
```

To support this, Ile uses a two-layer cache:

1. **During inference** (`typeExpr` in `type_expr.go`): every sub-expression is
   cached as a `nodeCacheEntry{t SimpleType, at level, pol polarity}`, keyed by
   source range + expression hash.

2. **Post-inference** (`TypeOf` in `type_context.go`): the cached `SimpleType` is
   instantiated, simplified, expanded to `ir.Type`, and the result is cached again
   in `expandedTypeCache` so the simplification pipeline runs at most once per
   expression.

```go
// type_context.go:558
func (ctx *TypeCtx) TypeOf(expr ir.Expr) (ret ir.Type) {
    // check expandedTypeCache (ir.Type)
    // then fall back to cache (SimpleType) → instantiate → simplify → expand
}
```

### The simplification + expansion pipeline

```
SimpleType
  → instantiate(fresher, level)
  → simplifyPipeline (bound cleaning, variable analysis, substitution, normalization)
  → expandSimpleType → ir.Type
```

This pipeline is equivalent to what MLScript does in `getType()` for display, but
Ile runs it for **every sub-expression the backend asks about**, not just top-level
bindings.

## Key Differences

| Aspect | MLScript | Ile |
|---|---|---|
| Target language | JS (dynamic) | Go (static) |
| Codegen needs types? | Rarely (pattern matching only) | Always |
| AST carries types? | No | No (but types cached alongside) |
| Type cache | None - `SimpleType` returned from `typeTerm` and discarded | Two-layer: `SimpleType` during inference, `ir.Type` post-inference |
| When expansion happens | Only for top-level display | On every `TypeOf` call from backend |
| Sub-expression types accessible? | No (would need re-inference) | Yes, via cache keyed by range + hash |

## Potential Shortcomings of the Ile Approach

### 1. Cache key collisions

Ile keys the cache by `(ir.Range, exprHash)`. If two distinct expressions happen to
share the same source range and structural hash, they would collide. The code has a
TODO noting that scope hashing would help. In practice this is unlikely for
well-formed source but could cause subtle bugs with generated or synthetic nodes.

### 2. Polarity must be correct at cache time

The `SimpleType` is cached with a polarity during inference. When `TypeOf` expands it
later, it uses that polarity. If the same expression appears in both positive and
negative positions (e.g., a variable used as both a function argument and return
value), the cached polarity may not reflect all uses. MLScript avoids this problem
entirely by not caching sub-expression types.

### 3. Simplification pipeline completeness

Ile's simplification pipeline has several unimplemented phases compared to the Scala
reference:

- `unskidTypes_!` - not implemented
- `factorRecursiveTypes_!` - not implemented
- `computeVariances` - panics with TODO

These missing phases mean some types may not simplify fully, leading to overly
complex or incorrect `ir.Type` representations that the backend must handle.

### 4. Memory overhead

Every sub-expression in the program has both a `SimpleType` cache entry and
(eventually) an `ir.Type` cache entry. For large programs this could be significant.
MLScript has no such overhead since types are ephemeral.

### 5. Instantiation timing

When `TypeOf` is called, it instantiates the cached `SimpleType` at the level stored
during inference. If the type is polymorphic and the call site should have produced a
monomorphic instantiation, the expansion may produce unexpected type variables. The
recent "reified types" work (commit ac1cec5) addressed one manifestation of this
where polymorphic functions' types were being polluted by call sites, but the
fundamental tension remains: the cache stores a single type per expression, while
the same polymorphic expression may need different instantiations at different use
sites.

## MLScript's C++ Backend: The Typed IR Approach

MLScript also has a C++ backend (under `hkmc2/`), which faces the same problem as
Ile: C++ is statically typed and needs type information for sub-expressions. The C++
backend takes a fundamentally different approach from the JS backend — it uses a
**typed intermediate representation (LLIR)**.

The pipeline is:

```
Source AST → Typer → Block IR (symbols carry type info) → LlirBuilder → LLIR → CppCodeGen → C++
```

LLIR nodes encode type information inline. For example:

```scala
// hkmc2/codegen/llir/Llir.scala
enum Expr:
  case Select(name: Local, cls: Local, field: Str)      // cls carries the class
  case CtorApp(cls: DefinitionSymbol[?], args: Ls[...]) // cls is explicit

enum Node:
  case LetMethodCall(names: Ls[Local], cls: Local, method: Local, ...)
```

The C++ codegen reads type info directly from these IR nodes — no cache, no
re-inference:

```scala
// hkmc2/codegen/cpp/CodeGen.scala
def codegen(expr: IExpr)(using Ctx, Raise, Scope): Expr = expr match
  case IExpr.Select(name, cls, field) =>
    Expr.Member(mlsAsUnchecked(name |> allocIfNew, cls |> mapClsLikeName), field |> mapName)
```

A **lowering pass** (`LlirBuilder`) converts the high-level Block IR into LLIR,
making all type information explicit in the process. Type info flows from the typer
through `Symbol` objects attached to Block IR nodes, and the lowering pass extracts
it into the LLIR's flat structure.

This avoids all five shortcomings of the cache-based approach: no cache key
collisions, no polarity ambiguity, no lazy simplification, no memory overhead from
dual caching, and no instantiation timing issues — because each LLIR node carries
exactly the type it needs.

## Could Ile Use MLScript's JS Approach?

No. MLScript's JS backend can get away without sub-expression types because JS
doesn't need them. Go requires explicit types for:

- Variable declarations (`var x T = ...`)
- Function signatures
- Type assertions and interface conversions
- Struct literals

## Could Ile Use MLScript's C++ Approach?

The LLIR approach is the cleaner architecture for a statically-typed target, but
adopting it in Ile would mean:

1. Defining a new typed IR where every node carries its resolved type
2. Writing a lowering pass from the current `ir.Expr` to the typed IR
3. Rewriting the Go backend to consume the typed IR instead of calling `TypeOf`

This is a large refactor. Ile's cache-based approach is a pragmatic middle ground
that avoids this work while still providing sub-expression types. The main risk is
that the shortcomings listed above (especially instantiation timing for polymorphic
types) may become harder to work around as the language grows.
