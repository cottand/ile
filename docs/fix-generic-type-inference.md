# Fix: Wrong Inferred Types for Generic Functions in LetGroups

## The Bug

Functions like `fn id1(x) { x }` display incorrect inferred types (e.g., `fn Int -> Nothing`)
when other declarations in the same package call them (e.g., `fn addOne(x) { id1(x) + 1 }`).

Expected: `fn 'a -> 'a`. Got: `fn Int -> Nothing`.

Test case: `test/generics/identityInferred.ile`.

## How Ile Currently Works

### The LetGroup

All top-level declarations in a package are placed into a **single `LetGroup`** by
`asGroupedLetWithImports` in `frontend/infer_phase.go:158-225`. The structure looks like:

```
LetGroup{
  Vars: [
    {Var: "id1",      Value: LetGroup{imports, Ascribe(Func(x, x), fn _ -> _)}},
    {Var: "id2",      Value: LetGroup{imports, Ascribe(Func(y, y), fn _ -> _)}},
    {Var: "addOne",   Value: LetGroup{imports, Ascribe(Func(x, id1(x)+1), fn _ -> _)}},
    {Var: "printOne", Value: LetGroup{imports, Ascribe(Func(x, fmt.Sprintf(x,[])), fn _ -> _)}},
  ],
  Body: IntLiteral("1")  // dummy, set by InferencePhase
}
```

### How the LetGroup is Typed (frontend/types/type_expr.go ~line 119)

```go
case *ir.LetGroup:
    // PHASE 1: Create binding variables, add as RAW type variables
    bindingVars := make([]*typeVariable, len(expr.Vars))
    for i, binding := range expr.Vars {
        bindingVars[i] = ctx.fresher.newTypeVariable(ctx.level+1, ...)
        ctx.env[binding.Var] = bindingVars[i]  // <-- RAW, not PolymorphicType!
    }

    // PHASE 2: Type each binding's value and constrain
    for i, binding := range expr.Vars {
        typed := ctx.nextLevel().TypeExpr(binding.Value, vars)
        ctx.constrain(typed, bindingVars[i], ...)
    }

    // PHASE 3: Create PolymorphicType wrappers for the body
    nested := ctx.nest()
    for i, binding := range expr.Vars {
        typeScheme := PolymorphicType{_level: ctx.level, Body: bindingVars[i]}
        nested.env[binding.Var] = typeScheme
    }
    return nested.TypeExpr(expr.Body, vars)
```

### Why This Causes Pollution

In Phase 1, all binding vars are stored as **raw type variables**. In Phase 2, when typing
`addOne`'s value, it looks up `id1` in the env -> gets raw `bv_id1` -> `instantiate()` on a
raw typeVariable is a **no-op** (see `frontend/types/datatypes.go:538`). So the callsite
`id1(x)` in `addOne` constrains `bv_id1` directly:

```
bv_id1 <: funcType{α_addone_x, res_call}
```

This propagates (via the constraint solver in `frontend/types/constrain.go`) from `bv_id1`'s
lower bound into the funcType's variables:

```
funcType{α_x, α_x} <: funcType{α_addone_x, res_call}
-> α_addone_x <: α_x   (adds lower bound to α_x)
-> α_x <: res_call      (adds upper bound to α_x)
```

And from `addOne`'s `+ 1` operation: `res_call <: Int`, `α_addone_x <: Int`.

So `α_x` (id1's parameter type variable) now has bounds: `[Int <: α_x <: Int]`.

### Why TypeOf Returns the Wrong Type

After inference, `TypeOf(decl.E)` (`frontend/types/type_context.go:561`) retrieves the cached
type for the declaration expression. For id1, it retrieves `funcType{α_a, α_b}` (from the
Ascribe wrapper). The variables α_a, α_b are connected to α_x through constraints:
`α_a <: α_x <: α_b`. Since α_x is polluted with Int bounds, the simplifier resolves:

- α_a (at negative/arg position) -> upper bounds chain -> Int
- α_b (at positive/ret position) -> lower bounds chain -> Nothing (because the simplifier
  follows a different path that bottoms out)

Result: `fn Int -> Nothing`.

`funcType.instantiate()` is a no-op (`datatypes.go:719`), so no freshening happens -- the
polluted variables are used directly.

## How MLStruct (Scala Reference) Avoids This

**File: `/Users/nico/dev/hkust-taco/mlstruct/shared/src/main/scala/mlscript/Typer.scala`**

MLStruct processes top-level definitions **sequentially**, not in a single LetGroup:

```scala
// In DiffTests.scala -- top-level definitions processed one by one:
terms.foreach {
  case Def(isrec, nme, L(rhs)) =>
    val ty_sch = typer.typeLetRhs(isrec, nme.name, rhs)(ctx, raise)
    ctx += nme.name -> ty_sch  // Added to context BEFORE next definition
}
```

Each definition is typed -> wrapped in `PolymorphicType` -> added to context -> THEN the next
definition is typed. So when `addOne` references `id1`, it finds a `PolymorphicType`, and
`PolymorphicType.instantiate` calls `freshenAbove` which creates fresh copies of all type
variables above the polymorphism level. Callsite constraints go to fresh copies.

**Key: `typeLetRhs` in Typer.scala ~line 385:**
```scala
def typeLetRhs(isrec: Boolean, nme: Str, rhs: Term): PolymorphicType = {
  val res = if (isrec) {
    val e_ty = freshVar(lvl + 1)
    ctx += nme -> e_ty              // raw var for self-reference during typing
    val ty = typeTerm(rhs)(ctx.nextLevel, ...)
    constrain(ty, e_ty)
    e_ty
  } else typeTerm(rhs)(ctx.nextLevel, ...)
  PolymorphicType(lvl, res)         // wrap AFTER typing
}
```

MLStruct does NOT have a LetGroup for mutual recursion. Individual `let rec` handles
self-recursion only.

## How MLScript Handles Mutual Recursion

**Key files in `/Users/nico/dev/hkust-taco/mlscript/`:**
- `shared/src/main/scala/mlscript/NuTypeDefs.scala` -- definition processing
- `shared/src/main/scala/mlscript/Typer.scala` -- type inference
- `shared/src/main/scala/mlscript/TyperDatatypes.scala` -- PolymorphicType
- `shared/src/main/scala/mlscript/ConstraintSolver.scala` -- constraint solving
- `shared/src/main/scala/mlscript/TypeSimplifier.scala` -- type simplification

MLScript DOES support mutually recursive top-level functions while preserving polymorphism.

### The Mechanism

**Step 1: Collect all definitions, create lazy type info (NuTypeDefs.scala:528-616)**

All declarations in a typing unit are collected into `DelayedTypeInfo` objects and added to
the context BEFORE any bodies are typed:

```scala
val lti = new DelayedTypeInfo(decl, implicitly)
ctx += name -> lti   // lazy, not yet computed
```

**Step 2: Each function gets a `mutRecTV` (NuTypeDefs.scala:1090-1098)**

```scala
lazy val mutRecTV: TV = freshVar(
  TypeProvenance(...), N, S(decl.name)
)(if (isGeneralized) level + 1 else level)
```

This is a fresh type variable at `level + 1` (for generalized/polymorphic functions).

**Step 3: Variable lookup during typing (Typer.scala:965-1003)**

When function A references function B during mutual recursion typing:

```scala
case lti: LazyTypeInfo =>
  lti match {
    case ti: DelayedTypeInfo =>
      ti.typeSignature(false, prov.loco)   // calls complete() lazily
  }
```

**Step 4: The `typeSignature` method (NuTypeDefs.scala:1876-1886)**

```scala
def typeSignature(...): ST = decl match {
  case _: NuFunDef =>
    if (isComputing) {
      mutRecTV  // <- Returns raw TV during active mutual recursion!
    } else complete() match {
      case TypedNuFun(_, fd, ty) => ty  // <- Returns fully computed type
    }
}
```

**Critical insight**: When A calls B and B is currently being computed (cycle), it gets the
raw `mutRecTV` (monomorphic). When A calls B and B is NOT being computed, `complete()` is
triggered lazily -- B is typed on demand -- and the completed type is returned.

**Step 5: Body typing at level+1 (NuTypeDefs.scala:1153-1196)**

```scala
val body_ty = if (isGeneralized) {
  ctx.nextLevel { implicit ctx: Ctx =>
    val ty = typeTerm(body)
    ty
  }
} else { ... }
// Later:
constrain(body_ty, mutRecTV)
```

**Step 6: Type signature for external use (NuTypeDefs.scala:428-431)**

```scala
lazy val typeSignature: ST =
  if (fd.isMut) bodyType
  else PolymorphicType.mk(level, bodyType)
```

After completion, the type signature wraps `bodyType` in `PolymorphicType`. External consumers
get a polymorphic instantiation.

### Key Design Comment from MLScript

From NuTypeDefs.scala:1159-1161:
```scala
// * So instead of inserting an individual `forall` in the type of each method,
// * we consider the `forall` to be implicit in the definition of TypedNuFun,
// * and that forall is eschewed while typing mutually-recursive methods
// * in the same typing unit, which will see the method's type through its mutRecTV.
```

**Translation**: Within a mutual recursion group, functions see each other through raw
`mutRecTV` variables (monomorphic). External code sees them through `PolymorphicType` wrappers.

### The Lazy Evaluation Is Key

MLScript's approach relies on **lazy evaluation** to handle non-circular forward references:

- When A calls B and B hasn't been typed yet -> triggers B's `complete()` -> B is typed first
  -> B's `mutRecTV` gets its bounds -> A gets B's full type
- When A calls B and B calls A (cycle) -> B sees A's incomplete `mutRecTV` (monomorphic)

Ile types bindings **eagerly in sequence**, so it can't do exactly this. But it CAN approximate
it by generalizing each binding immediately after it's typed (see Proposed Fix below).

## The `freshenAbove` Mechanism

**Ile: `frontend/types/level.go:57-220`**
**MLScript: `shared/src/main/scala/mlscript/ConstraintSolver.scala:549-608`**

`freshenAbove(newLevel, limit, type)` creates fresh copies of all type variables with
level > `limit`. It:

1. Walks the type structure recursively
2. When encountering a typeVariable with level > limit:
   a. Creates a fresh variable (empty bounds initially)
   b. Records mapping `oldID -> freshVar` in the `freshened` map
   c. Recursively freshens the old variable's lower and upper bounds
   d. Assigns the freshened bounds to the fresh variable
3. Returns the freshened type

**Critical**: The fresh variable gets **copies** of all bounds (including callsite pollution).
This is why the previous plan (TypeOfDecl wrapping the binding variable in PolymorphicType)
failed -- the fresh copy carried the same polluted bounds.

### Bug: Ile's `freshened` Map Persists Across Calls

**Ile** stores the `freshened` map as a field on `Fresher` (`level.go:14`). It persists across
`freshenAbove` calls. This means if function A instantiates id1's type, and later function B
also instantiates id1's type, B reuses A's stale fresh copies instead of creating new ones.

**MLScript** creates a local `MutMap.empty` per `freshenAbove` invocation
(`ConstraintSolver.scala:550`):
```scala
def freshenAbove(lim: Int, ty: SimpleType, ...): SimpleType = {
  val freshened = MutMap.empty[TV, SimpleType]  // LOCAL per call
  def freshen(ty: SimpleType): SimpleType = ...
  freshen(ty)
}
```

**This is a secondary bug that must be fixed.**

## Constraint Propagation Answers

### Does constraint propagation pollute bodyType in MLScript?

**YES**, MLScript's constraint solver propagates through bounds identically to Ile's:

```scala
// ConstraintSolver.scala:859-886 -- TypeVariable as LHS
case (lhs: TypeVariable, rhs) if rhs.level <= lhs.level =>
    lhs.upperBounds ::= newBound     // update the bound
    lhs.lowerBounds.foreach(rec(_, rhs, true)) // propagate from the bound

// ConstraintSolver.scala:888-915 -- TypeVariable as RHS
case (lhs, rhs: TypeVariable) if lhs.level <= rhs.level =>
    rhs.lowerBounds ::= newBound     // update the bound
    rhs.upperBounds.foreach(rec(lhs, _, true)) // propagate from the bound
```

So when `constrain(body_ty, mutRecTV)` establishes `body_ty <: mutRecTV`, and later a callsite
does `constrain(mutRecTV, callsite_funcType)`, the solver propagates into `body_ty`'s variables.

**BUT** this only matters for the MONOMORPHIC case (within mutual recursion). For the
POLYMORPHIC case (external callers), `PolymorphicType.instantiate` -> `freshenAbove` creates
fresh copies with a **local freshened map**, so callsite constraints go to fresh copies only.

The pollution of the original binding variable doesn't matter because external code never sees
the raw variable directly -- it always goes through freshening.

### How does the simplifier handle clean vs polluted variables?

For a clean `funcType{α_a, α_b}` where α_a and α_b are connected only through Ascribe:

The simplifier's analysis finds α_a and α_b are **bipolar** (occur at both polarities through
the bound chain α_a <: α_x <: α_b). Rule 4 (unify co-occurring variables) unifies them into
a single type variable, producing `fn 'a -> 'a`.

For a polluted case, the same analysis finds additional bounds (e.g., Int), and the merged
bounds produce `fn Int -> Nothing` or similar.

**Conclusion**: The simplifier works correctly -- the fix must ensure type variables stay clean.

## Proposed Fix

### Core Idea: Generalize Each Binding Immediately After Typing

Instead of waiting until Phase 3 to create PolymorphicType wrappers, generalize each binding
in Phase 2 immediately after it's typed and constrained. This way, later bindings in the same
LetGroup see earlier bindings through PolymorphicType wrappers and get fresh instantiations.

This matches MLScript's behavior: within a mutual recursion group, **already-typed** bindings
are accessed polymorphically, while **not-yet-typed** bindings (forward refs) are accessed
monomorphically (through the raw type variable from Phase 1).

### Change 1: Fix `freshened` map -- `frontend/types/level.go`

Make the `freshened` map local to each `freshenAbove` invocation.

**Current code (line 61-63):**
```go
func (t *Fresher) freshenAboveWithRigidify(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	return t.freshen(l, limit, type_, rigidify)
}
```

**New code:**
```go
func (t *Fresher) freshenAboveWithRigidify(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	old := t.freshened
	t.freshened = make(map[TypeVarID]SimpleType)
	result := t.freshen(l, limit, type_, rigidify)
	t.freshened = old
	return result
}
```

This matches MLScript's local `MutMap.empty` per invocation.

### Change 2: Generalize in Phase 2 -- `frontend/types/type_expr.go`

In the LetGroup case (~line 119), add a generalization line after each binding is constrained.

**Current Phase 2 (lines 127-131):**
```go
for i, binding := range expr.Vars {
    typed := ctx.nextLevel().TypeExpr(binding.Value, vars)
    bindingProv := newOriginProv(binding.Value, "binding of "+binding.Value.Describe(), "")
    ctx.constrain(typed, bindingVars[i], bindingProv, constrainOnErr)
}
```

**New Phase 2:**
```go
for i, binding := range expr.Vars {
    typed := ctx.nextLevel().TypeExpr(binding.Value, vars)
    bindingProv := newOriginProv(binding.Value, "binding of "+binding.Value.Describe(), "")
    ctx.constrain(typed, bindingVars[i], bindingProv, constrainOnErr)
    ctx.env[binding.Var] = PolymorphicType{_level: ctx.level, Body: bindingVars[i]}
}
```

Phase 1 and Phase 3 remain unchanged.

### Change 3: Revert Previous Plan's Changes

The previous plan added `declSchemes`, `TypeOfDecl`, and modified the backend. All of these
should be reverted:

**`frontend/types/type_context.go`:**
- Remove `declSchemes map[string]PolymorphicType` field from TypeState (line 111)
- Remove `declSchemes: make(map[string]PolymorphicType),` from NewEmptyTypeCtx (line 142)
- Remove the entire `TypeOfDecl` method (lines 587-605)

**`frontend/types/type_expr.go`:**
- Remove `ctx.declSchemes[binding.Var] = typeScheme` (line 140)

**`frontend/infer_phase.go`:**
- Change `ctx.TypeOfDecl(decl.Name, decl.E)` back to `ctx.TypeOf(decl.E)` (line 58)

**`backend/declarations.go`:**
- Change `tp.types.TypeOfDecl(decl.Name, decl.E)` back to `tp.types.TypeOf(decl.E)` (line 35)
- Change `tp.types.TypeOfDecl(irFunc.Name, irFunc.E)` back to `tp.types.TypeOf(irFunc.E)` (line 95)

### Why This Should Work

For the test case `identityInferred.ile`:

1. Phase 1: `bv_id1`, `bv_id2`, `bv_addOne`, `bv_printOne` created as raw vars in env
2. Phase 2, i=0: Type id1 -> `funcType{α_x, α_x}` -> constrain to bv_id1 ->
   **immediately** set `env["id1"] = PolymorphicType{level, bv_id1}`
3. Phase 2, i=2: Type addOne -> body calls `id1(x)`:
   - Lookup id1 -> gets `PolymorphicType{level, bv_id1}`
   - `instantiate()` -> `freshenAbove` (with fresh `freshened` map!) ->
     creates `bv_id1'` with freshened lower bound `funcType{α_x', α_x'}`
   - Callsite constrains `bv_id1'` -> `α_x' <: Int` etc.
   - **Original `bv_id1` and `α_x` stay clean!**
4. `TypeOf(id1.E)` retrieves clean `funcType{α_a, α_b}` from cache ->
   simplifier sees unconstrained bipolar vars -> produces `fn 'a -> 'a`

### Behavior for Different Reference Patterns

| Pattern | Behavior | Correct? |
|---------|----------|----------|
| Back reference (addOne->id1, id1 typed first) | Polymorphic instantiation, clean types | Yes |
| Self recursion (f calls f) | Phase 1 raw var used (monomorphic) | Yes (standard) |
| Forward reference (A->B, B typed later) | Phase 1 raw var used (monomorphic, no bounds yet) | Acceptable* |
| Mutual recursion cycle (A->B->A) | First typed gets raw var of second (monomorphic) | Acceptable* |

*Forward references and cycles get monomorphic references, same as standard let-rec. This
matches MLScript's behavior for cycles (where `isComputing` returns the raw `mutRecTV`).
For non-circular forward refs, MLScript does better (lazy eval types the target first), but
Ile's eager sequential approach can't do this without major refactoring.

## Verification

```bash
go test -run TestGenericsEndToEnd/identityInferred.ile -v

go test -run TestGenericsEndToEnd -v
go test -run TestExpressionsEndToEnd -v
go test -run TestFunctionsEndToEnd -v
go test -run TestInteropEndToEnd -v

go test -run TestFunctionsEndToEnd/mutuallyRecursive -v
```
