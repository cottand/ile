# Fix: Generic Type Inference Polluted by Callsites

## Context

Generic functions in Ile get their type variables polluted by callsites instead of staying polymorphic. For example, given:

```ile
fn id1(x) { x }
fn addOne(x) { id1(x) + 1 }
```

`id1` is inferred as `fn Int -> Nothing` instead of `fn 'a -> 'a`. The generated Go code is broken:

```go
func id1(x int64) { x }           // wrong — should be func id1[A any](x A) A
func addOne(x int64) int64 { ... }
```

**Failing test:** `go test -run TestGenericsEndToEnd/identityInferred.ile -v`
**Proposed solution doc:** `docs/fix-generic-type-inference.md` (proofread and confirmed correct)

### Root cause

All top-level declarations in a package become a single `ir.LetGroup` (see `frontend/infer_phase.go:158-225`). The LetGroup is typed in 3 phases (`frontend/types/type_expr.go:119-142`):

1. **Phase 1:** Create raw type variables for each binding, store in env
2. **Phase 2:** Type each binding's value and constrain against its type variable
3. **Phase 3:** Wrap binding variables in `PolymorphicType`, type the body expression

The bug: In Phase 2, when `addOne` references `id1`, it finds `id1`'s **raw type variable** in the env. `typeVariable.instantiate()` is a no-op (`datatypes.go:538`), so `addOne`'s callsite constraints flow directly into `id1`'s type variables. The `PolymorphicType` wrapping in Phase 3 comes too late.

There is a secondary bug: the `freshened` map on `Fresher` (`level.go:14`) persists across `freshenAbove()` calls, so repeated instantiations of the same polymorphic type reuse stale fresh variables instead of creating independent ones. MLScript creates a local map per call (`ConstraintSolver.scala:550`).

### Previous attempted fix (currently staged, doesn't work)

A `declSchemes` map and `TypeOfDecl` method were added to store/retrieve `PolymorphicType` wrappers separately. This fails because it freshens AFTER pollution — the fresh copies carry the same polluted bounds (e.g., `Int` is a `classTag`, unaffected by freshening).

### Comparison with MLScript

Verified against `/Users/nico/dev/hkust-taco/mlscript/` — the proposed Ile fix is NOT a 1:1 match with MLScript, but is actually **better for the common case**.

**MLScript's current behavior** (`NuTypeDefs.scala`):
- All functions in a typing unit see each other through raw body types or `mutRecTV` (a monomorphic placeholder for cycles). See `typeSignature(usesNew, loco)` method at line 1876: returns raw `ty` for completed defs, `mutRecTV` for in-progress defs.
- Within a typing unit, callsite pollution happens for ALL cross-references. This is demonstrated in `FunPoly.mls:29-34`:
  ```
  fun test = [id(1), id(true)]
  fun id(x) = x
  //│ fun id: forall 'a 'b. ('b & 'a) -> (1 | true | 'a)   // polluted!
  ```
- MLScript developers acknowledge this is suboptimal (`IntraBlockPolymorphism.mls:4-6`):
  > "Note: eventually we should progressively type check every mutually-recursive group in topological order, to maximize polymorphism."
- Only AFTER all definitions complete does MLScript wrap with `PolymorphicType.mk` for external consumers (`TypedNuFun.typeSignature` lazy val, line 429-431).

**Ile's proposed fix** (immediate generalization in Phase 2):
- Back references (later declaration calls earlier): Earlier binding is already wrapped in `PolymorphicType` → later one gets fresh instantiation → NO pollution. **This is better than MLScript.**
- Forward references (earlier calls later): Later binding still has Phase 1 raw var → monomorphic → same behavior as MLScript.
- True mutual recursion cycles: Asymmetric — earlier sees later monomorphically (forward), later sees earlier polymorphically (back). MLScript has similar asymmetry (via lazy eval + `isComputing` flag).
- Self-recursion: Monomorphic via Phase 1 raw var (wrapping happens after constraining). Same as MLScript's `mutRecTV`.

**Risk assessment**: MLScript uses monomorphic `mutRecTV` within cycles specifically to avoid "cyclic-looking constraints due to the polymorphic recursion limitation" (comment at line 1159). In the proposed fix, self-recursion is still monomorphic (safe), and for mutual recursion, the polymorphic back-reference creates fresh copies that are disconnected from the cycle (safe — the freshened forward-ref variable in the copy has empty bounds and doesn't create cyclic constraints).

## Implementation: 3 Changes

**Order matters:** Apply Change 3 first (revert broken workaround), then Changes 1 & 2 (the actual fix).

---

### Change 3: Revert the `declSchemes`/`TypeOfDecl` workaround

This removes the previous attempted fix that's currently in the working tree.

#### 3a. `backend/declarations.go`

**Line 35** — change `TypeOfDecl` back to `TypeOf`:
```go
// before
declType := tp.types.TypeOfDecl(decl.Name, decl.E)
// after
declType := tp.types.TypeOf(decl.E)
```

**Line 95** — same change:
```go
// before
fnType = tp.types.TypeOfDecl(irFunc.Name, irFunc.E)
// after
fnType = tp.types.TypeOf(irFunc.E)
```

#### 3b. `frontend/infer_phase.go`

**Line 58** — change `TypeOfDecl` back to `TypeOf`:
```go
// before
typed := ctx.TypeOfDecl(decl.Name, decl.E)
// after
typed := ctx.TypeOf(decl.E)
```

#### 3c. `frontend/types/type_context.go`

Remove the `declSchemes` field from `TypeState` (line 111):
```go
// remove this line:
declSchemes map[string]PolymorphicType
```

Remove its initialization from `NewEmptyTypeCtx` (line 142):
```go
// remove this line:
declSchemes: make(map[string]PolymorphicType),
```

Remove the entire `TypeOfDecl` method (lines 587-605):
```go
// remove this entire method:
func (ctx *TypeCtx) TypeOfDecl(name string, expr ir.Expr) ir.Type { ... }
```

#### 3d. `frontend/types/type_expr.go`

Remove the `declSchemes` storage in Phase 3 of the LetGroup case (line 140):
```go
// remove this line from the Phase 3 loop:
ctx.declSchemes[binding.Var] = typeScheme
```

---

### Change 1: Fix `freshened` map scope — `frontend/types/level.go`

**Why:** The `freshened` map (which caches old→new type variable mappings during freshening) persists on the `Fresher` struct across calls. This means if function A instantiates `id1`'s type and later function B also instantiates it, B reuses A's stale fresh copies instead of getting independent ones.

**Lines 61-63**, replace the method body:

```go
// before
func (t *Fresher) freshenAboveWithRigidify(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	return t.freshen(l, limit, type_, rigidify)
}

// after
func (t *Fresher) freshenAboveWithRigidify(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	old := t.freshened
	t.freshened = make(map[TypeVarID]SimpleType)
	result := t.freshen(l, limit, type_, rigidify)
	t.freshened = old
	return result
}
```

This is safe because `freshen()` never calls `freshenAbove()` — all its branches recurse via `t.freshen(...)` only. The save/restore handles any hypothetical nesting correctly.

---

### Change 2: Generalize each binding immediately — `frontend/types/type_expr.go`

**Why:** After typing and constraining each binding in Phase 2, immediately wrap it in `PolymorphicType` in the env. This way, when a later binding references an earlier one, `instantiate()` calls `freshenAbove()` and creates fresh type variable copies. Callsite constraints go to the fresh copies, leaving the original clean.

**Line ~131**, add one line after the `constrain` call inside the Phase 2 loop:

```go
// before (lines 127-131)
for i, binding := range expr.Vars {
    typed := ctx.nextLevel().TypeExpr(binding.Value, vars)
    bindingProv := newOriginProv(binding.Value, "binding of "+binding.Value.Describe(), "")
    ctx.constrain(typed, bindingVars[i], bindingProv, constrainOnErr)
}

// after
for i, binding := range expr.Vars {
    typed := ctx.nextLevel().TypeExpr(binding.Value, vars)
    bindingProv := newOriginProv(binding.Value, "binding of "+binding.Value.Describe(), "")
    ctx.constrain(typed, bindingVars[i], bindingProv, constrainOnErr)
    ctx.env[binding.Var] = PolymorphicType{_level: ctx.level, Body: bindingVars[i]}
}
```

**Reference patterns after this change:**
| Pattern | What happens | Correct? |
|---------|-------------|----------|
| Back ref (B→A, A typed first) | A already wrapped → B gets fresh instantiation | Yes |
| Self-recursion (A→A) | Phase 1 raw var still in env during A's own typing (wrapping happens AFTER constrain) | Yes |
| Forward ref (A→B, B typed later) | B still raw from Phase 1 → monomorphic | Acceptable |
| Mutual recursion cycle | Asymmetric: earlier sees later monomorphically, later sees earlier polymorphically | Acceptable |

Phase 1 and Phase 3 remain structurally unchanged (Phase 3 redundantly re-wraps the same values in a nested scope — harmless).

---

## Test Coverage

### Existing tests to verify (should all pass)

```bash
# Primary fix — currently fails, should pass
go test -run TestGenericsEndToEnd/identityInferred.ile -v

# Already-passing generics tests (should not regress)
go test -run TestGenericsEndToEnd -v

# Mutual recursion (should not regress)
go test -run TestFunctionsEndToEnd/mutuallyRecursive -v

# Full regression suites
go test -run TestExpressionsEndToEnd -v
go test -run TestFunctionsEndToEnd -v
go test -run TestInteropEndToEnd -v
```

### New test: forward reference (caller before callee)

Add `test/generics/forwardRefInferred.ile` to verify forward references still compile correctly (monomorphic behavior is acceptable):

```ile
//ile:compilerTest test.addOne(1) | int64(2)

package test

fn addOne(x) { id1(x) + 1 }

fn id1(x) { x }
```

This tests the forward-reference case where `addOne` is typed before `id1`. `addOne` should still work (it sees `id1` monomorphically). `id1`'s type may be polluted (like MLScript), but `addOne(1)` should still return `2`.

### New test: multiple callers of generic function

Add `test/generics/multipleCallers.ile` to verify Change 1 (freshened map fix) — each caller should get independent fresh copies:

```ile
//ile:compilerTest test.result() | string("true 42")

package test

import fmt "go:fmt"

fn id(x) { x }

fn boolResult() { id(true) }
fn intResult() { id(42) }
fn result() { fmt.Sprintf("%v %v", boolResult(), intResult()) }
```

Without Change 1, the second instantiation of `id` would reuse the first's stale fresh variables, causing type interference between `boolResult` and `intResult`.

### What to look for in `identityInferred.ile` output

The generated Go AST (printed in verbose test output) should show `id1` and `id2` with polymorphic type parameters:
```go
func id1[A any](x A) A { ... }  // NOT: func id1(x int64) { ... }
func id2[A any](y A) A { ... }  // NOT: func id2(y any) { ... }
```

## Proofreading Notes on `docs/fix-generic-type-inference.md`

The document is accurate. Minor notes:
1. "Phase 1 and Phase 3 remain unchanged" in Change 2's section is slightly misleading — Change 3 also removes the `ctx.declSchemes` line from Phase 3.
2. `genericApply.ile` passes even without the fix (backend uses annotation types directly for annotated functions at `backend/declarations.go:92-96`). The actually failing test is `identityInferred.ile`.
