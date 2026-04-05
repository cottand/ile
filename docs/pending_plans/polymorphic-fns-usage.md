# Fix `g(y)` sub-expression typed as Nothing inside polymorphic function bodies

## Context

`TestGenericsEndToEnd/doubleApplyId.ile` fails because the sub-expression `g(y)` inside `applyTwice(f, g, y) { f(g(y)) }` is typed as `Nothing` during post-inference type lowering. This causes the backend to wrap it in a dead-code closure (`func() any { g(y); return nil }()`), producing invalid Go code.

The same issue affects `doubleApply.ile` and `doubleApplyLambda.ile`.

## Root cause

`LowerTypes` (`frontend/types/lower.go:7-19`) calls `GetAstTypeFor` → `simplifyPipeline` on each sub-expression **independently**. For `g(y)`, the cached type is `α11''` — a type variable with no bounds on itself. Its meaning comes from co-occurrence with other variables in the parent function's constraint graph.

When simplified in isolation, `α11''` only appears at positive polarity → removed by rule "Remove non-recursive polar vars" (`simplify.go:232-247`) → empty lower bounds merge → `bottomType` → `NothingType`.

When the full function type is simplified, `α11''` appears at both polarities (arg of `f`'s bound, return of `g`'s bound) → preserved.

### Comparison with mlscript

mlscript's `TypeSimplifier` **never simplifies sub-expression types**. `simplifyType` (TypeSimplifier.scala:516) is called once per top-level binding on the full connected type graph. There is no `Map[Term, SimpleType]` cache — `typeTerm` returns ephemeral values. The co-occurrence analysis runs over the complete graph, so variables at both polarities are always preserved.

Ile needs sub-expression types for Go codegen (`injectPotentialNothingType`, `WhenMatch` IIFEs), but should follow the same principle: **simplify the full graph once, derive sub-expression types from that**.

## Fix: Simplify-then-derive via cumulative renewal mapping

Simplify the function type once (full pipeline). Capture the cumulative variable-renewal mapping across all pipeline passes. For sub-expressions inside the function body, substitute their cached type variables using this mapping, then expand.

### How the mapping works

The pipeline runs multiple passes that rename variables:
- `cleanBounds` (non-inPlace): `renewed map[TypeVarID]*typeVariable`
- `simplifyType` (×3): `renewals map[*typeVariable]*typeVariable`, `varSubst map[TypeVarID]*typeVariable`
- `normaliseType` (×2): in-place bound modification, no renaming

A cumulative `map[TypeVarID]*typeVariable` is initialized with all variables from `getVariables(funcType)` (which traverses bounds). After each renaming pass, the map is updated: if a variable's current target was renewed, follow the renewal.

For `applyTwice`: α10'', α11'', α12'' are **KEPT** (both polarities) → map to final vars (which match the function's `ir.TypeVar` identifiers). α8'', α9'' are **REMOVED** (one polarity) → not in the mapping, but the backend gets parameter types from the function signature, not `TypeOf(param)`.

### Implementation

**`frontend/types/simplify.go`:**

Add `simplifyPipelineWithVarMapping`:
```go
func (ctx *TypeCtx) simplifyPipelineWithVarMapping(st SimpleType, pol polarity) (SimpleType, map[TypeVarID]*typeVariable) {
    // Initialize mapping with all connected variables
    allVars := getVariables(st)
    mapping := make(map[TypeVarID]*typeVariable, len(allVars))
    for _, tv := range allVars {
        mapping[tv.id] = tv
    }

    // Run pipeline, composing mapping after each renaming pass
    cur := st
    cur, cbRenewed := ctx.cleanBoundsReturningRenewed(cur, cleanBoundsOpts{...inPlace: false})
    composeCleanBounds(mapping, cbRenewed)

    cur, renewals1, varSubst1 := ctx.simplifyTypeReturningState(cur, pol, true, true)
    composeSimplify(mapping, renewals1, varSubst1)

    cur = ctx.normaliseType(cur, pol)

    cur, renewals2, varSubst2 := ctx.simplifyTypeReturningState(cur, pol, true, true)
    composeSimplify(mapping, renewals2, varSubst2)

    cur = ctx.cleanBounds(cur, cleanBoundsOpts{...inPlace: true}) // in-place, no renaming

    cur, renewals3, varSubst3 := ctx.simplifyTypeReturningState(cur, pol, true, true)
    composeSimplify(mapping, renewals3, varSubst3)

    cur = ctx.normaliseType(cur, pol)
    return cur, mapping
}
```

Compose helpers:
```go
func composeSimplify(mapping map[TypeVarID]*typeVariable, renewals map[*typeVariable]*typeVariable, varSubst map[TypeVarID]*typeVariable) {
    for origID, currentVar := range mapping {
        // Follow renewal chain
        if renewed, ok := renewals[currentVar]; ok {
            mapping[origID] = renewed
        } else if sub, ok := varSubst[currentVar.id]; ok && sub != nil {
            // Variable was substituted (unified with another)
            if renewed, ok := renewals[sub]; ok {
                mapping[origID] = renewed
            } else {
                mapping[origID] = sub
            }
        }
        // If varSubst[id] == nil (removed), leave mapping unchanged —
        // the backend doesn't need types for removed vars (params come from fn signature)
    }
}
```

`simplifyTypeReturningState` — same as `simplifyType` but returns `(SimpleType, map[*typeVariable]*typeVariable, map[TypeVarID]*typeVariable)` (result, renewals, varSubst). Minimal change: extract `transformer.renewals` and `varSubst` before they go out of scope.

`cleanBoundsReturningRenewed` — same as `cleanBounds` but returns the `renewed` map.

**`frontend/types/lower.go`:**

Replace `ir.Walk` with a custom recursive walk. When encountering `ir.Func` whose type has type variables, compute the mapping and propagate it to sub-expressions:

```go
func (ctx *TypeCtx) LowerTypes(root ir.Expr) {
    ctx.loweredTypes = make(map[exprCacheEntry]ir.Type)
    ctx.lowerExpr(root, nil)
}

func (ctx *TypeCtx) lowerExpr(expr ir.Expr, varMapping map[TypeVarID]*typeVariable) {
    if expr == nil { return }
    // Walk children first (post-order, matching ir.Walk)
    switch e := expr.(type) {
    case *ir.Func:
        // Check if this function is polymorphic
        cached, ok := ctx.cache.getCached(expr)
        if ok {
            instantiated := cached.t.instantiate(ctx.fresher, cached.at)
            simplified, mapping := ctx.simplifyPipelineWithVarMapping(instantiated.uninstantiatedBody(), cached.pol)
            // Walk body with the new mapping
            ctx.lowerExpr(e.Body, mapping)
            // Store the function's own type (fully simplified)
            expanded := ctx.expandSimpleType(simplified, false)
            key := exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}
            ctx.loweredTypes[key] = expanded
            return
        }
    case *ir.Call:
        ctx.lowerExpr(e.Func, varMapping)
        for _, arg := range e.Args { ctx.lowerExpr(arg, varMapping) }
    // ... mirror all cases from ir.Walk ...
    }

    // Process this expression
    key := exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}
    cached, ok := ctx.cache.getCached(expr)
    if !ok { return }
    instantiated := cached.t.instantiate(ctx.fresher, cached.at)

    if varMapping != nil {
        // Inside a polymorphic function: substitute vars via mapping, then expand
        substituted := substituteVars(instantiated, varMapping)
        typ := ctx.expandSimpleType(substituted, true) // stopAtTyVars for unmapped vars
        ctx.loweredTypes[key] = typ
    } else {
        // Normal path
        typ := ctx.GetAstTypeFor(instantiated, cached.pol)
        ctx.loweredTypes[key] = typ
    }
}
```

**`frontend/types/simpleTypeFuncs.go`** (or new file):

Add `substituteVars` — walks a SimpleType and replaces `*typeVariable` nodes using the mapping:
```go
func substituteVars(t SimpleType, mapping map[TypeVarID]*typeVariable) SimpleType {
    t = unwrapProvenance(t)
    switch ty := t.(type) {
    case *typeVariable:
        if mapped, ok := mapping[ty.id]; ok && mapped != ty {
            return mapped
        }
        return ty
    case funcType:
        // recurse into args and ret
    // ... other composite types ...
    default:
        return t
    }
}
```

### Files to modify

- `frontend/types/simplify.go` — `simplifyPipelineWithVarMapping`, `simplifyTypeReturningState`, compose helpers
- `frontend/types/bounds_clean.go` — `cleanBoundsReturningRenewed`
- `frontend/types/lower.go` — custom recursive walk with mapping propagation
- `frontend/types/simpleTypeFuncs.go` — `substituteVars` helper

### What NOT to change

- `GetAstTypeFor`, `simplifyPipeline` — keep as-is, new variants alongside
- Backend code — no changes needed; sub-expression types will have matching identifiers
- `ir.Walk` — keep as-is, custom walk only in `lower.go`

## Verification

1. Run the target test: `go test -run TestGenericsEndToEnd/doubleApplyId.ile -v`
2. Run all generics tests: `go test -run TestGenericsEndToEnd -v`
3. Run all end-to-end tests to check for regressions: `go test -v`
