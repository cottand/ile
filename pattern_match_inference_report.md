# Pattern-Match Inference Difference Analysis

## Problem Statement

Ile and MLStruct implement inference of pattern-match expressions but provide different results for the same expression:

```scala
fn fib(x) {
  when(x) {
    1 -> 2
    0 -> 3
  }
}
```

**Expected type (MLStruct):** `0|1 -> 2|3`  
**Actual type (Ile):** `Any -> 2|3`

The issue is that Ile fails to properly constrain the input type to the union of matched patterns.

## Root Cause Analysis

### The Core Issue: Constraint Direction Mismatch

The fundamental problem lies in **how the pattern-match constraint is applied to the function parameter**. 

In Ile's `typeWhenMatch` function (`frontend/types/type_expr.go:276`):

```go
return ctx.typeExprConstrain(subjectType, requestedMatchedOnType, finalType, prov)
```

This calls `typeExprConstrain` which applies the constraint as:
```go
ctx.constrain(lhs, rhs, prov, onErr)  // subjectType <: requestedMatchedOnType
```

However, **the function parameter type variable is never directly constrained**.

### Function Parameter Type Variable Lifecycle

1. **Function Definition** (`type_expr.go:145-146`):
   ```go
   argType := ctx.newTypeVariable(newOriginProv(expr, "function parameter", ""), "", nil, nil)
   nested.env[arg] = argType
   ```
   - Creates an unconstrained type variable for parameter `x`

2. **When-Match Processing**:
   - The `when(x)` expression types `x` by looking it up in the environment
   - Gets the original unconstrained function parameter type variable
   - Builds constraint `requestedMatchedOnType = 0|1` 
   - Applies constraint: `typeof(x) <: (0|1)`

3. **The Problem**:
   - The constraint **does get applied** to the function parameter type variable
   - But the **function's argument type list still references the original unconstrained variable**
   - The function type is built before the when-match constraint is processed

### Detailed Flow Comparison

#### MLStruct (Working Correctly):
1. Function parameter gets type variable `T`
2. When-match constrains `T <: (0|1)`  
3. Function type uses the **same constrained** `T`
4. Result: `(0|1) -> (2|3)`

#### Ile (Broken):
1. Function parameter gets type variable `T` 
2. Function type is immediately built as `T -> ?`
3. When-match constrains `T <: (0|1)` 
4. But function's `argTypes` still references the **original unconstrained** `T`
5. During constraint solving, the function type doesn't see the pattern constraints
6. Result: `Any -> (2|3)`

## Technical Details

### Pattern Constraint Building (Working Correctly)

The pattern constraint building in Ile correctly follows MLStruct's algorithm:

```go
requestedMatchedOnType = unionOf(
    intersectionOf(branchT, tVar, unionOpts{}), 
    intersectionOf(requestedMatchedOnType, negateType(branchT, emptyProv), unionOpts{}), 
    unionOpts{}
)
```

This accumulates the union of all matched patterns: `0|1`.

### Constraint Application (Working Correctly)

The constraint application also works:
```go
ctx.typeExprConstrain(subjectType, requestedMatchedOnType, finalType, prov)
```

This correctly constrains the subject variable with the pattern union.

### The Missing Link: Function Type Construction Order

The issue is in the **order of operations**:

1. **Function typing happens first** and captures unconstrained parameter types
2. **Body typing happens second** and adds constraints to the parameter variables  
3. **Function type is already finalized** and doesn't reflect the constraints added during body typing

## Why MLStruct Works

In MLStruct, the constraint solving happens **after** the entire function expression has been typed, allowing the pattern constraints to properly flow back to the function parameters through the constraint solver's unification process.

## Solution Direction

The fix would require ensuring that function parameter constraints discovered during body typing are properly reflected in the final function type. This could be achieved by:

1. **Delayed function type construction**: Build the function type after body typing completes
2. **Constraint propagation**: Ensure parameter constraints flow back to the function type
3. **Post-processing**: Update function types based on constraints discovered during body typing

## Conclusion

The difference arises not from the pattern-match algorithm itself (which is correctly implemented), but from the interaction between function type construction and constraint discovery timing. Ile builds function types too early, before pattern-match constraints can influence the parameter types, resulting in unconstrained `Any` parameter types instead of the precise pattern unions that MLStruct correctly infers.