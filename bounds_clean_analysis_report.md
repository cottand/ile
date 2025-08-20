# cleanBounds vs removeIrrelevantBounds: Analysis Report

## Problem Summary

In Ile, the `cleanBounds()` function sometimes causes function types like `fn 1|2 -> 4|3` to become `fn Any -> 4|3`. This issue doesn't occur in MLStruct with the equivalent `removeIrrelevantBounds()` function. The problem manifests when the first `cleanBounds` step in the simplification pipeline is present (line 107 in `simplify.go`).

## Root Cause Analysis

### Key Functional Differences

#### 1. Polarity Determination Strategy

**Ile's Approach (`getVarsPolFor`):**
- Traverses the type structure and assigns a **single polarity** to each type variable
- When a variable appears in both positive and negative positions, it marks it as `invariant`
- However, the polarity assignment logic may not correctly handle all cases where variables appear in both positions

**MLStruct's Approach:**
- Uses a more conservative `forall` check requiring **all** occurrences to have the same polarity
- Only processes bounds when the variable appears exclusively in one polarity across all positions

#### 2. Bound Processing Logic

**Ile's `cleanBounds` (lines 81-104 in `bounds_clean.go`):**
```go
// Process lower bounds if all polarities for this variable are positive
pol, exists := cbc.pols[t.id]
if exists && pol == positive {
    // Process only lower bounds
}

// Process upper bounds if all polarities for this variable are negative  
pol, exists = cbc.pols[t.id]
if exists && pol == negative {
    // Process only upper bounds
}
```

**MLStruct's `removeIrrelevantBounds`:**
- More sophisticated filtering using `reduceOption(_ | _)` and `reduceOption(_ & _)`
- Better handling of invariant variables by preserving their bounds

### 3. The Function Type Problem

For a function type `fn 1|2 -> 4|3`, if there's a type variable `α` that appears in both:
- The argument position (contravariant/negative polarity)  
- The return position (covariant/positive polarity)

**Ile's Issue:**
1. `getVarsPolFor` may incorrectly determine the overall polarity of `α`
2. `cleanBounds` then processes only one set of bounds (lower OR upper) based on this determination  
3. This removes bounds that were actually relevant, leading to over-generalization to `Any`

**MLStruct's Solution:**
- Recognizes that `α` appears in both polarities
- Preserves both lower and upper bounds for such variables
- Doesn't attempt to simplify bounds for invariant variables

## Evidence from Ile's Implementation

### Polarity Detection in `getVarsPolFor` (factorise.go:337-350)

The critical logic that may cause issues:
```go
} else {
    // If the type variable has been seen before with a different polarity,
    // mark it as invariant and add its children to the queue
    seen[tv.id] = invariant
    // Yield the type variable with invariant polarity
    if !yield(tv, invariant) {
        return
    }
}
```

However, in `cleanBounds`, the processing logic doesn't handle invariant variables correctly:
- It checks for `pol == positive` or `pol == negative` but doesn't have explicit handling for `invariant`
- Variables marked as invariant might fall through without proper bound preservation

## Suggested Solution

### Approach 1: Conservative Invariant Handling (Recommended)

Modify `cleanBounds` to preserve bounds for invariant variables:

```go
// In bounds_clean.go, process method
pol, exists := cbc.pols[t.id]
if exists && pol == invariant {
    // For invariant variables, preserve both bounds without processing
    renewed.lowerBounds = make([]SimpleType, len(t.lowerBounds))
    copy(renewed.lowerBounds, t.lowerBounds)
    renewed.upperBounds = make([]SimpleType, len(t.upperBounds))  
    copy(renewed.upperBounds, t.upperBounds)
    return renewed
}
```

### Approach 2: Improve Polarity Detection

Enhance `getVarsPolFor` to be more conservative in polarity assignment, similar to MLStruct's approach:
- Only assign positive/negative polarity when ALL occurrences are in the same polarity
- Mark as invariant when ANY occurrence is in the opposite polarity

### Approach 3: Hybrid Solution

Combine both approaches:
1. Improve polarity detection to be more accurate
2. Add explicit invariant handling in `cleanBounds`
3. Add test cases covering function types with variables in both positions

## Testing Strategy

Add test cases to `bounds_clean_test.go` that cover:
1. Function types with type variables appearing in both argument and return positions
2. Variables that should be marked as invariant
3. Verification that `fn 1|2 -> 4|3` doesn't become `fn Any -> 4|3`

## Impact Assessment

**Risk:** Low - The conservative approach (Approach 1) will prevent over-simplification without breaking existing functionality.

**Benefit:** High - Fixes the core issue while aligning Ile's behavior with MLStruct's reference implementation.

## Conclusion

The discrepancy arises from Ile's `cleanBounds` being too aggressive with variables that appear in both positive and negative positions within function types. The solution is to adopt MLStruct's more conservative approach by properly handling invariant variables and not attempting to simplify their bounds.