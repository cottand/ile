

# Correctness Differences Between Go and Scala Implementations

## Bounds Cleaning Differences

After comparing the Go implementation of `cleanBounds()` with the Scala reference implementation of `removeIrrelevantBounds()`, I've identified several correctness issues:

## 1. Incorrect Filtering of Extreme Types in Bounds

### Lower Bounds Filtering Bug
In the Go implementation:
```go
// Skip bottom types
if _, isBot := processedLB.(*extremeType); !isBot || !processedLB.(*extremeType).isTop() {
  processedLowerBounds = append(processedLowerBounds, processedLB)
}
```

This condition is incorrect. It will include a type if it's either not an extreme type OR if it's a top type. For lower bounds, we should skip bottom types, not include types that are either not extreme or are top.

The correct condition should be:
```go
// Skip bottom types
if et, isExtreme := processedLB.(*extremeType); !isExtreme || !et.isTop() {
  processedLowerBounds = append(processedLowerBounds, processedLB)
}
```

### Upper Bounds Filtering Bug
Similarly for upper bounds:
```go
// Skip top types
if _, isTop := processedUB.(*extremeType); !isTop || processedUB.(*extremeType).isTop() {
  processedUpperBounds = append(processedUpperBounds, processedUB)
}
```

This condition will include a type if it's either not an extreme type OR if it's a top type. For upper bounds, we should skip top types, not include them.

The correct condition should be:
```go
// Skip top types
if et, isExtreme := processedUB.(*extremeType); !isExtreme || !et.isTop() {
  processedUpperBounds = append(processedUpperBounds, processedUB)
}
```

## 2. Missing Type Reference Expansion

In the Scala implementation, built-in type references are expanded and then processed:
```scala
case tr @ TypeRef(defn, targs) if builtinTypes.contains(defn) => process(tr.expand, parent)
```

But in the Go implementation, there's a comment acknowledging this difference but not implementing it:
```go
// In the Scala reference, this would expand the type reference and process the expansion
// For now, we'll just process the type arguments
```

This could lead to incorrect results if the expansion of the type reference is important for correctness.

## 3. Simplified Record Type Handling

The Go implementation has a simplified version of record type handling compared to the Scala implementation, which lacks the special handling for variant type parameter fields present in the Scala code:

```scala
// Scala has special handling for variant type parameter fields
case RecordType(fields) => RecordType.mk(fields.flatMap { case (v @ Var(fnme), fty) =>
  val prefix = fnme.takeWhile(_ =/= '#')
  val postfix = fnme.drop(prefix.length + 1)
  // ... special handling based on variance ...
})
```

This simplification could lead to incorrect results in some cases involving record types with variant type parameters.

These bugs could cause the Go implementation to behave differently from the Scala reference implementation, particularly when dealing with extreme types in bounds and when processing built-in type references.

## Constraint Algorithm Differences

After comparing the constraint algorithms in the Go implementation (`constrain.go`) with the Scala reference implementation (`ConstraintSolver.scala`), I've identified several key differences that could affect how type variables are constrained:

### 1. DNF Handling and the `goToWork` Function

#### Scala Implementation
- In the Scala implementation, `goToWork` is a simple function that immediately calls `constrainDNF` with the DNF representations of the types:
  ```scala
  def goToWork(lhs: ST, rhs: ST)(implicit cctx: ConCtx): Unit =
    constrainDNF(DNF.mkDeep(lhs, true), DNF.mkDeep(rhs, false), rhs)
  ```
- The `constrainDNF` function handles each conjunct in the LHS DNF, with a clear strategy for handling variables and non-variable cases.
- The `annoying` function is more fully implemented, handling various type combinations in a pattern-matching style.

#### Go Implementation
- The Go implementation has a similar structure but with incomplete implementations:
  ```go
  func (cs *constraintSolver) goToWork(lhs, rhs SimpleType, cctx constraintContext, shadows *shadowsState) bool {
    opsDnf := &opsDNF{
      ctx:    cs.ctx,
      Logger: cs.Logger.With("section", "inference.constraintSolver"),
    }
    cs.constrainDNF(opsDnf, opsDnf.mkDeep(lhs, true), opsDnf.mkDeep(rhs, false), cctx, shadows)
    return false
  }
  ```
- The `constrainDNF` function in Go has a partial implementation that handles some cases but not all.
- The `annoying` function in Go is essentially a stub with minimal implementation.

### 2. Type Variable Handling

#### Scala Implementation
- Type variables are handled with a clear distinction between LHS and RHS positions:
  ```scala
  case (lhs: TypeVariable, rhs) if rhs.level <= lhs.level =>
    val newBound = (cctx._1 ::: cctx._2.reverse).foldRight(rhs)((c, ty) =>
      if (c.prov is noProv) ty else mkProxy(ty, c.prov))
    lhs.upperBounds ::= newBound // update the bound
    lhs.lowerBounds.foreach(rec(_, rhs, true)) // propagate from the bound
  ```
- The Scala implementation handles bounds propagation directly in the main constraint function.

#### Go Implementation
- The Go implementation separates type variable handling into dedicated functions:
  ```go
  func (cs *constraintSolver) constrainTypeVarLhs(lhs *typeVariable, rhs SimpleType, cctx constraintContext, shadows *shadowsState) bool {
    // Check levels
    if rhs.level() <= lhs.level() {
      return cs.addUpperBound(lhs, rhs, cctx)
    }
    // Extrusion needed for RHS
    extrudedRhs := cs.extrude(rhs, lhs.level(), false)
    return cs.rec(lhs, extrudedRhs, true, cctx, shadows)
  }
  ```
- The Go implementation has separate `addUpperBound` and `addLowerBound` functions.

### 3. Extrusion Implementation

#### Scala Implementation
- The Scala extrusion function is concise and handles all cases with pattern matching:
  ```scala
  def extrude(ty: SimpleType, lvl: Int, pol: Boolean)
      (implicit ctx: Ctx, cache: MutMap[PolarVariable, TV] = MutMap.empty): SimpleType =
    if (ty.level <= lvl) ty else ty match {
      case t @ TypeRange(lb, ub) => if (pol) extrude(ub, lvl, true) else extrude(lb, lvl, false)
      // ... other cases
    }
  ```
- The Scala implementation uses a cache based on a tuple of (variable, polarity).

#### Go Implementation
- The Go extrusion function is more verbose with explicit type assertions:
  ```go
  func (cs *constraintSolver) extrude(ty SimpleType, targetLvl level, pol bool) SimpleType {
    if ty.level() <= targetLvl {
      return ty
    }
    switch t := ty.(type) {
      case typeRange:
        if pol { return cs.extrude(t.upperBound, targetLvl, true) }
        return cs.extrude(t.lowerBound, targetLvl, false)
      // ... other cases
    }
  }
  ```
- The Go implementation uses a map-based cache with a custom key type.

### 4. Constraint Context Handling

#### Scala Implementation
- The Scala implementation uses a simple pair of lists for context:
  ```scala
  type ConCtx = Ls[SimpleType] -> Ls[SimpleType]
  ```
- Context is updated by prepending new types to the lists.

#### Go Implementation
- The Go implementation uses a struct with explicit fields:
  ```go
  type constraintContext struct {
    lhsChain []SimpleType
    rhsChain []SimpleType
    hash     uint64
  }
  ```
- The Go implementation includes a hash field for optimization.

### 5. Error Reporting

#### Scala Implementation
- The Scala implementation has a sophisticated error reporting system that builds detailed error messages with multiple hints.
- It collects information from the constraint context and provenances to provide rich error messages.

#### Go Implementation
- The Go implementation also has detailed error reporting but with a different structure.
- It builds `TypeStackHint` objects and uses them to construct error messages.

## Reconciliation Plan

To reconcile the differences and ensure the Go implementation produces the same added bounds on type variables as the Scala reference:

1. **Complete the DNF Handling**:
   - Fully implement the `annoying` function in Go to match the Scala behavior
   - Ensure `constrainDNF` handles all cases correctly, especially variable extraction and constraint propagation

2. **Align Type Variable Handling**:
   - Review and update the `addUpperBound` and `addLowerBound` functions to ensure they propagate constraints correctly
   - Verify that the bounds are added in the same order as in the Scala implementation

3. **Refine Extrusion Logic**:
   - Complete the extrusion implementation for all type cases
   - Ensure the extrusion cache works correctly for all scenarios
   - Fix the incomplete parts (like the commented-out `makeRecordType` call)

4. **Improve Shadow State Implementation**:
   - Implement proper cycle detection using the shadow state
   - Complete the `shadowsState` methods that are currently missing

5. **Test with Complex Cases**:
   - Create test cases that exercise the constraint algorithm with complex types
   - Compare the resulting bounds on type variables between the two implementations
   - Focus on cases involving DNF normalization and extrusion
