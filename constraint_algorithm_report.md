# Functional Differences Between Go and Scala Constraint Algorithms

After comparing the constraint algorithms in the Go implementation (`constrain.go`) with the Scala reference implementation (`ConstraintSolver.scala`), I've identified several key functional differences that would cause the two implementations to produce different results:

## 1. Incomplete DNF Handling in Go

The Go implementation has incomplete handling of Disjunctive Normal Form (DNF) constraints:

- **Missing Cases**: The Go `constrainDNF` function doesn't handle all the cases that the Scala implementation does, particularly in complex type combinations.

- **Stub Implementation**: The `annoying` function in Go is essentially a stub with minimal implementation, while the Scala version fully handles various type combinations that require normalization.

This functional difference means that complex constraints involving unions, intersections, and negations will not be properly resolved in the Go implementation, leading to different bounds on type variables.

## 2. Different Type Variable Bound Propagation

The way bounds are propagated differs between implementations:

- **Scala Implementation**: Immediately propagates constraints from existing bounds when a new bound is added:
  ```scala
  lhs.upperBounds ::= newBound // update the bound
  lhs.lowerBounds.foreach(rec(_, rhs, true)) // propagate from the bound
  ```

- **Go Implementation**: Has a similar propagation mechanism but implemented differently, which could lead to different order of constraint processing:
  ```go
  // Propagate constraints: L <: new_rhs for all L in lowerBounds
  for _, lb := range tv.lowerBounds {
    if cs.rec(lb, rhs, true, cctx, nil) {
      return true // Terminate early if propagation fails
    }
  }
  ```

The order and manner of constraint propagation can affect the final set of bounds, especially in complex recursive types.

## 3. Incomplete Extrusion Implementation

Extrusion (copying a type up to its type variables of wrong level) has functional differences:

- **Missing Type Cases**: The Go implementation has incomplete handling for some type cases, with a notable example being the `makeRecordType` call that's commented out:
  ```go
  // Use makeRecordType for potential sorting/simplification
  panic("extrude: implement makeRecordType")
  //return makeRecordType(newFields, &t.provenance)
  ```

- **Different Variance Handling**: The way variance is handled during extrusion of type references differs, which affects how type arguments are extruded.

These differences would cause the two implementations to produce different results when extruding complex types across polymorphism levels.

## 4. Missing Cycle Detection

The Go implementation lacks proper cycle detection:

- **Incomplete Shadow State**: The shadow state for cycle detection is defined but not fully implemented:
  ```go
  // TODO: Implement methods for shadowsState: detectCycle, addConstraint, resetCurrent etc.
  ```

- **Missing Cycle Checks**: The Scala implementation has cycle detection logic that prevents infinite recursion in constraint solving, while the Go implementation relies primarily on a fuel/depth mechanism.

This difference could cause the Go implementation to either fail to terminate on certain recursive types or to terminate early without fully exploring the constraint space.

## Reconciliation Plan

To ensure the Go implementation produces the same added bounds on type variables as the Scala reference:

1. **Complete the DNF Handling**:
   - Fully implement the `annoying` function in Go to match the Scala behavior
   - Ensure `constrainDNF` handles all cases correctly, especially variable extraction and constraint propagation

2. **Align Type Variable Handling**:
   - Review and update the `addUpperBound` and `addLowerBound` functions to ensure they propagate constraints correctly
   - Verify that the bounds are added in the same order as in the Scala implementation

3. **Complete the Extrusion Implementation**:
   - Implement all missing type cases in the extrusion function
   - Fix the incomplete parts (like the commented-out `makeRecordType` call)
   - Ensure variance is handled correctly for type references

4. **Implement Proper Cycle Detection**:
   - Complete the shadow state implementation for cycle detection
   - Add proper cycle checks in the constraint algorithm

5. **Test with Complex Cases**:
   - Create test cases that exercise the constraint algorithm with complex types
   - Compare the resulting bounds on type variables between the two implementations
   - Focus on cases involving DNF normalization, extrusion, and recursive types
