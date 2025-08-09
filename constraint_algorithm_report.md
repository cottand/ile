# Functional Differences Between Go and Scala Constraint Algorithms

After comparing the constraint algorithms in the Go implementation (`constrain.go`) with the Scala reference implementation (`ConstraintSolver.scala`), I've identified several key functional differences that would cause the two implementations to produce different results:

## 1. Differences in the `annoying()` Function Implementation

The `annoying()` function is critical for handling constraints involving unions, intersections, and negations. There are significant differences between the implementations:

### Go Implementation (constrain.go, lines 1075-1357)
- Handles various type constructs through a series of if-else statements
- Converts types to normal forms (lhsNF and rhsNF) in a centralized manner
- Uses a separate `checkTagCompatibility` function for tag compatibility checks
- Explicitly handles inheritance relationships in the tag compatibility function

### Scala Reference (ConstraintSolver.scala, lines 92-205)
- Split into `annoying()` and `annoyingImpl()` functions
- Uses pattern matching to handle different type constructs
- Tag compatibility checks are distributed across different parts of the code
- Inheritance relationship checks are spread across multiple functions

These structural differences lead to functional differences in how constraints are processed, particularly:

- **Different Type Handling Order**: The order in which types are processed differs, which can affect the final constraint set.
- **Incomplete Cases**: The Go implementation doesn't handle all the cases that the Scala implementation does, particularly in complex type combinations.
- **Different Normal Form Conversion**: The approaches to converting types to normal forms differ, which can affect how complex types are compared.

## 2. Tag Compatibility Handling Differences

The way tag compatibility is checked differs significantly between implementations:

### Go Implementation

The Go implementation uses a dedicated function for tag compatibility checks:

```go
// checkTagCompatibility performs tag compatibility checks similar to the Scala filter.
func checkTagCompatibility(lnf lhsNF, rnf rhsNF) bool {
    // Check 1: Trait tag conflict
    // If LHS has trait T and RHS has ~T, there's a conflict
    for _, rhsTag := range rhsBases.tags {
        if tt, ok := rhsTag.(traitTag); ok {
            if lhsRefined.traitTags.Contains(tt) {
                return false // Conflict: LHS has trait T, RHS has ~T
            }
        }
    }

    // Check 2: Class tag conflict
    // If LHS has class C and RHS has ~C, there's a conflict
    if lhsRefined.base != nil { // If LHS has a class tag
        for _, rhsTag := range rhsBases.tags {
            // Check inheritance relationship: if C <: D and RHS has ~D, there's a conflict
            if ct, ok := rhsTag.(classTag); ok && lhsRefined.base != nil {
                clsTag := *(lhsRefined.base)
                if clsTag.parents.Contains(ct.id.CanonicalSyntax()) {
                    return false // Conflict: LHS has class C, RHS has ~D, and C <: D
                }
            }
        }
    }
    return true // No conflicts found
}
```

### Scala Reference
Tag compatibility checks are distributed across different parts of the code:

```scala
// In the filter part (lines 64-71)
!vars.exists(r.nvars) && ((lnf & r.lnf)).isDefined && ((lnf, r.rnf) match {
  case (LhsRefined(_, _, _, ttags, _, _), RhsBases(objTags, rest, trs))
    if objTags.exists { case t: TraitTag => ttags(t); case _ => false }
    => false
  case (LhsRefined(S(ot: ClassTag), _, _, _, _, _), RhsBases(objTags, rest, trs))
    => !objTags.contains(ot)
  case _ => true
})

// In the annoying implementation (line 179)
if (pts.contains(pt) || pts.exists(p => pt.parentsST.contains(p.id)))
  println(s"OK  $pt  <:  ${pts.mkString(" | ")}")
else annoying(Nil, LhsRefined(N, ft, at, ts, r, trs), Nil, RhsBases(Nil, bf, trs2))

// In the rec implementation (line 259)
case (prim: ClassTag, ot: ObjectTag)
  if prim.parentsST.contains(ot.id) => ()
```

These differences in tag compatibility checking could lead to:

- **Different Inheritance Handling**: The Go implementation checks inheritance relationships in a centralized function, while Scala distributes these checks, potentially leading to different behavior in complex inheritance scenarios.
- **Different Conflict Detection**: The way conflicts are detected and reported differs, which could affect constraint solving outcomes.

## 2. Different Type Variable Bound Propagation

The way bounds are propagated differs between implementations:

- **Scala Implementation**: Immediately propagates constraints from existing bounds when a new bound is added:
  ```scala
  lhs.upperBounds ::= newBound // update the bound
  lhs.lowerBounds.foreach(rec(_, rhs, true)) // propagate from the bound
  ```

- **Go Implementation**: Has a similar propagation mechanism but implemented differently, which could lead to different order of constraint processing:
  ```
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
  ```
  // Use makeRecordType for potential sorting/simplification
  panic("extrude: implement makeRecordType")
  //return makeRecordType(newFields, &t.provenance)
  ```

- **Different Variance Handling**: The way variance is handled during extrusion of type references differs, which affects how type arguments are extruded.

These differences would cause the two implementations to produce different results when extruding complex types across polymorphism levels.

## 4. Missing Cycle Detection

The Go implementation lacks proper cycle detection:

- **Incomplete Shadow State**: The shadow state for cycle detection is defined but not fully implemented:
  ```
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
