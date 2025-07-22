

# Correctness Differences Between Go and Scala Implementations

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