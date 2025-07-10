# Understanding the `level` Variable in Ile's Type System

## Introduction

This report examines the role of the `level` variable in Ile's type system, particularly during the Disjunctive Normal Form (DNF) and constraint solving phases. The `level` variable is a fundamental component that helps track the nesting depth of scopes and plays a crucial role in type inference.

## Basic Definition

In the Ile codebase, `level` is defined as:

```go
type level uint16
```

This simple unsigned integer represents the "polymorphism level" or "scope depth" during type checking and inference.

## Role in Type Variables

Every type variable in the system has an associated level:

```go
func (t *Fresher) newTypeVariable(
    level level,
    prov typeProvenance,
    nameHint string,
    lowerBounds,
    upperBounds []SimpleType,
) *typeVariable {
    variable := &typeVariable{
        id:             t.freshCount,
        level_:         level,
        lowerBounds:    lowerBounds,
        upperBounds:    upperBounds,
        nameHint:       nameHint,
        withProvenance: prov.embed(),
    }
    t.freshCount++
    return variable
}
```

This level indicates at which scope depth the variable was created, which is essential for proper type generalization and instantiation.

## Role in DNF and Constraint Solving

### In Constraint Solving

During constraint solving, the `level` variable serves several purposes:

1. **Tracking Scope Depth**: The `constraintSolver` struct maintains a current level:

   ```go
   type constraintSolver struct {
       // ...
       level level  // Current polymorphism level during solving
       // ...
   }
   ```

2. **Creating Nested Scopes**: When entering a nested scope, the level is incremented:

   ```go
   func (cs *constraintSolver) withSubLevel(action func(subSolver *constraintSolver) bool) bool {
       subSolver := &constraintSolver{
           // ...
           level: cs.level + 1,  // Increment level
           // ...
       }
       // ...
   }
   ```

3. **Extrusion**: The `extrude` function uses levels to determine which type variables need to be "lifted" to a higher scope:

   ```go
   func (cs *constraintSolver) extrude(ty SimpleType, targetLvl level, pol bool) SimpleType {
       if ty.level() <= targetLvl {
           return ty
       }
       // ... extrude the type ...
   }
   ```

   This is crucial for maintaining proper scope boundaries during type inference.

### In Type Inference

During type inference, levels help manage the scope of type variables:

1. **Caching**: Types are cached at their current level:

   ```go
   if cached, ok := ctx.cache.getCached(expr); ok && cached.at == ctx.level {
       // ...
       return cached.t
   }
   ```

2. **Instantiation**: When referencing a variable, its type is instantiated at the current level:

   ```go
   instance := known.instantiate(ctx.fresher, ctx.level)
   ```

3. **Creating New Scopes**: When entering a new scope (e.g., for let bindings), the level is incremented:

   ```go
   valueResult = ctx.nextLevel().TypeExpr(expr.Value, vars)
   ```

   ```go
   bindingVars[i] = ctx.fresher.newTypeVariable(ctx.level+1, typeProvenance{}, binding.Var, nil, nil)
   ```

## Distinguishing Separate Scopes at the Same Depth

One limitation of using just a `level` variable is that it cannot distinguish between separate scopes at the same depth. For example, two sibling let-bindings would have the same level, but they represent different scopes.

The codebase acknowledges this limitation with a TODO comment:

```go
// TODO ideally we store not just level info, but also a hash of scope of the variable,
// so that two variables at the same level in different scopes do not hit this case
// (but two variables in the same scope at the same level do get a cache hit)
```

This suggests that the current implementation cannot reliably distinguish between separate scopes at the same depth. A more complete solution would involve tracking not just the level but also some identifier for the scope itself, such as a hash of the scope's variables or a unique scope ID.

## Conclusion

The `level` variable in Ile's type system serves as a way to track the nesting depth of scopes during type inference and constraint solving. It helps maintain proper scope boundaries for type variables and is essential for features like polymorphism. However, it has limitations when it comes to distinguishing between separate scopes at the same depth, which would require additional tracking mechanisms beyond just a level number.

This limitation is acknowledged in the codebase, and a potential solution would be to supplement the level with additional scope identification information. This would allow the type system to distinguish between variables at the same level but in different scopes, improving the precision of type inference and caching.