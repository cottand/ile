package types

import (
	"github.com/hashicorp/go-set/v3"
	"iter"
	"log/slog"
)

const factoriseApproximate = false

// pol is positive by default
func (ctx *TypeCtx) factorRecursiveTypes(typ SimpleType, pol *polarity) SimpleType {
	realPol := positive
	if pol != nil {
		realPol = *pol
	}

	ts := &factoriseState{
		ctx:              ctx,
		Logger:           ctx.logger.With("section", "factoriseRecursiveTypes"),
		processed:        set.New[TypeVarID](1),
		varSubstitutions: make(map[TypeVarID]*typeVariable),
	}

	// Iterate over the type variables and their polarities
	allVarPols := ctx.getVarsPolFor(typ, realPol)
	for tv, tvPol := range allVarPols {
		if tvPol == invariant {
			continue
		}
		bounds := tv.lowerBounds
		if tvPol == negative {
			bounds = tv.upperBounds
		}
		if len(bounds) != 1 {
			continue
		}

		// TODO do not generate seq twice
		bound1 := bounds[0]
		allVarPols2 := ctx.getVarsPolFor(typ, realPol)
		for tv2, tvPol2 := range allVarPols2 {
			ts.processTypeVars(bound1, tv, tv2, tvPol, tvPol2)
		}

		// TODO: Implement factoring logic using tv and tvPol
		_ = tv    // Avoid unused variable warning
		_ = tvPol // Avoid unused variable warning
	}

	// For now, just return the original type
	return typ
}

type factoriseState struct {
	ctx *TypeCtx
	*slog.Logger
	processed        set.Collection[TypeVarID]
	varSubstitutions map[TypeVarID]*typeVariable
}

func (fs *factoriseState) processTypeVars(bound1 SimpleType, tv1, tv2 *typeVariable, pol1, pol2 polarity) {
	if pol2 == invariant || pol1 != pol2 || tv1 != tv2 {
		return
	}
	if _, ok := fs.varSubstitutions[tv1.id]; ok {
		return
	}
	if _, ok := fs.varSubstitutions[tv2.id]; ok {
		return
	}
	bounds2 := tv2.lowerBounds
	if pol2 == negative {
		bounds2 = tv2.upperBounds
	}
	if len(bounds2) != 1 {
		return
	}
	bound2 := bounds2[0]
	if fs.unify(bound1, bound2) {
		fs.varSubstitutions[tv2.id] = tv1
	}
}

func (fs *factoriseState) unify(t1, t2 SimpleType) (ret bool) {
	defer func() {
		if !ret {
			fs.Debug("unify: cannot unify", "t1", t1, "t2", t2)
		}
	}()

	// Handle wrappingProvType by unwrapping
	if wpt, ok := t1.(wrappingProvType); ok {
		return fs.unify(wpt.SimpleType, t2)
	}
	if wpt, ok := t2.(wrappingProvType); ok {
		return fs.unify(t1, wpt.SimpleType)
	}

	// Handle type variables
	if tv1, ok1 := t1.(*typeVariable); ok1 {
		if tv2, ok2 := t2.(*typeVariable); ok2 {
			// If both are type variables, they unify if they're the same variable
			return tv1.id == tv2.id
		}
	}

	// Handle different type combinations
	switch v1 := t1.(type) {
	case negType:
		if v2, ok := t2.(negType); ok {
			return fs.unify(v1.negated, v2.negated)
		}
		return false

	case extremeType:
		if v2, ok := t2.(extremeType); ok {
			return v1.polarity == v2.polarity
		}
		return false

	case unionType:
		if v2, ok := t2.(unionType); ok {
			return fs.unify(v1.lhs, v2.lhs) && fs.unify(v1.rhs, v2.rhs)
		}
		return false

	case intersectionType:
		if v2, ok := t2.(intersectionType); ok {
			return fs.unify(v1.lhs, v2.lhs) && fs.unify(v1.rhs, v2.rhs)
		}
		return false

	case funcType:
		if v2, ok := t2.(funcType); ok {
			if len(v1.args) != len(v2.args) {
				return false
			}
			for i, arg1 := range v1.args {
				if !fs.unify(arg1, v2.args[i]) {
					return false
				}
			}
			return fs.unify(v1.ret, v2.ret)
		}
		return false

	case arrayType:
		if v2, ok := t2.(arrayType); ok {
			return fs.unify(v1.innerT, v2.innerT)
		}
		return false

	case tupleType:
		if v2, ok := t2.(tupleType); ok {
			if len(v1.fields) != len(v2.fields) {
				return false
			}
			for i, f1 := range v1.fields {
				if !fs.unify(f1, v2.fields[i]) {
					return false
				}
			}
			return true
		}
		return false

	case recordType:
		if v2, ok := t2.(recordType); ok {
			if len(v1.fields) != len(v2.fields) {
				return false
			}
			for i, f1 := range v1.fields {
				f2 := v2.fields[i]
				if f1.name.Name != f2.name.Name {
					return false
				}
				if !fs.unify(f1.type_.lowerBound, f2.type_.lowerBound) || !fs.unify(f1.type_.upperBound, f2.type_.upperBound) {
					return false
				}
			}
			return true
		}
		return false

	case typeRef:
		if v2, ok := t2.(typeRef); ok {
			if v1.defName != v2.defName || len(v1.typeArgs) != len(v2.typeArgs) {
				return false
			}
			for i, arg1 := range v1.typeArgs {
				if !fs.unify(arg1, v2.typeArgs[i]) {
					return false
				}
			}
			return true
		}
		return false

	case typeRange:
		if v2, ok := t2.(typeRange); ok {
			return fs.unify(v1.lowerBound, v2.lowerBound) && fs.unify(v1.upperBound, v2.upperBound)
		}
		return false

	case classTag:
		if v2, ok := t2.(classTag); ok {
			return v1.id.CanonicalSyntax() == v2.id.CanonicalSyntax()
		}
		return false

	case traitTag:
		if v2, ok := t2.(traitTag); ok {
			return v1.id.CanonicalSyntax() == v2.id.CanonicalSyntax()
		}
		return false
	}

	return false
}

// childrenPol returns the children of a SimpleType with their polarities
func childrenPol(st SimpleType, pol polarity) iter.Seq2[polarity, SimpleType] {
	return func(yield func(polarity, SimpleType) bool) {
		switch t := st.(type) {
		case *typeVariable:
			// For type variables, we don't yield any children with polarities
			return
		case funcType:
			// For function types, the arguments are in contravariant position (inverse polarity)
			// and the return type is in covariant position (same polarity)
			for _, arg := range t.args {
				if !yield(pol.inverse(), arg) {
					return
				}
			}
			yield(pol, t.ret)
		case unionType:
			// For union types, both children have the same polarity
			if !yield(pol, t.lhs) {
				return
			}
			yield(pol, t.rhs)
		case intersectionType:
			// For intersection types, both children have the same polarity
			if !yield(pol, t.lhs) {
				return
			}
			yield(pol, t.rhs)
		case negType:
			// For negation types, the child has the inverse polarity
			yield(pol.inverse(), t.negated)
		case extremeType:
			// Extreme types have no children
			return
		case typeRef:
			// For type references, all type arguments have the same polarity
			for _, arg := range t.typeArgs {
				if !yield(pol, arg) {
					break
				}
			}
		case recordType:
			// For record types, field lower bounds are contravariant and upper bounds are covariant
			for _, field := range t.fields {
				if !yield(pol.inverse(), field.type_.lowerBound) {
					break
				}
				if !yield(pol, field.type_.upperBound) {
					break
				}
			}
		case tupleType:
			// For tuple types, all elements have the same polarity
			for _, field := range t.fields {
				if !yield(pol, field) {
					break
				}
			}
		case arrayType:
			// For array types, the element type has the same polarity
			yield(pol, t.innerT)
		case wrappingProvType:
			// For wrapping provenance types, delegate to the underlying type
			yield(pol, t.SimpleType)
		default:
			// For other types, use the children method and assume the same polarity
			t.children(true)(func(child SimpleType) bool {
				return yield(pol, child)
			})
		}
	}
}

// getVarsPolFor returns an iterator of type variables and their polarities for all type variables in the given SimpleType
//  TODO should be ordered
func (ctx *TypeCtx) getVarsPolFor(st SimpleType, pol polarity) iter.Seq2[*typeVariable, polarity] {
	return func(yield func(*typeVariable, polarity) bool) {
		// Keep a local map to track what has been seen before
		seen := make(map[TypeVarID]polarity)

		// Use an iterative approach with a queue to avoid stack overflow
		queue := []struct {
			pol polarity
			typ SimpleType
		}{{pol, st}}

		for len(queue) > 0 {
			// Pop the first element from the queue
			current := queue[0]
			queue = queue[1:]

			// If the current type is a type variable, process it
			if tv, ok := current.typ.(*typeVariable); ok {
				existingPol, exists := seen[tv.id]

				if !exists {
					// If the type variable hasn't been seen before, add it to the result
					seen[tv.id] = current.pol
					// Yield the type variable and its polarity
					if !yield(tv, current.pol) {
						return
					}
					// Add its children to the queue
					for childPol, childTyp := range childrenPol(tv, current.pol) {
						queue = append(queue, struct {
							pol polarity
							typ SimpleType
						}{childPol, childTyp})
					}
				} else if existingPol == current.pol {
					// If the type variable has been seen before with the same polarity, just continue
					continue
				} else if existingPol == invariant {
					// If the type variable has been seen before with invariant polarity, just continue
					continue
				} else {
					// If the type variable has been seen before with a different polarity,
					// mark it as invariant and add its children to the queue
					seen[tv.id] = invariant
					// Yield the type variable with invariant polarity
					if !yield(tv, invariant) {
						return
					}
					for childPol, childTyp := range childrenPol(tv, invariant) {
						queue = append(queue, struct {
							pol polarity
							typ SimpleType
						}{childPol, childTyp})
					}
				}
			} else {
				// If the current type is not a type variable, add its children to the queue
				for childPol, childTyp := range childrenPol(current.typ, current.pol) {
					queue = append(queue, struct {
						pol polarity
						typ SimpleType
					}{childPol, childTyp})
				}
			}
		}
	}
}
