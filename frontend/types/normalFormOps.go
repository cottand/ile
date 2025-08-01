package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"go/token"
	"log/slog"
	"slices"
)

// opsDNF corresponds to the DNF companion object in the scala reference
// We add operations between dnf and cnf here instead of as methods of those structs so that we can
// store here their constructors (like mk) and the TypeCtx.
type opsDNF struct {
	ctx *TypeCtx
	// preserveTypeRefs controls whether TypeRefs are expanded during normalization
	preserveTypeRefs bool
	*slog.Logger
}

func newOpsDNF(ctx *TypeCtx, preserveTypeRefs bool) *opsDNF {
	return &opsDNF{ctx: ctx, preserveTypeRefs: preserveTypeRefs, Logger: logger.With("section", "inference.DNF")}
}

// --- DNF Methods (Corresponds to methods on DNF class in Scala) ---

// and computes the intersection of two DNFs (this & other).
// DNF represents a union, so intersection distributes: (A | B) & (C | D) = (A&C) | (A&D) | (B&C) | (B&D)
func (o *opsDNF) and(d dnf, other dnf) dnf {
	result := o.extr(false)
	for _, c2 := range other {
		merged := o.andConjunct(d, c2)
		result = o.or(merged, result)
	}
	return result
}

// or computes the union of two DNFs (this | other).
func (o *opsDNF) or(left, right dnf) dnf {
	// Fold `rhs` into `lhs` using `orConjunct` for simplification.
	result := left
	for _, c := range right {
		result = o.orConjunct(result, c)
	}
	return result
}

// andConjunct computes the intersection of a DNF and a single Conjunct (this & c).
func (o *opsDNF) andConjunct(d dnf, c conjunct) dnf {
	resultConjuncts := make([]conjunct, 0, len(d))
	for _, dc := range d {
		if merged, ok := o.conjunctAndConjunct(dc, c); ok {
			resultConjuncts = append(resultConjuncts, merged)
		}
	}
	return resultConjuncts
}

func (o *opsDNF) conjunctOrConjunct(left, right conjunct) *conjunct {
	panic(fmt.Sprintf("opsDNF.conjunctOrConjunct: not implemented for conjuncts %v and %v", left, right))
}

func (o *opsDNF) conjunctAndConjunct(left, right conjunct) (conjunct, bool) {
	if o.ctx.isSubtype(left.toType(), right.toType(), nil) {
		return conjunct{}, false
	}

	// Try to merge the LHS components
	mergedLhs, ok := left.lhs.and(right.lhs, o.ctx)
	if !ok {
		return conjunct{}, false // Cannot merge LHS, intersection is empty
	}

	// Create sets for the merged variables
	// For AND operation, we take the union of all variables
	mergedVars := left.vars.Union(right.vars)
	mergedNvars := left.nvars.Union(right.nvars)

	// Merge the RHS components
	mergedRhs, ok := o.toCNF().rhsOrRhs(left.rhs, right.rhs)
	if !ok {
		return conjunct{}, false
	}

	// Create the merged conjunct
	return o.simplifyConjunct(newConjunct(mergedLhs, mergedRhs, mergedVars, mergedNvars))
}

// orConjunct computes the union of a DNF and a single Conjunct (this | c).
// This involves checking for subsumption and trying to merge conjuncts.
func (o *opsDNF) orConjunct(d dnf, c conjunct) (ret dnf) {
	acc := make([]conjunct, 0, len(d)+1)
	toMerge := c

	defer func() {
		slices.SortFunc(ret, util.ComparingHashable)
	}()

	for i, c := range d {
		if c.lessThanOrEqual(toMerge) {
			remaining := d[i+1:]
			ret = append(acc, toMerge)
			ret = append(ret, remaining...)

			return ret
		}
		if toMerge.lessThanOrEqual(c) {
			remainingWithExistingC := d[i:]
			return append(acc, remainingWithExistingC...)
		}

		if mergedC, ok := o.tryMergeUnion(c, toMerge); ok {
			remaining := d[i+1:]
			ret = append(acc, mergedC)
			ret = append(ret, remaining...)
			return ret
		}

		// cannot merge, so we keep looking
		acc = append(acc, c)
	}

	return append(acc, toMerge)
}

func (o *opsDNF) tryMergeIntersection(left, right rhsNF) (rhsNF, bool) {
	panic(fmt.Sprintf("opsDNF.tryMergeIntersection: not implemented"))
}

// tryMergeUnion attempts to merge two conjuncts in a union
func (o *opsDNF) tryMergeUnion(left, right conjunct) (ret conjunct, ok bool) {
	if left.lessThanOrEqual(right) {
		return right, true
	}
	if right.lessThanOrEqual(left) {
		return left, true
	}

	// Case 1: If both conjuncts have the same variable sets
	if left.vars.EqualSet(right.vars) && left.nvars.EqualSet(right.nvars) && left.lhs.isLeftNfTop() && right.lhs.isLeftNfTop() {
		if left.lhs.isLeftNfTop() && right.lhs.isLeftNfTop() {
			inter, ok := o.tryMergeIntersection(left.rhs, right.rhs)
			if ok {
				return newConjunct(lhsTop{}, inter, left.vars, left.nvars), true
			}
		}

	}

	leftRefined, ok := left.lhs.(*lhsRefined)
	if !ok {
		return ret, false
	}
	rightRefined, ok := right.lhs.(*lhsRefined)
	if !ok {
		return ret, false
	}

	equalTypeRefs := slices.EqualFunc(leftRefined.typeRefs, rightRefined.typeRefs, Equal)
	if leftRefined.base == nil ||
		rightRefined.base == nil ||
		!equalTypeRefs ||
		!Equal(leftRefined.base, rightRefined.base) ||
		!leftRefined.traitTags.EqualSet(rightRefined.traitTags) ||
		!left.vars.EqualSet(right.vars) ||
		!left.nvars.EqualSet(right.nvars) ||
		!left.rhs.equal(right.rhs) {
		return ret, false
	}

	// Case2: both are leftRefined with the same base types. Let's try to merge them

	fnType := leftRefined.fn
	if !Equal(leftRefined.fn, rightRefined.fn) {
		if leftRefined.fn == nil || rightRefined.fn == nil || len(leftRefined.fn.args) != len(rightRefined.fn.args) {
			return ret, false
		}
		args := make([]SimpleType, len(leftRefined.fn.args))
		for i, arg := range leftRefined.fn.args {
			args[i] = intersectionOf(arg, rightRefined.fn.args[i], unionOpts{})
		}
		fnType = &funcType{
			args: args,
			ret:  unionOf(leftRefined.fn.ret, rightRefined.fn.ret, unionOpts{}),
		}
	}

	arrType := leftRefined.arr
	if !Equal(leftRefined.arr, rightRefined.arr) {
		o.Warn("DNF.tryMergeUnion: merging arrays not implemented", "left", left, "right", right)
		return ret, false
	}

	typeRefs := leftRefined.typeRefs
	if !slices.EqualFunc(typeRefs, rightRefined.typeRefs, Equal) {
		typeRefs = mergeTypeRefs(typeRefs, rightRefined.typeRefs)
	}

	recType := recordType{
		fields: recordUnionOf(leftRefined.reft.fields, rightRefined.reft.fields),
	}

	return newConjunct(
		&lhsRefined{
			base:      leftRefined.base,
			fn:        fnType,
			arr:       arrType,
			traitTags: leftRefined.traitTags,
			reft:      recType,
			typeRefs:  typeRefs,
		},
		left.rhs,
		left.vars,
		left.nvars,
	), true
}

// Helper function to check if two sets are disjoint
func isDisjoint(set1, set2 set.Collection[*typeVariable]) bool {
	for elem := range set1.Items() {
		if set2.Contains(elem) {
			return false
		}
	}
	return true
}

// isBot checks if the DNF represents Bottom (empty disjunction).
func (d dnf) isBot() bool {
	return len(d) == 0
}

// top returns a DNF representing Top.
func (o *opsDNF) top() dnf {
	return dnf{newConjunct(lhsTop{}, nil, nil, nil)}
}

// --- opsDNF Methods (Corresponds to methods on DNF companion object in Scala) ---

// of creates a DNF from a single LhsNf component.
func (o *opsDNF) of(lnf lhsNF) dnf {
	return dnf{newConjunct(lnf, rhsBot{}, nil, nil)}
}

// extr returns the extremal DNF (Top or Bottom).
// pol=true means Top, pol=false means Bottom.
func (o *opsDNF) extr(pol bool) dnf {
	if pol {
		return o.of(lhsTop{})
	}
	return dnf{} // Empty DNF represents Bottom
}

// merge combines two DNFs based on polarity.
// pol=true means union (|), pol=false means intersection (&).
func (o *opsDNF) merge(pol bool, l, r dnf) dnf {
	if pol {
		return o.or(l, r)
	}
	return o.and(l, r)
}

// mk creates a DNF representation of a SimpleType.
// pol=true means positive polarity (interpret unions as unions, intersections as intersections).
// pol=false means negative polarity (interpret unions as intersections, intersections as unions - effectively CNF).
func (o *opsDNF) mk(ty SimpleType, pol bool) (ret dnf) {
	ty = unwrapProvenance(ty) // Work with the underlying type
	defer func() {
		o.Debug("DNF.mk", "type", ty, "polarity", pol, "result", ret)
	}()

	switch t := ty.(type) {
	case funcType:
		return o.of(&lhsRefined{fn: &t, reft: emptyRecord})
	case arrayBase: // Covers tupleType, arrayType, namedTupleType
		return o.of(&lhsRefined{arr: t, reft: emptyRecord})
	case classTag:
		return o.of(&lhsRefined{base: &t, reft: emptyRecord})
	case traitTag:
		traitSet := set.TreeSetFrom([]traitTag{t}, func(a, b traitTag) int {
			// Use Compare method if available and reliable, otherwise hash or string comparison
			// return a.Compare(b)
			return cmp.Compare(a.Hash(), b.Hash()) // Fallback using hash
		})
		return o.of(&lhsRefined{traitTags: traitSet, reft: emptyRecord})
	case recordType:
		return o.of(&lhsRefined{reft: t})
	case extremeType:
		return o.extr(!t.polarity)
	case unionType:
		return o.merge(true, o.mk(t.lhs, pol), o.mk(t.rhs, pol))
	case intersectionType:
		return o.merge(false, o.mk(t.lhs, pol), o.mk(t.rhs, pol))
	case negType:
		// Negation flips the polarity for the inner type and then negates the result.
		// DNF(~A, pol) = negate(CNF(A, !pol))
		cnfResult := o.toCNF().mk(t.negated, !pol)
		conjuncts := make([]conjunct, len(cnfResult))
		for i, disj := range cnfResult {
			conjuncts[i] = disj.negate() // Negate each disjunct to get a conjunct
		}
		return conjuncts // Return the resulting DNF
	case *typeVariable:
		vars := set.TreeSetFrom([]*typeVariable{t}, compareTypeVars)
		return dnf{newConjunct(lhsTop{}, rhsBot{}, vars, nil)}
	case typeRef:
		if o.preserveTypeRefs && !isPrimitiveTypeName(t.defName) { // TODO: Refine primitive check
			// Represent as LhsRefined with the TypeRef
			refs := []typeRef{t}
			var baseTag *classTag

			// TODO: Add mkTag logic if needed here, similar to Scala
			// if tag := t.mkTag(o.simplifier); tag != nil { baseTag = tag }
			return o.of(&lhsRefined{base: baseTag, typeRefs: refs, reft: emptyRecord})
		}
		// Expand the TypeRef and create DNF from the expansion
		return o.mk(o.ctx.expand(t, expandOpts{}), pol) // Assuming simplifier.expand exists
	case typeRange:
		// In positive context, use upper bound. In negative, use lower bound.
		boundToUse := t.upperBound
		if !pol {
			boundToUse = t.lowerBound
		}
		return o.mk(boundToUse, pol)
	case *PolymorphicType:
		// This case might indicate an issue if polymorphic types are expected
		// to be instantiated before normalization. Handle by normalizing the body.
		logger.Warn("Normalizing polymorphic type body directly", "type", t)
		return o.mk(t.Body, pol)
	default:
		o.ctx.addFailure(fmt.Sprintf("DNF.mk: unhandled type %T", ty), ty.prov())
		return o.extr(pol)
	}
}

// mapPolRecursive applies a function `fn` to the children of a type `ty`,
// adjusting the polarity according to the type structure.
// It's used for implementing deep normalization and other recursive transformations.
func (o *opsDNF) mapPolRecursive(ty SimpleType, pol polarity, fn func(pol polarity, child SimpleType) SimpleType) SimpleType {
	originalProv := ty.prov()
	ty = unwrapProvenance(ty) // Work with the underlying type

	switch t := ty.(type) {
	case extremeType, *typeVariable:
		// Base cases: No children to map
		return t

	case unionType:
		newLhs := o.mapPolRecursive(t.lhs, pol, fn)
		newRhs := o.mapPolRecursive(t.rhs, pol, fn)
		if newLhs == t.lhs && newRhs == t.rhs {
			return t // Optimization
		}
		// Use unionOf for potential simplification
		return unionOf(newLhs, newRhs, unionOpts{prov: originalProv})

	case intersectionType:
		newLhs := o.mapPolRecursive(t.lhs, pol, fn)
		newRhs := o.mapPolRecursive(t.rhs, pol, fn)
		// Use intersectionOf for potential simplification
		return intersectionOf(newLhs, newRhs, unionOpts{prov: originalProv})

	case negType:
		newNegated := o.mapPolRecursive(t.negated, pol, fn) // Polarity doesn't flip for the negation itself
		if newNegated == t.negated {
			return t // Optimization
		}
		// Use negateType for potential simplification
		return negateType(newNegated, originalProv)

	case funcType:
		newArgs := make([]SimpleType, len(t.args))
		argsChanged := false
		for i, arg := range t.args {
			newArgs[i] = o.mapPolRecursive(arg, pol.inverse(), fn) // Contravariant args
			if newArgs[i] != arg {
				argsChanged = true
			}
		}
		newRet := o.mapPolRecursive(t.ret, pol, fn) // Covariant return
		if !argsChanged && newRet == t.ret {
			return t // Optimization
		}
		return funcType{args: newArgs, ret: newRet, withProvenance: withProvenance{originalProv}}

	case recordType:
		newFields := make([]recordField, len(t.fields))
		changed := false
		for i, field := range t.fields {
			newLb := o.mapPolRecursive(field.type_.lowerBound, pol.inverse(), fn) // Contravariant lower bound
			newUb := o.mapPolRecursive(field.type_.upperBound, pol, fn)           // Covariant upper bound
			if newLb != field.type_.lowerBound || newUb != field.type_.upperBound {
				changed = true
			}
			newFields[i] = recordField{
				name: field.name,
				// Preserve original field provenance
				type_: fieldType{lowerBound: newLb, upperBound: newUb, withProvenance: field.type_.withProvenance},
			}
		}
		if !changed {
			return t
		} // Optimization: return original if no change
		// TODO: Use makeRecordType if it exists for sorting/simplification
		return recordType{fields: newFields, withProvenance: withProvenance{originalProv}}

	case tupleType:
		newFields := make([]SimpleType, len(t.fields))
		changed := false
		for i, field := range t.fields {
			newFields[i] = o.mapPolRecursive(field, pol, fn) // Covariant fields
			if newFields[i] != field {
				changed = true
			}
		}
		if !changed {
			return t
		}
		return tupleType{fields: newFields, withProvenance: withProvenance{originalProv}}

	case namedTupleType:
		newFields := make([]util.Pair[ir.Var, SimpleType], len(t.fields))
		changed := false
		for i, field := range t.fields {
			newFieldSnd := o.mapPolRecursive(field.Snd, pol, fn) // Covariant fields
			if newFieldSnd != field.Snd {
				changed = true
			}
			newFields[i] = util.Pair[ir.Var, SimpleType]{Fst: field.Fst, Snd: newFieldSnd}
		}
		if !changed {
			return t
		}
		return namedTupleType{fields: newFields, withProvenance: withProvenance{originalProv}}

	case arrayType:
		newInner := o.mapPolRecursive(t.innerT, pol, fn) // Covariant inner type
		if newInner == t.innerT {
			return t
		}
		return arrayType{innerT: newInner, withProvenance: withProvenance{originalProv}}

	case typeRef:
		newTargs := make([]SimpleType, len(t.typeArgs))
		changed := false
		idx := 0

		// Use forEachTypeArg to handle variance correctly
		for argPol, arg := range o.ctx.typeRefTraverseTypeArguments(t, pol) {
			if idx >= len(newTargs) { // Should not happen if forEachTypeArg is correct
				panic("forEachTypeArg yielded more arguments than expected")
			}
			newTargs[idx] = fn(argPol, arg) // Apply the provided function `fn`
			if newTargs[idx] != arg {
				changed = true
			}
			idx++
		}
		if idx != len(t.typeArgs) { // Should not happen
			panic("forEachTypeArg yielded fewer arguments than expected")
		}
		if !changed {
			return t
		}
		return typeRef{defName: t.defName, typeArgs: newTargs, withProvenance: withProvenance{originalProv}}

	case typeRange:
		newLb := o.mapPolRecursive(t.lowerBound, pol.inverse(), fn) // Contravariant lower bound
		newUb := o.mapPolRecursive(t.upperBound, pol, fn)           // Covariant upper bound
		if newLb == t.lowerBound && newUb == t.upperBound {
			return t
		}
		// Use makeTypeRange for potential simplification
		return o.ctx.makeTypeRange(newLb, newUb, originalProv)

	case wrappingProvType: // Should have been unwrapped, but handle defensively
		newUnderlying := o.mapPolRecursive(t.underlying(), pol, fn)
		if newUnderlying == t.underlying() {
			return t
		}
		// Re-wrap with the original wrapper's provenance
		return wrappingProvType{SimpleType: newUnderlying, proxyProvenance: t.prov()}

	case *PolymorphicType:
		// This case should ideally not be hit if types are instantiated first.
		logger.Warn("mapPolRecursive encountered PolymorphicType", "type", t)
		newBody := o.mapPolRecursive(t.Body, pol, fn)
		if newBody == t.Body {
			return t
		}
		// Return the mapped body, losing the polymorphism. This matches Scala's mapPol behavior.
		return newBody
	case classTag:
		return ty

	default:
		panic(fmt.Sprintf("mapPolRecursive: unhandled type %T", ty))
	}
}

// mkDeep recursively normalizes a type into DNF form.
func (o *opsDNF) mkDeep(ty SimpleType, pol bool) dnf {
	deepST := o.mkDeepST(ty, pol)
	return o.mk(deepST, pol)
}

// mkDeepST performs the recursive traversal for mkDeep.
// It normalizes the type and then recursively normalizes its children based on polarity.
func (o *opsDNF) mkDeepST(ty SimpleType, pol bool) SimpleType {
	originalProv := ty.prov() // Store original provenance

	// Handle provenance wrappers first
	if wrapper, ok := ty.(wrappingProvType); ok {
		// Recurse on the underlying type and re-wrap with the wrapper's provenance
		deepUnderlying := o.mkDeepST(wrapper.underlying(), pol)
		// Check if the result is already the same to avoid unnecessary wrapping
		if deepUnderlying == wrapper.underlying() {
			return wrapper // Return original wrapper if underlying didn't change
		}
		// Ensure the new wrapper uses the *wrapper's* provenance, not the original input's
		return wrappingProvType{SimpleType: deepUnderlying, proxyProvenance: wrapper.prov()}
	}

	// Handle the base type itself
	switch t := ty.(type) {
	case typeRange:
		// Normalize the relevant bound based on polarity
		boundToNormalize := t.upperBound
		if !pol {
			boundToNormalize = t.lowerBound
		}
		// Return the deeply normalized bound, potentially wrapped in the original TypeRange provenance
		deepBound := o.mkDeepST(boundToNormalize, pol)

		// If the bound didn't change, return the original range
		if deepBound == boundToNormalize {
			return t
		}

		// If the structure changed, wrap the result in the original provenance
		// Check if deepBound already has the correct provenance
		if deepBound.prov() == originalProv {
			return deepBound
		}
		return wrappingProvType{SimpleType: deepBound, proxyProvenance: originalProv}

	default:
		// 1. Normalize the current type `t` (using the receiver's preserveTypeRefs setting)
		dnfResult := o.mk(t, pol)
		// 2. Convert back to type
		normalizedTy := dnfResult.toType() // This type might have a different structure and provenance

		// 3. Define the recursive 'go' function for deep traversal
		// Create a new opsDNF instance configured to preserve type refs for deep recursion
		opsDeep := newOpsDNF(o.ctx, true) // Corresponds to ptr = true in Scala recursive calls

		var go_ func(childPol polarity, child SimpleType) SimpleType
		go_ = func(childPol polarity, child SimpleType) SimpleType {
			// Use Equivalent to handle potential cycles involving ProvType wrappers etc.
			// Compare with the *original* unwrapped type `t`
			if Equal(child, t) {
				// Recursive step: Apply mapPol again to handle cycles/nested structures
				// Use the *original* opsDNF `o` here, as mapPol itself doesn't change preserveTypeRefs setting
				return o.mapPolRecursive(child, childPol, go_)
			}
			// Base case for recursion: Apply mkDeepST using opsDeep
			switch childPol {
			case positive:
				return opsDeep.mkDeepST(child, true)
			case negative:
				return opsDeep.mkDeepST(child, false)
			case invariant:
				lb := opsDeep.mkDeepST(child, false)
				ub := opsDeep.mkDeepST(child, true)
				// Use child's provenance for the new range
				return o.ctx.makeTypeRange(lb, ub, child.prov())
			default:
				panic(fmt.Sprintf("mkDeepST.go: unexpected polarity: %v", childPol))
			}
		}

		// 4. Apply mapPolRecursive to the children of the normalized type
		// Use the *original* opsDNF `o` for the mapPol call itself.
		result := o.mapPolRecursive(normalizedTy, polarityFromBool(pol), go_)

		// 5. Re-wrap with original top-level provenance if necessary.
		// Check if the final result's provenance matches the original input `ty`'s provenance.
		if result.prov() != originalProv && originalProv.Pos() != token.NoPos {
			// Avoid wrapping if the result is already a wrapper with the correct provenance
			if wrapper, ok := result.(wrappingProvType); ok && wrapper.prov() == originalProv {
				return result
			}
			return wrappingProvType{SimpleType: result, proxyProvenance: originalProv}
		}
		return result
	}
}

// Helper to check for primitive type names (adjust as needed)
func isPrimitiveTypeName(name typeName) bool {
	switch name {
	case "int", "number", "bool", "true", "false", "string", "any", "nothing", "error", "nil", "Array": // Added Array
		return true
	default:
		return false
	}
}

// --- CNF Operations ---

type opsCNF struct {
	ctx              *TypeCtx
	preserveTypeRefs bool
}

func newOpsCNF(ctx *TypeCtx, preserveTypeRefs bool) *opsCNF {
	return &opsCNF{ctx: ctx, preserveTypeRefs: preserveTypeRefs}
}

func (o *opsCNF) toDNF() *opsDNF {
	return newOpsDNF(o.ctx, o.preserveTypeRefs)
}
func (o *opsDNF) toCNF() *opsCNF {
	return newOpsCNF(o.ctx, o.preserveTypeRefs)
}

// mk creates a CNF representation of a SimpleType.
// pol=true means positive polarity (interpret unions as unions, intersections as intersections).
// pol=false means negative polarity (interpret unions as intersections, intersections as unions).
func (o *opsCNF) mk(ty SimpleType, pol bool) cnf {
	ty = unwrapProvenance(ty)

	switch t := ty.(type) {
	case funcType, arrayBase, classTag, traitTag:
		// Basic types form a single disjunct in CNF. Need RhsNf representation.
		rnf := o.mkRhsNf(t)
		if rnf == nil { // If mkRhsNf returns nil (e.g., for Top), CNF is Top (empty list)
			return o.extr(true)
		}
		return cnf{newDisjunct(lhsTop{}, rnf, nil, nil)}

	case recordType:
		// RecordType {f: T, g: U} becomes CNF: ({f: T}) & ({g: U})
		disjuncts := make([]disjunct, 0, len(t.fields))
		for _, field := range t.fields {
			rnfField := &rhsField{ty: field.type_, name: field.name}
			disjuncts = append(disjuncts, newDisjunct(lhsTop{}, rnfField, nil, nil))
		}
		return disjuncts

	case extremeType:
		return o.extr(!t.polarity) // CNF(Top) = Top (empty), CNF(Bot) = Bot (RhsBot disjunct)

	case unionType:
		// CNF(A | B, pol) = merge(true, CNF(A, pol), CNF(B, pol))
		return o.merge(true, o.mk(t.lhs, pol), o.mk(t.rhs, pol))

	case intersectionType:
		// CNF(A & B, pol) = merge(false, CNF(A, pol), CNF(B, pol))
		return o.merge(false, o.mk(t.lhs, pol), o.mk(t.rhs, pol))

	case negType:
		// CNF(~A, pol) = negate(DNF(A, !pol))
		dnfResult := o.toDNF().mk(t.negated, !pol)
		disjuncts := make([]disjunct, len(dnfResult))
		for i, conj := range dnfResult {
			disjuncts[i] = conj.negate() // Negate each conjunct to get a disjunct
		}
		return disjuncts

	case *typeVariable:
		vars := set.TreeSetFrom([]*typeVariable{t}, compareTypeVars)
		return cnf{newDisjunct(lhsTop{}, rhsBot{}, vars, nil)} // Represents variable `t`

	case typeRef:
		if o.preserveTypeRefs && !isPrimitiveTypeName(t.defName) {
			// Represent as RhsBases with the TypeRef
			refs := []typeRef{t}
			// TODO: Add tag logic if needed
			rnf := &rhsBases{typeRefs: refs}
			return cnf{newDisjunct(lhsTop{}, rnf, nil, nil)}
		}
		// Expand and create CNF
		return o.mk(o.ctx.expand(t, expandOpts{}), pol)

	case typeRange:
		boundToUse := t.upperBound
		if !pol {
			boundToUse = t.lowerBound
		}
		return o.mk(boundToUse, pol)

	case *PolymorphicType:
		logger.Warn("Normalizing polymorphic type body directly for CNF", "type", t)
		return o.mk(t.Body, pol)

	default:
		panic(fmt.Sprintf("opsCNF.mk: unhandled type %T", ty))
	}
}

// mkRhsNf converts a basic SimpleType into its RhsNf representation.
// Returns nil if the type corresponds to Top in RhsNf.
func (o *opsCNF) mkRhsNf(ty SimpleType) rhsNF {
	switch t := ty.(type) {
	case objectTag: // classTag or traitTag
		return &rhsBases{tags: []objectTag{t}}
	case funcType:
		return &rhsBases{rest: t}
	case tupleType:
		return &rhsBases{rest: t}
	case arrayType:
		return &rhsBases{rest: t}
	case namedTupleType:
		return &rhsBases{rest: t}
	// recordType is handled in mk directly
	// extremeType is handled in mk directly
	// Other types (composed, neg, var, ref, range, poly) are handled in mk
	default:
		panic(fmt.Sprintf("opsCNF.mkRhsNf: unhandled type %T", ty))
	}
}

// extr returns the extremal CNF (Top or Bottom).
// pol=true means Top (empty CNF), pol=false means Bottom (CNF with RhsBot).
func (o *opsCNF) extr(pol bool) cnf {
	if pol {
		return cnf{} // Empty CNF represents Top
	}
	// CNF with a single disjunct representing Bottom
	return cnf{newDisjunct(lhsTop{}, rhsBot{}, nil, nil)}
}

// merge combines two CNFs based on polarity.
// pol=true means union (|), pol=false means intersection (&).
func (o *opsCNF) merge(pol bool, l, r cnf) cnf {
	if pol {
		return o.or(l, r) // CNF(A | B) = CNF(A) | CNF(B)
	}
	return o.and(l, r) // CNF(A & B) = CNF(A) & CNF(B)
}

// and computes the intersection of two CNFs (this & other).
// CNF represents an intersection, so it's just concatenation.
func (o *opsCNF) and(left, right cnf) cnf {
	// TODO: Add simplification/subsumption checks if needed
	return append(left, right...)
}

// or computes the union of two CNFs (this | other).
// CNF(A | B) = CNF(A) | CNF(B)
// (D1 & D2) | (D3 & D4) = (D1|D3) & (D1|D4) & (D2|D3) & (D2|D4)
func (o *opsCNF) or(left, right cnf) cnf {
	result := o.extr(true) // Start with Top (empty CNF)
	for _, d1 := range left {
		for _, d2 := range right {
			if merged := o.disjunctOrDisjunct(d1, d2); merged != nil {
				result = o.andDisjunct(result, *merged) // Add the merged disjunct
			} else {
				// If merging results in Top, the whole CNF becomes Top
				return o.extr(true)
			}
		}
	}
	return result
}

// andDisjunct computes the intersection of a CNF and a single Disjunct (this & d).
func (o *opsCNF) andDisjunct(c cnf, d disjunct) cnf {
	// TODO: Add simplification/subsumption checks if needed
	return append(c, d)
}

// disjunctOrDisjunct computes the union of two Disjuncts.
// Returns nil if the result is Top.
func (o *opsCNF) disjunctOrDisjunct(left, right disjunct) *disjunct {
	// Corresponds to Disjunct.| in Scala
	newRnf, ok := o.rhsOrRhs(left.right, right.right)
	if !ok { // If Rhs union results in Top
		return nil
	}
	newLnf, ok := left.left.and(right.left, o.ctx)
	if !ok { // If Lhs intersection results in Bottom
		return nil // Represents Top in the disjunct context
	}
	return &disjunct{
		right: newRnf,
		vars:  left.vars.Union(right.vars),
		left:  newLnf,
		nvars: left.nvars.Union(right.nvars),
	}
}

// rhsOrRhs computes the union of two RhsNf components.
// Returns nil if the result is Top.
func (o *opsCNF) rhsOrRhs(left, right rhsNF) (res rhsNF, ok bool) {
	// This logic mirrors RhsNf.| in Scala
	// Need to handle combinations of RhsBot, RhsField, RhsBases

	if _, ok := right.(rhsBot); ok {
		return left, true
	}

	switch l := left.(type) {
	case rhsBot:
		return right, true
	case *rhsField:
		switch r := right.(type) {
		case *rhsField:
			// {f: T} | {g: U} -> Top if f != g
			// {f: T} | {f: U} -> {f: T | U}
			if l.name.Name == r.name.Name {
				newTy := o.fieldTypeUnion(l.ty, r.ty) // Need fieldTypeUnion helper
				return &rhsField{name: l.name, ty: newTy}, true
			}
			return nil, false // Represents Top
		case *rhsBases:
			// {f: T} | (Tags | Rest | Refs) -> Top if Rest is Func/Array
			// {f: T} | (Tags | {g: U} | Refs) -> Top if f != g
			// {f: T} | (Tags | {f: U} | Refs) -> (Tags | {f: T | U} | Refs)
			if r.rest != nil {
				if rField, ok := r.rest.(*rhsField); ok {
					if l.name.Name == rField.name.Name {
						newTy := o.fieldTypeUnion(l.ty, rField.ty)
						newRest := &rhsField{name: l.name, ty: newTy}
						return &rhsBases{tags: r.tags, rest: newRest, typeRefs: r.typeRefs}, true
					}
				}
				// If rest is not a field or names don't match, result is Top
				return nil, false
			}
			// If r.rest is nil, add l as the rest
			return &rhsBases{tags: r.tags, rest: l, typeRefs: r.typeRefs}, true
		}
	case *rhsBases:
		switch r := right.(type) {
		case *rhsField:
			// Symmetric to the case above
			return o.rhsOrRhs(r, l)
		case *rhsBases:
			// (T1|R1|F1) | (T2|R2|F2)
			newTags := mergeTags(l.tags, r.tags)             // Union of tags
			newRefs := mergeTypeRefs(l.typeRefs, r.typeRefs) // Union of refs
			newRest := o.rhsRestOrRhsRest(l.rest, r.rest)
			if newRest == nil && (l.rest != nil || r.rest != nil) { // If rest union resulted in Top
				return nil, false
			}
			return &rhsBases{tags: newTags, rest: newRest, typeRefs: newRefs}, true
		}
	}
	// Should not be reached if all rhsNF types are handled
	panic(fmt.Sprintf("rhsOrRhs: unhandled combination %T | %T", left, right))
}

// rhsRestOrRhsRest computes the union of two optional rhsRest components.
// Returns nil if the result is Top.
func (o *opsCNF) rhsRestOrRhsRest(left, right rhsRest) rhsRest {
	if left == nil {
		return right
	}
	if right == nil {
		return left
	}

	switch l := left.(type) {
	case funcType:
		if r, ok := right.(funcType); ok {
			// (A -> B) | (C -> D) = (A & C) -> (B | D)
			newArg := intersectionOf(l.args[0], r.args[0], unionOpts{}) // Assuming single arg for now
			newRet := unionOf(l.ret, r.ret, unionOpts{})
			// TODO: Preserve provenance better
			return funcType{args: []SimpleType{newArg}, ret: newRet}
		}
		return nil // Func | Array/FieldType -> Top
	case arrayBase: // tuple, namedTuple, array
		switch r := right.(type) {
		case funcType:
			return nil // Array/Tuple | Func -> Top
		case arrayBase:
			// Array/Tuple | Array/Tuple
			// Need to handle Array|Array, Tuple|Tuple, Array|Tuple
			// Convert tuples to arrays if sizes differ or mixing with ArrayType
			lArray, lIsArray := l.(arrayType)
			rArray, rIsArray := r.(arrayType)
			lTuple, lIsTuple := l.(tupleType) // Assuming tupleType covers namedTupleType for inner logic
			rTuple, rIsTuple := r.(tupleType)

			if lIsArray && rIsArray {
				// Array<T> | Array<U> = Array<T | U>
				newInner := unionOf(lArray.inner(o.ctx), rArray.inner(o.ctx), unionOpts{})
				return arrayType{innerT: newInner}
			} else if lIsTuple && rIsTuple {
				// Tuple<T...> | Tuple<U...>
				if len(lTuple.fields) == len(rTuple.fields) {
					// Tuple<T1,T2> | Tuple<U1,U2> = Tuple<T1|U1, T2|U2>
					newFields := make([]SimpleType, len(lTuple.fields))
					for i := range lTuple.fields {
						newFields[i] = unionOf(lTuple.fields[i], rTuple.fields[i], unionOpts{})
					}
					// TODO: Handle named tuples properly if needed
					return tupleType{fields: newFields}
				}
				// Tuples of different sizes -> Array<innerL | innerR>
				newInner := unionOf(lTuple.inner(o.ctx), rTuple.inner(o.ctx), unionOpts{})
				return arrayType{innerT: newInner}
			} else {
				// Array | Tuple or Tuple | Array -> Array<innerA | innerT>
				var arrInner, tupInner SimpleType
				if lIsArray {
					arrInner = lArray.inner(o.ctx)
					tupInner = rTuple.inner(o.ctx)
				} else {
					arrInner = rArray.inner(o.ctx)
					tupInner = lTuple.inner(o.ctx)
				}
				newInner := unionOf(arrInner, tupInner, unionOpts{})
				return arrayType{innerT: newInner}
			}
		}
	case *rhsField:
		if r, ok := right.(*rhsField); ok {
			// {f: T} | {g: U} -> Top if f != g
			// {f: T} | {f: U} -> {f: T | U}
			if l.name.Name == r.name.Name {
				newTy := o.fieldTypeUnion(l.ty, r.ty)
				return &rhsField{name: l.name, ty: newTy}
			}
		}
		return nil // FieldType | Func/Array -> Top
	}
	// Should not be reached
	o.ctx.addFailure(fmt.Sprintf("rhsRestOrRhsRest: unhandled combination %T | %T", left, right), nil)
	return left
}

// --- Helper functions for CNF ---

// fieldTypeUnion computes the union of two field types.
func (o *opsCNF) fieldTypeUnion(ft1, ft2 fieldType) fieldType {
	// (L1..U1) | (L2..U2) = (L1 & L2)..(U1 | U2)
	lb := intersectionOf(ft1.lowerBound, ft2.lowerBound, unionOpts{})
	ub := unionOf(ft1.upperBound, ft2.upperBound, unionOpts{})
	// TODO: Better provenance handling
	return fieldType{lowerBound: lb, upperBound: ub, withProvenance: ft1.withProvenance} // Arbitrarily pick ft1's prov
}

// mergeTags computes the union of two lists of object tags, removing duplicates.
func mergeTags(tags1, tags2 []objectTag) []objectTag {
	// Simple approach: concatenate and unique. Could be optimized.
	seen := make(map[uint64]bool)
	result := make([]objectTag, 0, len(tags1)+len(tags2))
	for _, tag := range append(tags1, tags2...) {
		h := tag.Hash() // Assuming Hash is implemented and distinguishes tags
		if !seen[h] {
			seen[h] = true
			result = append(result, tag)
		}
		// TODO: Could add logic here to simplify based on hierarchy, e.g., if C <: P, keep only P in a union.
	}
	return result
}

// mergeTypeRefs computes the union of two lists of type references, removing duplicates.
func mergeTypeRefs(refs1, refs2 []typeRef) []typeRef {
	// Simple approach: concatenate and unique.
	seen := make(map[uint64]bool)
	result := make([]typeRef, 0, len(refs1)+len(refs2))
	for _, ref := range append(refs1, refs2...) {
		h := ref.Hash()
		if !seen[h] {
			seen[h] = true
			result = append(result, ref)
		}
		// TODO: Could potentially merge refs like A[T] | A[U] -> A[T|U] based on variance.
	}
	return result
}
