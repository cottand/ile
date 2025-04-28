package types

import (
	"cmp"
	"fmt"
	"github.com/hashicorp/go-set/v3"
)

// opsDNF corresponds to the DNF companion object in the scala reference
// We add operations between dnf and cnf here instead of as methods of those structs so that we can
// store here their constructors (like mk) and the TypeCtx.
type opsDNF struct {
	ctx *TypeCtx
	// preserveTypeRefs controls whether TypeRefs are expanded during normalization
	preserveTypeRefs bool
}

func newOpsDNF(ctx *TypeCtx, preserveTypeRefs bool) *opsDNF {
	return &opsDNF{ctx: ctx, preserveTypeRefs: preserveTypeRefs}
}

// --- DNF Methods (Corresponds to methods on DNF class in Scala) ---

// and computes the intersection of two DNFs (this & other).
// DNF represents a union, so intersection distributes: (A | B) & (C | D) = (A&C) | (A&D) | (B&C) | (B&D)
func (o *opsDNF) and(d dnf, other dnf) dnf {
	result := o.extr(false)
	for _, c2 := range other {
		if merged := o.andConjunct(d, c2); merged != nil {
			result = o.or(merged, result)
		}
	}
	return result
}

// or computes the union of two DNFs (this | other).
func (o *opsDNF) or(left, right dnf) dnf {
	// Fold `right` into `left` using `orConjunct` for simplification.
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
		if merged := o.conjunctAndConjunct(dc, c); merged != nil {
			resultConjuncts = append(resultConjuncts, *merged)
		}
	}
	return resultConjuncts
}

func (o *opsDNF) conjunctOrConjunct(left, right conjunct) *conjunct {
	panic("TODO")
}
func (o *opsDNF) conjunctAndConjunct(left, right conjunct) *conjunct {
	panic("TODO")
}

// orConjunct computes the union of a DNF and a single Conjunct (this | c).
// This involves checking for subsumption and trying to merge conjuncts.
func (o *opsDNF) orConjunct(d dnf, c conjunct) dnf {
	newConjuncts := make([]conjunct, 0, len(d)+1)
	merged := false
	subsumed := false // If the new conjunct `c` is subsumed by an existing one

	for _, existingC := range d {
		if subsumed { // If c was already subsumed, just keep the rest
			newConjuncts = append(newConjuncts, existingC)
			continue
		}
		if existingC.lessThanOrEqual(c) {
			// `c` is larger or equal, keep `c` and potentially discard `existingC` later if strictly larger
			// For now, we just know `c` is not subsumed by `existingC`.
			// Try merging:
			if mergedC := o.tryMergeUnion(existingC, c); mergedC != nil {
				// Successfully merged, replace existingC with mergedC and mark c as handled
				newConjuncts = append(newConjuncts, *mergedC)
				merged = true // Mark that we performed a merge (c is now part of mergedC)
			} else {
				// Cannot merge, keep existingC
				newConjuncts = append(newConjuncts, existingC)
			}
		} else if c.lessThanOrEqual(existingC) { // c <:< existingC
			// `c` is smaller or equal, it's subsumed by `existingC`. Keep `existingC`.
			newConjuncts = append(newConjuncts, existingC)
			subsumed = true // Mark `c` as handled (subsumed)
		} else {
			// No subsumption, try merging:
			if mergedC := o.tryMergeUnion(existingC, c); mergedC != nil {
				// Successfully merged
				newConjuncts = append(newConjuncts, *mergedC)
				merged = true
			} else {
				// Cannot merge, keep existingC
				newConjuncts = append(newConjuncts, existingC)
			}
		}
	}

	// If the new conjunct `c` was not merged or subsumed, add it.
	if !merged && !subsumed {
		newConjuncts = append(newConjuncts, c)
	}

	// Final sort might be needed if strict canonical form is required,
	// but the logic above maintains relative order and handles merging/subsumption.
	// Sorting might be expensive. Let's skip explicit sorting for now.
	// slices.SortFunc(newConjuncts, func(a, b conjunct) int { ... })
	return newConjuncts
}

// result might be nil
func (o *opsDNF) tryMergeUnion(left, right conjunct) *conjunct {
	panic("TODO implement tryMergeUnion")
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
	if _, ok := lnf.(lhsTop); ok {
		return o.top() // LhsTop corresponds to Top DNF
	}
	return dnf{newConjunct(lnf, rhsBot{}, nil, nil)}
}

// extr returns the extremal DNF (Top or Bottom).
// pol=true means Top, pol=false means Bottom.
func (o *opsDNF) extr(pol bool) dnf {
	if pol {
		return o.top()
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
func (o *opsDNF) mk(ty SimpleType, pol bool) dnf {
	ty = unwrapProvenance(ty) // Work with the underlying type

	switch t := ty.(type) {
	case funcType:
		return o.of(&lhsRefined{fn: &t, reft: emptyRecord})
	case arrayBase: // Covers tupleType, arrayType, namedTupleType
		return o.of(&lhsRefined{arr: t, reft: emptyRecord})
	case classTag:
		return o.of(&lhsRefined{base: &t, reft: emptyRecord})
	case traitTag:
		traitSet := set.TreeSetFrom([]traitTag{t}, func(a, b traitTag) int {
			return cmp.Compare(a.Hash(), b.Hash())
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
		vars := set.TreeSetFrom([]typeVariable{*t}, compareTypeVars)
		return dnf{newConjunct(lhsTop{}, rhsBot{}, vars, nil)}
	case typeRef:
		if o.preserveTypeRefs && !isPrimitiveTypeName(t.defName) { // TODO: Refine primitive check
			// Represent as LhsRefined with the TypeRef
			refs := []typeRef{t}
			var baseTag *classTag

			// TODO: Add mkTag logic if needed here, similar to Scala
			// if tag := t.mkTag(o.ctx); tag != nil { baseTag = tag }
			return o.of(&lhsRefined{base: baseTag, typeRefs: refs, reft: emptyRecord})
		}
		// Expand the TypeRef and create DNF from the expansion
		return o.mk(o.ctx.expand(t), pol) // Assuming ctx.expand exists
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
		panic(fmt.Sprintf("opsDNF.mk: unhandled type %T", ty))
	}
}

// mkDeep recursively normalizes a type into DNF form.
func (o *opsDNF) mkDeep(ty SimpleType, pol bool) dnf {
	deepST := o.mkDeepST(ty, pol)
	return o.mk(deepST, pol)
}

// mkDeepST performs the recursive traversal for mkDeep.
func (o *opsDNF) mkDeepST(ty SimpleType, pol bool) SimpleType {
	ty = unwrapProvenance(ty) // Operate on the underlying type

	switch t := ty.(type) {
	case typeRange:
		// Normalize the relevant bound based on polarity
		boundToNormalize := t.upperBound
		if !pol {
			boundToNormalize = t.lowerBound
		}
		// Return the deeply normalized bound, wrapped in original provenance
		return wrappingProvType{
			SimpleType:      o.mkDeepST(boundToNormalize, pol),
			proxyProvenance: t.prov(),
		}
	default:
		// Apply the recursive normalization to children
		// Need a way to map over children based on polarity, similar to mapPol
		// This part is complex and depends heavily on how mapPol is implemented or adapted.
		// Placeholder: Return the type reconstructed from DNF without deep recursion for now.
		// A full implementation requires a `mapPol` equivalent.
		// return stFromDnf.mapPol(polarityFromBool(pol), mapFunc) // Assuming mapPol exists
		logger.Error("mkDeepST: Deep recursive normalization on children not fully implemented", "type", ty)
		panic("TODO implement mkDeppST") // Return shallow normalized type for now
	}
}

// Helper to check for primitive type names (adjust as needed)
func isPrimitiveTypeName(name typeName) bool {
	switch name {
	case "int", "number", "bool", "true", "false", "string", "unit", "any", "nothing", "error", "nil":
		return true
	default:
		return false
	}
}

// --- Helper: Get DNF representation for Top ---
// func dnfTop() dnf {
// 	return dnf{newConjunct(lhsTop{}, nil, rhsBot{}, nil)}
// }

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

func (o *opsCNF) mk(type_ SimpleType, pol bool) cnf {
	panic("implement CNF mk")
}
