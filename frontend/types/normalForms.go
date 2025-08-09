package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"hash/fnv"
	"iter"
	"slices"
	"strings"
)

// junction corresponds to conjunction or disjunction in the scala reference
type conjunct struct {
	lhs lhsNF
	rhs rhsNF

	// vars, nvars are ordered
	// we use *typeVariable pointer so that it matches a SimpleType, and
	// because the type var is already allocated in the heap anyway, so no point
	// in moving it back and forth from the stack
	vars, nvars set.Collection[*typeVariable]
}
type disjunct struct {
	left  lhsNF
	right rhsNF

	// vars, nvars are ordered
	vars, nvars set.Collection[*typeVariable]
}

var compareTypeVars set.CompareFunc[*typeVariable] = func(t1, t2 *typeVariable) int {
	return cmp.Compare(t1.id, t2.id)
}

var compareTraitTags set.CompareFunc[traitTag] = func(tag traitTag, tag2 traitTag) int {
	return cmp.Compare(tag.Hash(), tag2.Hash())
}

func newDisjunct(left lhsNF, right rhsNF, vars, nvars set.Collection[*typeVariable]) disjunct {
	if vars == nil {
		vars = set.NewTreeSet(compareTypeVars)
	}
	if nvars == nil {
		nvars = set.NewTreeSet(compareTypeVars)
	}
	return disjunct{
		left:  left,
		right: right,
		vars:  vars,
		nvars: nvars,
	}
}
func newConjunct(left lhsNF, right rhsNF, vars, nvars set.Collection[*typeVariable]) conjunct {
	if vars == nil {
		vars = set.NewTreeSet(compareTypeVars)
	}
	if nvars == nil {
		nvars = set.NewTreeSet(compareTypeVars)
	}
	return conjunct{
		lhs:   left,
		rhs:   right,
		vars:  vars,
		nvars: nvars,
	}
}

// simplifyConjunct corresponds to Conjunct.mk in the Scala reference
// we do not implement it as a normal constructor because it has a failure path when ok = false
func (o *opsDNF) simplifyConjunct(c conjunct) (res conjunct, ok bool) {
	rightBases, okRightBases := c.rhs.(*rhsBases)
	leftRefined, okLeftRefined := c.lhs.(*lhsRefined)

	foundOccurrence := false
	// determine if there is a class name X in LHS-refined and a tag Y in RHS-bases such that X == Y
	// if so, we cannot merge
	if okLeftRefined && okRightBases {
		typeRefClasses := util.MapIter(slices.Values(leftRefined.typeRefs), func(tr typeRef) string { return tr.String() })
		var typeRefBases iter.Seq[string] = func(yield func(string) bool) {
			for _, ref := range leftRefined.typeRefs {
				for baseClass := range o.ctx.baseClassesOf(ref.defName) {
					if !yield(baseClass) {
						return
					}
				}
			}
		}
		var baseClassNames iter.Seq[string] = func(yield func(string) bool) {
			base := leftRefined.base
			if base == nil {
				return
			}

			for ref := range base.parents.Items() {
				if !yield(ref) {
					return
				}
			}

			if base, ok := base.id.(*ir.Var); ok {
				yield(base.Name)
			}
		}

		allClasses := util.ConcatIter(
			typeRefClasses,
			typeRefBases,
			baseClassNames,
		)

		for className := range allClasses {
			foundOccurrence = slices.ContainsFunc(rightBases.tags, func(rhsTag objectTag) bool {
				return rhsTag.Id().CanonicalSyntax() == className
			})
			if foundOccurrence {
				break
			}
		}
	}

	if foundOccurrence {
		return conjunct{}, false
	}

	if !okRightBases {
		return c, true
	}

	newTags := make([]objectTag, 0, 1)
	for _, tag := range rightBases.tags {
		// throw away the tag if it cannot be &'d with
		if _, ok := o.leftNFAndType(c.lhs, tag); ok {
			newTags = append(newTags, tag)
		}
	}

	newRest := rightBases.rest
	// throw away the negated side if it is a type, and we cannot &'d it with the LHS
	// eg, True & ~False -> True
	if newRest != nil {
		restAsType, ok := newRest.(basicType)
		if ok {
			if _, ok := o.leftNFAndType(c.lhs, restAsType); !ok {
				newRest = nil
			}
		}
	}
	return conjunct{
		lhs: c.lhs,
		rhs: &rhsBases{
			tags:     newTags,
			rest:     newRest,
			typeRefs: rightBases.typeRefs,
		},
		vars:  c.vars,
		nvars: c.nvars,
	}, true
}

func (o *opsDNF) leftNFAndType(left lhsNF, b basicType) (res lhsNF, ok bool) {
	// Implementation based on LhsNf.&(that: BasicType) from Scala
	switch l := left.(type) {
	case lhsTop:
		// Cases for lhsTop
		switch bt := b.(type) {
		case tupleType:
			return &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       bt,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			}, true
		case arrayType:
			return &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       bt,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			}, true
		case classTag:
			return &lhsRefined{
				base:      &bt,
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			}, true
		case traitTag:
			traitTags := set.NewTreeSet(compareTraitTags)
			traitTags.Insert(bt)
			return &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: traitTags,
				reft:      recordType{},
				typeRefs:  nil,
			}, true
		default:
			o.Error("leftNFAndType: unsupported basicType for lhsTop", "lhsTop", b)
			return nil, false
		}
	case *lhsRefined:
		// Cases for lhsRefined
		switch bt := b.(type) {
		case traitTag:
			// Add the trait tag to the existing trait tags
			newTraitTags := set.NewTreeSet(compareTraitTags)
			if l.traitTags != nil {
				for tag := range l.traitTags.Items() {
					newTraitTags.Insert(tag)
				}
			}
			newTraitTags.Insert(bt)

			return &lhsRefined{
				base:      l.base,
				fn:        l.fn,
				arr:       l.arr,
				traitTags: newTraitTags,
				reft:      l.reft,
				typeRefs:  l.typeRefs,
			}, true
		case classTag:
			if l.base == nil {
				// No class tag yet, add it
				return &lhsRefined{
					base:      &bt,
					fn:        l.fn,
					arr:       l.arr,
					traitTags: l.traitTags,
					reft:      l.reft,
					typeRefs:  l.typeRefs,
				}, true
			} else {
				// Already has a class tag, find the greatest lower bound
				// In Scala: cls.glb(that).map(cls => LhsRefined(S(cls), f1, a1, ts, r1, trs))
				// In Go, we need to check if one is a subtype of the other
				if o.ctx.isSubtype(bt, *l.base, nil) {
					// bt is a subtype of l.base, so use bt
					return &lhsRefined{
						base:      &bt,
						fn:        l.fn,
						arr:       l.arr,
						traitTags: l.traitTags,
						reft:      l.reft,
						typeRefs:  l.typeRefs,
					}, true
				} else if o.ctx.isSubtype(*l.base, bt, nil) {
					// l.base is a subtype of bt, so keep l.base
					return l, true
				} else {
					// Incompatible class tags
					return nil, false
				}
			}
		case tupleType:
			if l.arr == nil {
				// No array base yet, add it
				return &lhsRefined{
					base:      l.base,
					fn:        l.fn,
					arr:       bt,
					traitTags: l.traitTags,
					reft:      l.reft,
					typeRefs:  l.typeRefs,
				}, true
			} else {
				// Already has an array base, merge them
				var result arrayBase

				switch at := l.arr.(type) {
				case tupleType:
					// Check if tuple sizes match
					if len(at.fields) != len(bt.fields) {
						return nil, false
					}

					// Create a new tuple with intersected element types
					fields := make([]SimpleType, len(at.fields))
					for i := 0; i < len(at.fields); i++ {
						fields[i] = intersectionOf(at.fields[i], bt.fields[i], unionOpts{})
					}

					result = tupleType{
						fields:         fields,
						withProvenance: at.withProvenance,
					}
				case arrayType:
					// Convert tuple to array and intersect with the array type
					fields := make([]SimpleType, len(bt.fields))
					for i, field := range bt.fields {
						fields[i] = intersectionOf(at.innerT, field, unionOpts{})
					}

					result = tupleType{
						fields:         fields,
						withProvenance: bt.withProvenance,
					}
				default:
					o.Error("leftNFAndType: unsupported array base combination", "left", l.arr, "type", bt)
					return nil, false
				}

				return &lhsRefined{
					base:      l.base,
					fn:        l.fn,
					arr:       result,
					traitTags: l.traitTags,
					reft:      l.reft,
					typeRefs:  l.typeRefs,
				}, true
			}
		case arrayType:
			if l.arr == nil {
				// No array base yet, add it
				return &lhsRefined{
					base:      l.base,
					fn:        l.fn,
					arr:       bt,
					traitTags: l.traitTags,
					reft:      l.reft,
					typeRefs:  l.typeRefs,
				}, true
			} else {
				// Already has an array base, merge them
				var result arrayBase

				switch at := l.arr.(type) {
				case tupleType:
					// Convert tuple to array and intersect with the array type
					fields := make([]SimpleType, len(at.fields))
					for i, field := range at.fields {
						fields[i] = intersectionOf(field, bt.innerT, unionOpts{})
					}

					result = tupleType{
						fields:         fields,
						withProvenance: at.withProvenance,
					}
				case arrayType:
					// Intersect element types
					innerT := intersectionOf(at.innerT, bt.innerT, unionOpts{})

					result = arrayType{
						innerT:         innerT,
						withProvenance: at.withProvenance,
					}
				default:
					o.Error("leftNFAndType: unsupported array base combination", "left", l.arr, "type", bt)
					return nil, false
				}

				return &lhsRefined{
					base:      l.base,
					fn:        l.fn,
					arr:       result,
					traitTags: l.traitTags,
					reft:      l.reft,
					typeRefs:  l.typeRefs,
				}, true
			}
		default:
			o.Error("leftNFAndType: unsupported basicType for lhsRefined", "type", bt)
			return nil, false
		}
	default:
		o.Error("leftNFAndType: unsupported lhsNF", "left", left)
		return nil, false
	}
}

func (j disjunct) String() string {
	sb := &strings.Builder{}
	sb.WriteString(j.right.String())
	for v := range j.vars.Items() {
		sb.WriteString("v")
		sb.WriteString(v.String())
	}
	sb.WriteString("v!(" + j.left.String() + ")")
	for v := range j.nvars.Items() {
		sb.WriteString("v!(" + v.String() + ")")
	}
	return sb.String()
}

func (j conjunct) String() string {
	sb := &strings.Builder{}
	sb.WriteString(j.lhs.String())
	for v := range j.vars.Items() {
		sb.WriteString("∧" + v.String())
	}
	sb.WriteString("∧!(" + j.rhs.String() + ")")
	for v := range j.nvars.Items() {
		sb.WriteString("∧!(" + v.String() + ")")
	}
	return sb.String()
}

func (j conjunct) Hash() uint64 {
	hash := uint64(31)
	h := fnv.New64a()
	_, _ = h.Write([]byte(j.lhs.String()))
	_, _ = h.Write([]byte(j.rhs.String()))
	for v := range j.vars.Items() {
		hash = hash * v.Hash()
	}
	for v := range j.nvars.Items() {
		hash = hash * v.Hash()
	}
	return hash * h.Sum64()
}

func (j conjunct) negate() disjunct {
	return newDisjunct(j.lhs, j.rhs, j.nvars, j.vars)
}

func (j disjunct) negate() conjunct {
	return newConjunct(j.left, j.right, j.nvars, j.vars)
}

// the level of a junction is the max of all the levels of its children
func (j conjunct) level() level {
	var maxLevel level = 0
	for v := range j.vars.Items() {
		maxLevel = max(maxLevel, v.level())
	}
	for v := range j.nvars.Items() {
		maxLevel = max(maxLevel, v.level())
	}
	return max(maxLevel, j.lhs.level(), j.rhs.level())
}

// the scala reference allows choosing whether to sort type variables
// we are using sorted sets anyway so we sort all the time.
func (j conjunct) toType() SimpleType {
	return j.toTypeWith(
		func(nf lhsNF) SimpleType { return nf.toType() },
		func(nf rhsNF) SimpleType { return nf.toType() },
	)
}

func (j conjunct) toTypeWith(lhsToType func(lhsNF) SimpleType, rhsToType func(rhsNF) SimpleType) SimpleType {
	negatedNVars := util.MapIter(j.nvars.Items(), func(nvar *typeVariable) SimpleType { // nvars.map(!_)
		return negateType(nvar, emptyProv)
	})
	// should be a noop, but we do this so that we can Set[*typeVariable] -> Set[SimpleType]
	vars := util.MapIter(j.vars.Items(), func(tv *typeVariable) SimpleType {
		return tv
	})
	types := util.ConcatIter[SimpleType](
		vars, // our vars
		util.SingleIter(negateType(rhsToType(j.rhs), emptyProv)), // !(rhs.toType())
		negatedNVars,
	)
	current := lhsToType(j.lhs)
	for type_ := range types {
		current = intersectionOf(current, type_, unionOpts{})
	}
	return current

}

func (j conjunct) lessThanOrEqual(other conjunct) bool {
	for otherVar := range other.vars.Items() {
		if !j.vars.Contains(otherVar) {
			return false
		}
	}
	if !(j.lhs.lessThanOrEqual(other.lhs) && other.rhs.lessThanOrEqual(j.rhs)) {
		return false
	}
	for otherNVar := range other.nvars.Items() {
		if !j.nvars.Contains(otherNVar) {
			return false
		}
	}

	return true
}

// both lhsNF are rhsNF are normalForm
type normalForm interface {
	fmt.Stringer
	toType() SimpleType
	level() level
	// hasTag checks if the normal form includes a specific trait tag.
	// Note: This differs slightly from Scala's `hasTag` which takes `ObjectTag`.
	// We might need separate methods or a broader interface if class tag checks are needed here.
	hasTag(tag traitTag) bool
	size() int
}
type lhsNF interface {
	// lessThanOrEqual is written as <:< in the scala reference
	lessThanOrEqual(lhsNF) bool
	and(nf lhsNF, ctx *TypeCtx) (lhs lhsNF, ok bool)
	normalForm
	isLeftNf() // marker for sealed hierarchy
	isLeftNfTop() bool
}

var (
	_ lhsNF = (*lhsRefined)(nil)
	_ lhsNF = lhsTop{}
)

// lhsRefined corresponds to LhsRefined in the scala reference.
// It represents an intersection of various type components.
type lhsRefined struct {
	// Optional components (use pointers)
	base *classTag
	fn   *funcType
	// Interface for array-like types (can be nil)
	arr arrayBase
	// Use map for set semantics (ID -> tag)
	// traitTags must stay ordered for canonical representation
	traitTags set.Collection[traitTag]
	// Record type component
	reft recordType
	// Slice of type references (needs sorting for canonical representation)
	typeRefs []typeRef
}

func (*lhsRefined) isLeftNfTop() bool {
	return false
}

func (l *lhsRefined) isLeftNf() {}

// toType reconstructs the SimpleType representation of the intersection.
// Corresponds to the `underlying` lazy val in Scala.
func (l *lhsRefined) toType() SimpleType {
	// Start with the record type, which is assumed to be the base non-optional part.
	// In Scala, LhsRefined always has a RecordType, even if empty.
	current := SimpleType(l.reft)

	// we always want to store SimpleType the same way so we follow the pointer
	// before putting it in the intersection here
	if l.base != nil {
		current = intersectionOf(current, *(l.base), unionOpts{})
	}
	if l.fn != nil {
		current = intersectionOf(current, *(l.fn), unionOpts{})
	}
	if l.arr != nil {
		current = intersectionOf(current, l.arr, unionOpts{})
	}

	if l.traitTags != nil {
		for id := range l.traitTags.Items() {
			current = intersectionOf(current, id, unionOpts{})
		}
	}

	// Intersect with type references (need to sort for canonical representation)
	// Assuming typeRef implements Comparable or has a consistent sort order
	sortedTrefs := slices.Clone(l.typeRefs) // Avoid modifying original slice
	slices.SortFunc(sortedTrefs, func(a, b typeRef) int {
		// Define a consistent comparison for typeRef, e.g., by name then args
		if a.defName != b.defName {
			if a.defName < b.defName {
				return -1
			}
			return 1
		}
		// TODO: Add comparison for typeArgs if needed for strict canonical form
		return 0
	})
	for _, tr := range sortedTrefs {
		current = intersectionOf(current, tr, unionOpts{})
	}

	return current
}

// level calculates the maximum polymorphism level among all components.
func (l *lhsRefined) level() level {
	maxLevel := l.reft.level()
	if l.base != nil {
		maxLevel = max(maxLevel, l.base.level())
	}
	if l.fn != nil {
		maxLevel = max(maxLevel, l.fn.level())
	}
	if l.arr != nil {
		maxLevel = max(maxLevel, l.arr.level())
	}
	// Trait tags have level 0
	for _, tr := range l.typeRefs {
		maxLevel = max(maxLevel, tr.level())
	}
	return maxLevel
}

// hasTag checks if the given trait tag is present.
func (l *lhsRefined) hasTag(tag traitTag) bool {
	return l.traitTags.Contains(tag)
}

// size calculates the number of components in the intersection.
func (l *lhsRefined) size() int {
	count := len(l.reft.fields)
	if l.base != nil {
		count++
	}
	if l.fn != nil {
		count++
	}
	if l.arr != nil {
		count++ // Assuming arrayBase contributes 1 to size
	}
	count += l.traitTags.Size()
	count += len(l.typeRefs)
	return count
}

// String provides a string representation similar to Scala's.
func (l *lhsRefined) String() string {
	var sb strings.Builder
	if l.base != nil {
		sb.WriteString(l.base.String())
	}
	if l.fn != nil {
		sb.WriteString(l.fn.String())
	}
	if l.arr != nil {
		sb.WriteString(l.arr.String())
	}
	sb.WriteString(l.reft.String()) // Assuming recordType has Stringer

	// Trait tags (sorted for consistency)
	if l.traitTags != nil {
		for id := range l.traitTags.Items() {
			sb.WriteString("∧")
			sb.WriteString(id.String())
		}
	}

	// Type references (sorted for consistency)
	sortedTrefs := slices.Clone(l.typeRefs)
	slices.SortFunc(sortedTrefs, func(a, b typeRef) int {
		if a.defName != b.defName {
			if a.defName < b.defName {
				return -1
			}
			return 1
		}
		return 0
	})
	for _, tr := range sortedTrefs {
		sb.WriteString("∧")
		sb.WriteString(tr.String())
	}

	return sb.String()
}

func (l *lhsRefined) lessThanOrEqual(other lhsNF) bool {
	ctx := NewEmptyTypeCtx()
	switch lhs := other.(type) {
	case lhsTop:
		return true
	case *lhsRefined:
		baseSubtype := l.base != nil && lhs.base != nil && ctx.isSubtype(*l.base, *lhs.base, nil)
		fnSubtype := l.fn != nil && lhs.fn != nil && ctx.isSubtype(*l.fn, *lhs.fn, nil)
		arrSubtype := l.arr != nil && lhs.arr != nil && ctx.isSubtype(l.arr, lhs.arr, nil)
		allTraitTagsSub := (l.traitTags == nil && lhs.traitTags == nil) || (l.traitTags != nil && lhs.traitTags != nil && l.traitTags.Subset(lhs.traitTags))
		rTypeSub := ctx.isSubtype(l.reft, lhs.reft, nil)
		if !((baseSubtype || fnSubtype || arrSubtype) && allTraitTagsSub && rTypeSub) {
			return false
		}
		for _, typeRef := range lhs.typeRefs {
			exists := false
			for _, thisTypeRef := range l.typeRefs {
				exists = exists || ctx.isSubtype(thisTypeRef, typeRef, nil)
			}
			if !exists {
				return false
			}
		}
		return true
	default:
		panic(fmt.Sprintf("unexpected type in lessThanOrEqual: %T", other))

	}
}

func (l *lhsRefined) and(other lhsNF, ctx *TypeCtx) (lhsNF, bool) {
	_, ok := other.(lhsTop)
	if ok {
		// Combining with top yields the original type
		return l, true
	}

	r, ok := other.(*lhsRefined)
	if !ok {
		// Should never happen as only lhsTop and lhsRefined implement lhsNF
		panic(fmt.Sprintf("unknown lhsNF implementation: %T", other))
	}

	// Merge class tags - both must be compatible
	var base *classTag
	if l.base != nil && r.base != nil {
		if ctx.isSubtype(*r.base, *l.base, nil) {
			// r is a subtype of l, so use r
			base = r.base
		} else if ctx.isSubtype(*l.base, *r.base, nil) {
			// l is a subtype of r, so use l
			base = l.base
		} else {
			// Incompatible base types
			return nil, false
		}
	} else if l.base != nil {
		base = l.base
	} else {
		base = r.base
	}

	// Merge function types
	var fn *funcType
	if l.fn != nil && r.fn != nil {
		// Function types must be compatible
		if ctx.isSubtype(*l.fn, *r.fn, nil) {
			fn = l.fn
		} else if ctx.isSubtype(*r.fn, *l.fn, nil) {
			fn = r.fn
		} else {
			// Incompatible function types
			return nil, false
		}
	} else if l.fn != nil {
		fn = l.fn
	} else {
		fn = r.fn
	}

	// Merge array bases
	var arr arrayBase
	if l.arr != nil && r.arr != nil {
		// Array types must be compatible
		if ctx.isSubtype(l.arr, r.arr, nil) {
			arr = l.arr
		} else if ctx.isSubtype(r.arr, l.arr, nil) {
			arr = r.arr
		} else {
			// Incompatible array types
			return nil, false
		}
	} else if l.arr != nil {
		arr = l.arr
	} else {
		arr = r.arr
	}

	// Merge trait tags - take union of both sets
	var traitTags set.Collection[traitTag]
	if l.traitTags != nil && r.traitTags != nil {
		traitTags = set.NewTreeSet(compareTraitTags)
		for tag := range l.traitTags.Items() {
			traitTags.Insert(tag)
		}
		for tag := range r.traitTags.Items() {
			traitTags.Insert(tag)
		}
	} else if l.traitTags != nil {
		traitTags = l.traitTags
	} else {
		traitTags = r.traitTags
	}

	// Merge record types
	mergedReft, ok := mergeRecordTypes(l.reft, r.reft, ctx)
	if !ok {
		return nil, false
	}

	// Merge type refs
	typeRefs := make([]typeRef, 0, len(l.typeRefs)+len(r.typeRefs))
	typeRefs = append(typeRefs, l.typeRefs...)
	typeRefs = append(typeRefs, r.typeRefs...)

	// Deduplicate type refs (basic approach)
	if len(typeRefs) > 0 {
		// Simple approach to deduplication - could be optimized
		uniqueRefs := make(map[string]typeRef)
		for _, tr := range typeRefs {
			uniqueRefs[tr.defName] = tr
		}

		typeRefs = make([]typeRef, 0, len(uniqueRefs))
		for _, tr := range uniqueRefs {
			typeRefs = append(typeRefs, tr)
		}
	}

	// Construct the combined lhsRefined
	result := &lhsRefined{
		base:      base,
		fn:        fn,
		arr:       arr,
		traitTags: traitTags,
		reft:      mergedReft,
		typeRefs:  typeRefs,
	}

	return result, true
}

// mergeRecordTypes combines two record types, returning false if they're incompatible
func mergeRecordTypes(r1, r2 recordType, ctx *TypeCtx) (recordType, bool) {
	// If one is empty, return the other
	if len(r1.fields) == 0 {
		return r2, true
	}
	if len(r2.fields) == 0 {
		return r1, true
	}

	// Build a map of field names to field types for r1
	fieldMap := make(map[string]fieldType, len(r1.fields))
	for _, field := range r1.fields {
		fieldMap[field.name.Name] = field.type_
	}

	// Create a result with all fields from r1
	result := recordType{
		fields:         make([]recordField, 0, len(r1.fields)+len(r2.fields)),
		withProvenance: r1.withProvenance,
	}
	for _, field := range r1.fields {
		result.fields = append(result.fields, field)
	}

	// Add or merge fields from r2
	for _, field := range r2.fields {
		name := field.name.Name
		if existing, found := fieldMap[name]; found {
			// FieldType exists in both records, merge bounds
			// Lower bound becomes the union of lower bounds
			mergedLower := unionOf(existing.lowerBound, field.type_.lowerBound, unionOpts{})
			// Upper bound becomes the intersection of upper bounds
			mergedUpper := intersectionOf(existing.upperBound, field.type_.upperBound, unionOpts{})

			// Ensure that lower <: upper
			if !ctx.isSubtype(mergedLower, mergedUpper, nil) {
				return recordType{}, false
			}

			// Update the field in the result
			for i, resultField := range result.fields {
				if resultField.name.Name == name {
					result.fields[i].type_ = fieldType{
						lowerBound:     mergedLower,
						upperBound:     mergedUpper,
						withProvenance: resultField.type_.withProvenance,
					}
					break
				}
			}
		} else {
			// FieldType only in r2, add it to result
			result.fields = append(result.fields, field)
			fieldMap[name] = field.type_
		}
	}

	return result, true
}

func (l *lhsRefined) andRecordType(r recordType) lhsRefined {
	panic("TODO implement andRecordType for lhsRefined")
}

type lhsTop struct{}

func (lhsTop) String() string         { return "⊤" }
func (lhsTop) isLeftNf()              {}
func (lhsTop) toType() SimpleType     { return topType }
func (lhsTop) level() level           { return topType.level() }
func (lhsTop) hasTag(_ traitTag) bool { return false }
func (lhsTop) size() int              { return 0 }
func (lhsTop) lessThanOrEqual(nf lhsNF) bool {
	return nf.isLeftNfTop()
}
func (lhsTop) and(nf lhsNF, ctx *TypeCtx) (lhsNF, bool) {
	return nf, true
}
func (t lhsTop) isLeftNfTop() bool { return true }

// --- Right Normal Form (RhsNf) ---

type rhsNF interface {
	normalForm
	// lessThanOrEqual is written as <:< in the scala reference
	lessThanOrEqual(rhsNF) bool
	equal(rhsNF) bool
	isRhsNf() // marker for sealed hierarchy
	isRhsBot() bool
}

var (
	_ rhsNF = (*rhsBot)(nil)
	_ rhsNF = (*rhsField)(nil)
	_ rhsNF = (*rhsBases)(nil)
)

// rhsBot corresponds to RhsBot in Scala. Represents the bottom type (empty union).
type rhsBot struct{}

func (b rhsBot) isRhsBot() bool                { return true }
func (b rhsBot) lessThanOrEqual(nf rhsNF) bool { return true }
func (rhsBot) String() string                  { return "⊥" }
func (rhsBot) isRhsNf()                        {}
func (rhsBot) toType() SimpleType              { return bottomType }
func (rhsBot) level() level                    { return bottomType.level() }
func (rhsBot) hasTag(_ traitTag) bool          { return false }
func (rhsBot) size() int                       { return 0 }
func (rhsBot) equal(other rhsNF) bool          { return other.isRhsBot() }

// rhsField corresponds to RhsField in Scala. Represents a single record field {name: type}.
type rhsField struct {
	name ir.Var
	ty   fieldType
}

func (_ *rhsField) isRhsBot() bool { return false }
func (r *rhsField) lessThanOrEqual(nf rhsNF) bool {
	ctx := NewEmptyTypeCtx()
	return ctx.isSubtype(r.toType(), nf.toType(), nil)
}

func (r *rhsField) isRhsNf() {}
func (r *rhsField) String() string {
	return fmt.Sprintf("{%s: %s}", r.name.Name, r.ty.String())
}
func (r *rhsField) toType() SimpleType {
	// Represents the type `{name: ty}` which is a RecordType
	return recordType{
		fields: []recordField{{name: r.name, type_: r.ty}},
		// Use a relevant provenance if available, otherwise emptyProv
		withProvenance: r.ty.withProvenance, // Use fieldType's provenance
	}
}
func (r *rhsField) level() level {
	return r.ty.level()
}
func (r *rhsField) hasTag(_ traitTag) bool {
	return false // A single field doesn't have a trait tag
}
func (r *rhsField) size() int {
	return 1 // Represents a single field component
}
func (r *rhsField) equal(other rhsNF) bool {
	asRhsField, ok := other.(*rhsField)
	return ok &&
		r.name.Name == asRhsField.name.Name &&
		Equal(r.ty.lowerBound, asRhsField.ty.lowerBound) &&
		Equal(r.ty.upperBound, asRhsField.ty.upperBound)
}

// rhsRest represents the `FunOrArrType \/ RhsField` part in Scala's RhsBases.
// We use an interface that both `funcType`, `arrayBase`, and `rhsField` can satisfy.
type rhsRest interface {
	isRhsRest()
}

// Ensure relevant types implement rhsRest
func (funcType) isRhsRest()       {}
func (tupleType) isRhsRest()      {}
func (namedTupleType) isRhsRest() {}
func (arrayType) isRhsRest()      {}
func (*rhsField) isRhsRest()      {} // Pointer receiver to match interface

// rhsBases corresponds to RhsBases in Scala. Represents a union of tags, a base type/field, and type refs.
type rhsBases struct {
	// Slice of class/trait tags (needs sorting for canonical representation)
	tags []objectTag
	// rest is an optional function/array/field component (ie, can be nil)
	rest rhsRest
	// Slice of type references (needs sorting for canonical representation)
	typeRefs []typeRef
}

func (*rhsBases) isRhsBot() bool { return false }
func (r *rhsBases) lessThanOrEqual(nf rhsNF) bool {
	ctx := NewEmptyTypeCtx()
	return ctx.isSubtype(r.toType(), nf.toType(), nil)
}
func (r *rhsBases) isRhsNf() {}

// toType reconstructs the SimpleType representation of the union.
// tagN | tag(N-1) | ... | tag1 | rest | typeRef1 | ... | typeRefN
func (r *rhsBases) toType() SimpleType {
	var res SimpleType
	switch t := r.rest.(type) {
	case *rhsField:
		res = t.toType()
		// case for function, tuple, namedTuple, or array types
	case SimpleType:
		res = t
	default:
		if r.rest != nil {
			panic(fmt.Sprintf("unreachable case: unexpected type in rhsBases.toType: %T", t))
		}
		res = bottomType
	}
	sortedRefs := slices.SortedFunc(slices.Values(r.typeRefs), util.ComparingHashable)
	for _, ref := range sortedRefs {
		res = unionOf(res, ref, unionOpts{})
	}
	sortedTags := slices.SortedFunc(slices.Values(r.tags), util.ComparingHashable)
	slices.Reverse(sortedTags)
	for _, tag := range sortedTags {
		res = unionOf(tag, res, unionOpts{})
	}
	return res
}

// level calculates the maximum polymorphism level among all components.
func (r *rhsBases) level() level {
	maxLevel := level(0)
	if r.rest != nil {
		if st, isST := r.rest.(SimpleType); isST {
			maxLevel = st.level()
		} else {
			panic("TODO - instead of figuring out the level manually, could probably just call mkType and get its level")
		}

	}
	// Object tags have level 0
	for _, tr := range r.typeRefs {
		maxLevel = max(maxLevel, tr.level())
	}
	return maxLevel
}

// hasTag checks if the given trait tag is present in the tags slice.
func (r *rhsBases) hasTag(tag traitTag) bool {
	for _, t := range r.tags {
		// Need to check type and ID equivalence
		if tt, ok := t.(traitTag); ok && Equal(tt, tag) {
			return true
		}
	}
	return false
}

// size calculates the number of components in the union.
func (r *rhsBases) size() int {
	count := 0
	if r.rest != nil {
		count++ // Function/Array/FieldType counts as 1
	}
	count += len(r.tags)
	count += len(r.typeRefs)
	return count
}

// String provides a string representation similar to Scala's.
func (r *rhsBases) String() string {
	var parts []string

	// Tags (sorted)
	sortedTags := slices.Clone(r.tags)
	slices.SortFunc(sortedTags, util.ComparingHashable)
	for _, tag := range sortedTags {
		parts = append(parts, tag.String())
	}

	// Rest (Function/Array/FieldType)
	if r.rest != nil {
		if st, isSimpleType := r.rest.(SimpleType); isSimpleType {
			parts = append(parts, st.String())
		} else {
			panic("TODO - handle String for when rest is a record field")
		}
	}

	// Type references (sorted)
	sortedTrefs := slices.Clone(r.typeRefs)
	slices.SortFunc(sortedTrefs, util.ComparingHashable)
	for _, tr := range sortedTrefs {
		parts = append(parts, tr.String())
	}

	if len(parts) == 0 {
		return "⊥" // Should technically be handled by rhsBot, but good fallback
	}
	return strings.Join(parts, "|")
}

func (r *rhsBases) equal(other rhsNF) bool {
	otherBases, ok := other.(*rhsBases)
	if !ok {
		return false
	}
	if !slices.EqualFunc(r.tags, otherBases.tags, Equal) {
		return false
	}
	if !slices.EqualFunc(r.typeRefs, otherBases.typeRefs, Equal) {
		return false
	}
	asType, thisIsType := r.rest.(SimpleType)
	otherAsType, otherIsType := otherBases.rest.(SimpleType)
	if thisIsType && otherIsType {
		return Equal(asType, otherAsType)
	}
	if thisIsType != otherIsType {
		return false
	}
	asField, thisIsField := r.rest.(*rhsField)
	otherAsField, otherIsField := otherBases.rest.(*rhsField)
	if thisIsField && otherIsField {
		return asField.equal(otherAsField)
	}
	return false
}

// --- DNF/CNF Types ---
type dnf []conjunct
type cnf []disjunct

// TODO use a proper hash function here for better performance
func compareConjuncts(a, b conjunct) int {
	return cmp.Compare(a.String(), b.String())
}

func (j dnf) toType() SimpleType {
	sorted := slices.SortedFunc(slices.Values(j), compareConjuncts)

	var result SimpleType = bottomType
	for _, c := range sorted {
		result = unionOf(result, c.toType(), unionOpts{})
	}
	return result
}

func (j dnf) String() string {
	return util.JoinString(j, "V")
}

func (j cnf) String() string {
	return util.JoinString(j, "V")
}
