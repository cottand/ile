package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"slices"
	"strings"
)

var nfLogger = logger.With("section", "normalForms")

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
	negatedNVars := util.MapIter(j.nvars.Items(), func(nvar *typeVariable) SimpleType { // nvars.map(!_)
		return negateType(nvar, emptyProv)
	})
	vars := util.MapIter(j.vars.Items(), func(tv *typeVariable) SimpleType {
		return tv
	})
	types := util.ConcatIter[SimpleType](
		vars, // our vars
		util.SingleIter(negateType(j.rhs.toType(), emptyProv)), // !(rhs.toType())
		negatedNVars,
	)
	current := j.lhs.toType()
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
		baseSubtype := l.base != nil && lhs.base != nil && ctx.SolveSubtype(*l.base, *lhs.base, nil)
		fnSubtype := l.fn != nil && lhs.fn != nil && ctx.SolveSubtype(*l.fn, *lhs.fn, nil)
		arrSubtype := l.arr != nil && lhs.arr != nil && ctx.SolveSubtype(l.arr, lhs.arr, nil)
		allTraitTagsSub := l.traitTags != nil && lhs.traitTags != nil && l.traitTags.Subset(lhs.traitTags)
		rTypeSub := ctx.SolveSubtype(l.reft, lhs.reft, nil)
		if !((baseSubtype || fnSubtype || arrSubtype) && allTraitTagsSub && rTypeSub) {
			return false
		}
		for _, typeRef := range lhs.typeRefs {
			exists := false
			for _, thisTypeRef := range l.typeRefs {
				exists = exists || ctx.SolveSubtype(thisTypeRef, typeRef, nil)
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
		if ctx.SolveSubtype(*r.base, *l.base, nil) {
			// r is a subtype of l, so use r
			base = r.base
		} else if ctx.SolveSubtype(*l.base, *r.base, nil) {
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
		if ctx.SolveSubtype(*l.fn, *r.fn, nil) {
			fn = l.fn
		} else if ctx.SolveSubtype(*r.fn, *l.fn, nil) {
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
		if ctx.SolveSubtype(l.arr, r.arr, nil) {
			arr = l.arr
		} else if ctx.SolveSubtype(r.arr, l.arr, nil) {
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
		fieldMap[field.Fst.Name] = field.Snd
	}

	// Create a result with all fields from r1
	result := recordType{
		fields:         make([]util.Pair[ast.Var, fieldType], 0, len(r1.fields)+len(r2.fields)),
		withProvenance: r1.withProvenance,
	}
	for _, field := range r1.fields {
		result.fields = append(result.fields, field)
	}

	// Add or merge fields from r2
	for _, field := range r2.fields {
		name := field.Fst.Name
		if existing, found := fieldMap[name]; found {
			// Field exists in both records, merge bounds
			// Lower bound becomes the union of lower bounds
			mergedLower := unionOf(existing.lowerBound, field.Snd.lowerBound, unionOpts{})
			// Upper bound becomes the intersection of upper bounds
			mergedUpper := intersectionOf(existing.upperBound, field.Snd.upperBound, unionOpts{})

			// Ensure that lower <: upper
			if !ctx.SolveSubtype(mergedLower, mergedUpper, nil) {
				return recordType{}, false
			}

			// Update the field in the result
			for i, resultField := range result.fields {
				if resultField.Fst.Name == name {
					result.fields[i].Snd = fieldType{
						lowerBound:     mergedLower,
						upperBound:     mergedUpper,
						withProvenance: resultField.Snd.withProvenance,
					}
					break
				}
			}
		} else {
			// Field only in r2, add it to result
			result.fields = append(result.fields, field)
			fieldMap[name] = field.Snd
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
	panic("TODO lessThanOrEqual for lhsNF")
}
func (lhsTop) and(nf lhsNF, ctx *TypeCtx) (lhsNF, bool) {
	return nf, true
}

// --- Right Normal Form (RhsNf) ---

type rhsNF interface {
	normalForm
	// lessThanOrEqual is written as <:< in the scala reference
	lessThanOrEqual(rhsNF) bool
	isRhsNf() // marker for sealed hierarchy
}

var (
	_ rhsNF = (*rhsBot)(nil)
	_ rhsNF = (*rhsField)(nil)
	_ rhsNF = (*rhsBases)(nil)
)

// rhsBot corresponds to RhsBot in Scala. Represents the bottom type (empty union).
type rhsBot struct{}

func (b rhsBot) lessThanOrEqual(nf rhsNF) bool {
	//TODO implement me
	panic("implement me")
}

func (rhsBot) String() string         { return "⊥" }
func (rhsBot) isRhsNf()               {}
func (rhsBot) toType() SimpleType     { return bottomType }
func (rhsBot) level() level           { return bottomType.level() }
func (rhsBot) hasTag(_ traitTag) bool { return false }
func (rhsBot) size() int              { return 0 }

// rhsField corresponds to RhsField in Scala. Represents a single record field {name: type}.
type rhsField struct {
	name string
	ty   fieldType
}

func (r *rhsField) lessThanOrEqual(nf rhsNF) bool {
	//TODO implement me
	panic("implement me")
}

func (r *rhsField) isRhsNf() {}
func (r *rhsField) String() string {
	return fmt.Sprintf("{%s: %s}", r.name, r.ty.String())
}
func (r *rhsField) toType() SimpleType {
	// Represents the type `{name: ty}` which is a RecordType
	return recordType{
		fields: []util.Pair[ast.Var, fieldType]{{Fst: ast.Var{Name: r.name}, Snd: r.ty}},
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
	// Optional function/array/field component
	rest rhsRest
	// Slice of type references (needs sorting for canonical representation)
	typeRefs []typeRef
}

func (r *rhsBases) lessThanOrEqual(nf rhsNF) bool {
	//TODO implement me
	panic("implement me")
}

func (r *rhsBases) isRhsNf() {}

// toType reconstructs the SimpleType representation of the union.
func (r *rhsBases) toType() SimpleType {
	panic("TODO implement rhsBases toType in scala's RhsNf")
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
		if tt, ok := t.(traitTag); ok && tt.Equivalent(tag) {
			return true
		}
	}
	return false
}

// size calculates the number of components in the union.
func (r *rhsBases) size() int {
	count := 0
	if r.rest != nil {
		count++ // Function/Array/Field counts as 1
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
	slices.SortFunc(sortedTags, func(a, b objectTag) int { return a.Compare(b) })
	for _, tag := range sortedTags {
		parts = append(parts, tag.String())
	}

	// Rest (Function/Array/Field)
	if r.rest != nil {
		if st, isSimpleType := r.rest.(SimpleType); isSimpleType {
			parts = append(parts, st.String())
		} else {
			panic("TODO - handle String for when rest is a record field")
		}
	}

	// Type references (sorted)
	sortedTrefs := slices.Clone(r.typeRefs)
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
		parts = append(parts, tr.String())
	}

	if len(parts) == 0 {
		return "⊥" // Should technically be handled by rhsBot, but good fallback
	}
	return strings.Join(parts, "|")
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
