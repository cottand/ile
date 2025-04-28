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
	left  lhsNF
	right rhsNF

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
		left:  left,
		right: right,
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
	sb.WriteString(j.left.String())
	for v := range j.vars.Items() {
		sb.WriteString("∧" + v.String())
	}
	sb.WriteString("∧!(" + j.right.String() + ")")
	for v := range j.nvars.Items() {
		sb.WriteString("∧!(" + v.String() + ")")
	}
	return sb.String()
}

func (j conjunct) negate() disjunct {
	return newDisjunct(j.left, j.right, j.nvars, j.vars)
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
	return max(maxLevel, j.left.level(), j.right.level())
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
		util.SingleIter(negateType(j.right.toType(), emptyProv)), // !(right.toType())
		negatedNVars,
	)
	current := j.left.toType()
	for type_ := range types {
		current = intersectionOf(current, type_, unionOpts{})
	}
	return current
}

func (j conjunct) lessThanOrEqual(other conjunct) bool {
	panic("TODO lessThanOrEqual")
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
	normalForm
	isLeftNf() // marker for sealed hierarchy
}

var (
	_ lhsNF = (*lhsRefined)(nil)
	_ lhsNF = (*lhsTop)(nil)
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

	// Intersect with optional base class tag
	if l.base != nil {
		current = intersectionOf(current, l.base, unionOpts{})
	}
	// Intersect with optional function type
	if l.fn != nil {
		current = intersectionOf(current, l.fn, unionOpts{})
	}
	// Intersect with optional array base type
	if l.arr != nil {
		current = intersectionOf(current, l.arr, unionOpts{})
	}

	for _, id := range l.traitTags.Slice() {
		current = intersectionOf(current, id, unionOpts{})
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
	for id := range l.traitTags.Items() {
		sb.WriteString("∧")
		sb.WriteString(id.String())
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

type lhsTop struct{}

func (lhsTop) String() string         { return "⊤" }
func (lhsTop) isLeftNf()              {}
func (lhsTop) toType() SimpleType     { return topType }
func (lhsTop) level() level           { return topType.level() }
func (lhsTop) hasTag(_ traitTag) bool { return false }
func (lhsTop) size() int              { return 0 }

// --- Right Normal Form (RhsNf) ---

type rhsNF interface {
	normalForm
	isRhsNf() // marker for sealed hierarchy
}

var (
	_ rhsNF = (*rhsBot)(nil)
	_ rhsNF = (*rhsField)(nil)
	_ rhsNF = (*rhsBases)(nil)
)

// rhsBot corresponds to RhsBot in Scala. Represents the bottom type (empty union).
type rhsBot struct{}

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
// Alternatively, use a struct with pointers and check for non-nil. Let's use an interface.
type rhsRest interface {
	SimpleType // All possible types here are SimpleType
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

func (r *rhsBases) isRhsNf() {}

// toType reconstructs the SimpleType representation of the union.
func (r *rhsBases) toType() SimpleType {
	// Start with the base type/field, or Bottom if nil
	current := SimpleType(bottomType)
	if r.rest != nil {
		current = r.rest // Note: rhsField.toType() converts it to a record
	}

	// Union with tags (sorted for canonical representation)
	sortedTags := slices.Clone(r.tags)
	slices.SortFunc(sortedTags, func(a, b objectTag) int {
		// Define a consistent comparison for objectTag, e.g., using Compare method if available
		// or by string representation as a fallback.
		return a.Compare(b) // Assuming Compare exists and works
	})
	for _, tag := range sortedTags {
		current = unionOf(current, tag, unionOpts{})
	}

	// Union with type references (sorted for canonical representation)
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
		current = unionOf(current, tr, unionOpts{})
	}

	return current
}

// level calculates the maximum polymorphism level among all components.
func (r *rhsBases) level() level {
	maxLevel := level(0)
	if r.rest != nil {
		maxLevel = r.rest.level()
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
		parts = append(parts, r.rest.String())
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
