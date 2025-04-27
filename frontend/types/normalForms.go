package types

import (
	"fmt"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"iter"
	"maps"
	"slices"
	"strings"
)

var nfLogger = logger.With("section", "normalForms")

// junction corresponds to conjunction or disjunction in the scala reference
type conjunct struct {
	left  leftNF
	right rightNF

	// vars, nvars are ordered
	vars, nvars map[TypeVarID]*typeVariable
}
type disjunct struct {
	left  leftNF
	right rightNF

	// vars, nvars are ordered
	vars, nvars map[TypeVarID]*typeVariable
}

func newDisjunct(left leftNF, right rightNF, vars, nvars map[TypeVarID]*typeVariable) disjunct {
	if vars == nil {
		vars = make(map[TypeVarID]*typeVariable)
	}
	if nvars == nil {
		nvars = make(map[TypeVarID]*typeVariable)
	}
	return disjunct{
		left:  left,
		right: right,
		vars:  vars,
		nvars: nvars,
	}
}
func newConjunct(left leftNF, right rightNF, vars, nvars map[TypeVarID]*typeVariable) conjunct {
	if vars == nil {
		vars = make(map[TypeVarID]*typeVariable)
	}
	if nvars == nil {
		nvars = make(map[TypeVarID]*typeVariable)
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
	for _, v := range j.vars {
		sb.WriteString("v")
		sb.WriteString(v.String())
	}
	sb.WriteString("v!(" + j.left.String() + ")")
	for _, v := range j.nvars {
		sb.WriteString("v!(" + v.String() + ")")
	}
	return sb.String()
}

func (j conjunct) String() string {
	sb := &strings.Builder{}
	sb.WriteString(j.left.String())
	for _, v := range j.vars {
		sb.WriteString("∧" + v.String())
	}
	sb.WriteString("∧!(" + j.right.String() + ")")
	for _, v := range j.nvars {
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

func sortedVars(vars map[TypeVarID]*typeVariable) iter.Seq[SimpleType] {
	ordered := slices.Sorted(maps.Keys(vars))
	orderedValues := func(yield func(variable SimpleType) bool) {
		for _, k := range ordered {
			if !yield(vars[k]) {
				return
			}
		}
	}
	return orderedValues
}

// the level of a junction is the max of all the levels of its children
func (j conjunct) level() level {
	var maxLevel level = 0
	for _, v := range j.vars {
		maxLevel = max(maxLevel, v.level())
	}
	for _, v := range j.nvars {
		maxLevel = max(maxLevel, v.level())
	}
	return max(maxLevel, j.left.level(), j.right.level())
}

// the scala reference allows choosing whether to sort type variables
// we are using sorted sets anyway so we sort all the time.
func (j conjunct) toType() SimpleType {
	vars := sortedVars(j.vars)
	negatedNVars := util.MapIter(sortedVars(j.nvars), func(nvar SimpleType) SimpleType { // nvars.map(!_)
		return negateType(nvar, emptyProv)
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

// both leftNF are rightNF are normalForm
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
type leftNF interface {
	normalForm
	isLeftNf() // marker for sealed hierarchy
}

var (
	_ leftNF = (*leftRefined)(nil)
	_ leftNF = (*leftTop)(nil)
)

// leftRefined corresponds to LhsRefined in the scala reference.
// It represents an intersection of various type components.
type leftRefined struct {
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

func (l *leftRefined) isLeftNf() {}

// toType reconstructs the SimpleType representation of the intersection.
// Corresponds to the `underlying` lazy val in Scala.
func (l *leftRefined) toType() SimpleType {
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
func (l *leftRefined) level() level {
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
func (l *leftRefined) hasTag(tag traitTag) bool {
	return l.traitTags.Contains(tag)
}

// size calculates the number of components in the intersection.
func (l *leftRefined) size() int {
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
func (l *leftRefined) String() string {
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

type leftTop struct{}

func (leftTop) String() string         { return "⊤" }
func (leftTop) isLeftNf()              {}
func (leftTop) toType() SimpleType     { return topType }
func (leftTop) level() level           { return topType.level() }
func (leftTop) hasTag(_ traitTag) bool { return false }
func (leftTop) size() int              { return 0 }

type rightNF interface {
	normalForm
	isRightNf() // marker for sealed hierarchy
}

type dnf []conjunct
type cnf []disjunct
