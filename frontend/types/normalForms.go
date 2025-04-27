package types

import (
	"github.com/cottand/ile/util"
	"iter"
	"maps"
	"slices"
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
		vars,                                                     // our vars
		util.SingleIter(negateType(j.right.toType(), emptyProv)), // !(right.toType())
		negatedNVars,
	)
	current := j.left.toType()
	for type_ := range types {
		current = intersectionOf(current, type_, unionOpts{})
	}
	return current
}

type normalForm interface {
	toType() SimpleType
	level() level
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

type leftRefined struct{}
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

// dnf is a slice of junction where junction.isConjunct() is true
type dnf []conjunct
type cnf []disjunct
