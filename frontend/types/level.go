package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"reflect"
)

// Fresher keeps track of new variable IDs
// it us mutable and not suitable for concurrent use
type Fresher struct {
	freshCount uint64
	freshened  map[TypeVarID]SimpleType
}

func NewFresher() *Fresher {
	return &Fresher{
		freshCount: 0,
		freshened:  make(map[TypeVarID]SimpleType),
	}
}

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

type level uint16

func (t *Fresher) freshenAbove(l level, limit level, type_ SimpleType) SimpleType {
	return t.freshenAboveWithRigidify(l, limit, type_, false)
}

func (t *Fresher) freshenAboveWithRigidify(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	return t.freshen(l, limit, type_, rigidify)
}

func (t *Fresher) freshen(l level, limit level, type_ SimpleType, rigidify bool) SimpleType {
	// Rigidification now also substitutes TypeBound-s with fresh vars;
	// since these have the level of their bounds, when rigidifying
	// we need to make sure to copy the whole type regardless of level...
	if !rigidify && type_.level() <= limit {
		return type_
	}
	switch type_ := type_.(type) {
	case *typeVariable:
		found, ok := t.freshened[type_.id]
		if ok {
			return found
		}
		if !rigidify {
			// else
			freshV := t.newTypeVariable(l, type_.provenance, type_.nameHint, nil, nil)
			t.freshened[type_.id] = freshV
			for _, lb := range type_.lowerBounds {
				freshV.lowerBounds = append(freshV.lowerBounds, t.freshen(l, limit, lb, rigidify))
			}
			for _, ub := range type_.upperBounds {
				freshV.upperBounds = append(freshV.upperBounds, t.freshen(l, limit, ub, rigidify))
			}
			return freshV
		}
		// so we have rigidify=true

		var_ := &ast.Var{
			Name: type_.nameHint,
		}
		if type_.nameHint == "" {
			var_.Name = "_" + t.newTypeVariable(l, typeProvenance{}, "", nil, nil).String()
		}
		rigidTypeVar := traitTag{
			id:             var_,
			withProvenance: type_.withProvenance,
		}
		if len(type_.lowerBounds) == 0 && len(type_.upperBounds) == 0 {
			t.freshened[type_.id] = rigidTypeVar
			return rigidTypeVar
		}
		// The bounds of `type_` may be recursive (refer to `tv` itself),
		//    so here we create a fresh variable that will be able to tie the presumed recursive knot
		//    (if there is no recursion, it will just be a useless type variable)
		freshV := t.newTypeVariable(l, type_.provenance, type_.nameHint, nil, nil)
		t.freshened[type_.id] = freshV
		// Assuming there were no recursive bounds, given L <: tv <: U,
		//    we essentially need to turn type_'s occurrence into the type-bounds (rv | L)..(rv & U),
		//    meaning all negative occurrences should be interpreted as rv | L
		//    and all positive occurrences should be interpreted as rv & U
		//    where rv is the rigidified variables.
		// Now, since there may be recursive bounds, we do the same
		//    but through the indirection of a type variable tv2:
		var currentLowerBound SimpleType = topType // we use the fact that A & Top = A
		for _, lb := range type_.lowerBounds {
			freshLb := t.freshen(l, limit, lb, rigidify)
			currentLowerBound = intersectionOf(currentLowerBound, freshLb, unionOpts{})
		}
		freshV.lowerBounds = append(freshV.lowerBounds, currentLowerBound)

		var currentUpperBound SimpleType = bottomType // we use the fact that A | Bot = A
		for _, up := range type_.upperBounds {
			freshUp := t.freshen(l, limit, up, rigidify)
			currentUpperBound = unionOf(currentUpperBound, freshUp, unionOpts{})
		}
		freshV.upperBounds = append(freshV.upperBounds, currentUpperBound)

		return freshV
	case typeRange:
		if !rigidify {
			return typeRange{
				lowerBound:     t.freshen(l, limit, type_.lowerBound, rigidify),
				upperBound:     t.freshen(l, limit, type_.upperBound, rigidify),
				withProvenance: type_.withProvenance,
			}
		}
		freshVar := t.newTypeVariable(l, type_.provenance, "", nil, nil)
		freshVar.lowerBounds = append(freshVar.lowerBounds, t.freshen(l, limit, type_.lowerBound, rigidify))
		freshVar.upperBounds = append(freshVar.upperBounds, t.freshen(l, limit, type_.upperBound, rigidify))
		return freshVar
	case funcType:
		var fields = make([]SimpleType, len(type_.args))
		for i, field := range type_.args {
			fields[i] = t.freshen(l, limit, field, rigidify)
		}
		return funcType{
			args: fields,
			ret:  t.freshen(l, limit, type_.ret, rigidify),
		}
	case unionType:
		return unionType{
			lhs: t.freshen(l, limit, type_.lhs, rigidify),
			rhs: t.freshen(l, limit, type_.rhs, rigidify),
		}
	case intersectionType:
		return intersectionType{
			lhs: t.freshen(l, limit, type_.lhs, rigidify),
			rhs: t.freshen(l, limit, type_.rhs, rigidify),
		}
	case negType:
		return negType{
			negated: t.freshen(l, limit, type_.negated, rigidify),
		}
	case extremeType:
		return type_
	case tupleType:
		var fields = make([]SimpleType, len(type_.fields))
		for i, field := range type_.fields {
			fields[i] = t.freshen(l, limit, field, rigidify)
		}
		return tupleType{
			fields:         fields,
			withProvenance: type_.withProvenance,
		}
	case namedTupleType:
		var fields []util.Pair[ast.Var, SimpleType] = make([]util.Pair[ast.Var, SimpleType], len(type_.fields))
		for i, field := range type_.fields {
			fields[i] = util.Pair[ast.Var, SimpleType]{
				Fst: field.Fst,
				Snd: t.freshen(l, limit, field.Snd, rigidify),
			}
		}
		return namedTupleType{
			fields:         fields,
			withProvenance: type_.withProvenance,
		}
	case arrayType:
		return arrayType{
			innerT:         t.freshen(l, limit, type_.innerT, rigidify),
			withProvenance: type_.withProvenance,
		}
	case classTag, traitTag:
		return type_

	case typeRef:
		var typeArgs []SimpleType = make([]SimpleType, len(type_.typeArgs))
		for i, typeArg := range type_.typeArgs {
			typeArgs[i] = t.freshen(l, limit, typeArg, rigidify)
		}
		return typeRef{
			defName:        type_.defName,
			typeArgs:       typeArgs,
			withProvenance: type_.withProvenance,
		}

	default:
		panic("unhandled type for freshen: " + reflect.TypeOf(type_).String())
	}
}
