package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"reflect"
)

type level uint16

func (l level) freshTypeVar(prov *typeProvenance, nameHint string, lowerBounds, upperBounds []SimpleType) typeVariable {
	panic("TODO implement me")
}

func (l level) freshenAbove(limit level, type_ SimpleType) SimpleType {
	return l.freshenAboveWithRigidify(limit, type_, false)
}

func (l level) freshenAboveWithRigidify(limit level, type_ SimpleType, rigidify bool) SimpleType {
	return l.freshen(limit, type_, rigidify, make(map[typeVariableID]SimpleType))
}

func (l level) freshen(limit level, type_ SimpleType, rigidify bool, freshened map[typeVariableID]SimpleType) SimpleType {
	// Rigidification now also substitutes TypeBound-s with fresh vars;
	// since these have the level of their bounds, when rigidifying
	// we need to make sure to copy the whole type regardless of level...
	if !rigidify && type_.level() <= limit {
		return type_
	}
	switch type_ := type_.(type) {
	case typeVariable:
		found, ok := freshened[type_.id]
		if ok {
			return found
		}
		if !rigidify {
			// else
			freshV := l.freshTypeVar(type_.provenance, type_.nameHint, nil, nil)
			freshened[type_.id] = freshV
			for _, lb := range type_.lowerBounds {
				freshV.lowerBounds = append(freshV.lowerBounds, l.freshen(limit, lb, rigidify, freshened))
			}
			for _, ub := range type_.upperBounds {
				freshV.upperBounds = append(freshV.upperBounds, l.freshen(limit, ub, rigidify, freshened))
			}
			return freshV
		}
		// so we have rigidify=true

		var_ := &ast.Var{
			Name: type_.nameHint,
		}
		if type_.nameHint == "" {
			var_.Name = "_" + l.freshTypeVar(nil, "", nil, nil).String()
		}
		rigidTypeVar := traitTag{
			id:             var_,
			withProvenance: type_.withProvenance,
		}
		if len(type_.lowerBounds) == 0 && len(type_.upperBounds) == 0 {
			freshened[type_.id] = rigidTypeVar
			return rigidTypeVar
		}
		// The bounds of `type_` may be recursive (refer to `tv` itself),
		//    so here we create a fresh variable that will be able to tie the presumed recursive knot
		//    (if there is no recursion, it will just be a useless type variable)
		freshV := l.freshTypeVar(type_.provenance, type_.nameHint, nil, nil)
		freshened[type_.id] = freshV
		// Assuming there were no recursive bounds, given L <: tv <: U,
		//    we essentially need to turn type_'s occurrence into the type-bounds (rv | L)..(rv & U),
		//    meaning all negative occurrences should be interpreted as rv | L
		//    and all positive occurrences should be interpreted as rv & U
		//    where rv is the rigidified variables.
		// Now, since there may be recursive bounds, we do the same
		//    but through the indirection of a type variable tv2:
		var currentLowerBound SimpleType = topType // we use the fact that A & Top = A
		for _, lb := range type_.lowerBounds {
			freshLb := l.freshen(limit, lb, rigidify, freshened)
			currentLowerBound = intersectionOf(currentLowerBound, freshLb, unionOpts{})
		}
		freshV.lowerBounds = append(freshV.lowerBounds, currentLowerBound)

		var currentUpperBound SimpleType = bottomType // we use the fact that A | Bot = A
		for _, up := range type_.upperBounds {
			freshUp := l.freshen(limit, up, rigidify, freshened)
			currentUpperBound = unionOf(currentUpperBound, freshUp, unionOpts{})
		}
		freshV.upperBounds = append(freshV.upperBounds, currentUpperBound)

		return freshV
	case typeRange:
		if !rigidify {
			return typeRange{
				lowerBound:     l.freshen(limit, type_.lowerBound, rigidify, freshened),
				upperBound:     l.freshen(limit, type_.upperBound, rigidify, freshened),
				withProvenance: type_.withProvenance,
			}
		}
		freshVar := l.freshTypeVar(type_.provenance, "", nil, nil)
		freshVar.lowerBounds = append(freshVar.lowerBounds, l.freshen(limit, type_.lowerBound, rigidify, freshened))
		freshVar.upperBounds = append(freshVar.upperBounds, l.freshen(limit, type_.upperBound, rigidify, freshened))
		return freshVar
	case funcType:
		var fields = make([]SimpleType, len(type_.args))
		for i, field := range type_.args {
			fields[i] = l.freshen(limit, field, rigidify, freshened)
		}
		return funcType{
			args: fields,
			ret:  l.freshen(limit, type_.ret, rigidify, freshened),
		}
	case unionType:
		return unionType{
			lhs: l.freshen(limit, type_.lhs, rigidify, freshened),
			rhs: l.freshen(limit, type_.rhs, rigidify, freshened),
		}
	case intersectionType:
		return intersectionType{
			lhs: l.freshen(limit, type_.lhs, rigidify, freshened),
			rhs: l.freshen(limit, type_.rhs, rigidify, freshened),
		}
	case negType:
		return negType{
			negated: l.freshen(limit, type_.negated, rigidify, freshened),
		}
	case extremeType:
		return type_
	case tupleType:
		var fields = make([]SimpleType, len(type_.fields))
		for i, field := range type_.fields {
			fields[i] = l.freshen(limit, field, rigidify, freshened)
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
				Snd: l.freshen(limit, field.Snd, rigidify, freshened),
			}
		}
		return namedTupleType{
			fields:         fields,
			withProvenance: type_.withProvenance,
		}
	case arrayType:
		return arrayType{
			innerT:         l.freshen(limit, type_.innerT, rigidify, freshened),
			withProvenance: type_.withProvenance,
		}
	case classTag, traitTag:
		return type_

	case typeRef:
		var typeArgs []SimpleType = make([]SimpleType, len(type_.typeArgs))
		for i, typeArg := range type_.typeArgs {
			typeArgs[i] = l.freshen(limit, typeArg, rigidify, freshened)
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
