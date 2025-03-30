package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"reflect"
)

type level uint16

func (l level) freshTypeVar(prov *typeProvenance, nameHint string, lowerBounds, upperBounds []simpleType) typeVariable {
	panic("TODO implement me")
}

func (l level) freshenAbove(limit level, type_ simpleType) simpleType {
	return l.freshenAboveWithRigidify(limit, type_, false)
}

/*
	def freshenAbove(lim: Int, ty: SimpleType, rigidify: Bool = false)(implicit lvl: Int): SimpleType = {
	  val freshened = MutMap.empty[TV, SimpleType]
	  def freshen(ty: SimpleType): SimpleType =
	    if (!rigidify // Rigidification now also substitutes TypeBound-s with fresh vars;
	                  // since these have the level of their bounds, when rigidifying
	                  // we need to make sure to copy the whole type regardless of level...
	      && ty.level <= lim) ty else ty match {
	    case tv: TypeVariable => freshened.get(tv) match {
	      case Some(tv) => tv
	      case None if rigidify =>
	        val rv = TraitTag( // Rigid type variables (ie, skolems) are encoded as TraitTag-s
	          Var(tv.nameHint.getOrElse("_"+freshVar(noProv).toString)))(tv.prov)
	        if (tv.lowerBounds.nonEmpty || tv.upperBounds.nonEmpty) {
	          // The bounds of `tv` may be recursive (refer to `tv` itself),
	          //    so here we create a fresh variabe that will be able to tie the presumed recursive knot
	          //    (if there is no recursion, it will just be a useless type variable)
	          val tv2 = freshVar(tv.prov, tv.nameHint)
	          freshened += tv -> tv2
	          // Assuming there were no recursive bounds, given L <: tv <: U,
	          //    we essentially need to turn tv's occurrence into the type-bounds (rv | L)..(rv & U),
	          //    meaning all negative occurrences should be interpreted as rv | L
	          //    and all positive occurrences should be interpreted as rv & U
	          //    where rv is the rigidified variables.
	          // Now, since there may be recursive bounds, we do the same
	          //    but through the indirection of a type variable tv2:
	          tv2.lowerBounds ::= tv.lowerBounds.map(freshen).foldLeft(rv: ST)(_ & _)
	          tv2.upperBounds ::= tv.upperBounds.map(freshen).foldLeft(rv: ST)(_ | _)
	          tv2
	        } else {
	          freshened += tv -> rv
	          rv
	        }
	      case None =>
	        val v = freshVar(tv.prov, tv.nameHint)
	        freshened += tv -> v
	        v.lowerBounds = tv.lowerBounds.mapConserve(freshen)
	        v.upperBounds = tv.upperBounds.mapConserve(freshen)
	        v
	    }
	    case t @ TypeRange(lb, ub) =>
	      if (rigidify) {
	        val tv = freshVar(t.prov)
	        tv.lowerBounds ::= freshen(lb)
	        tv.upperBounds ::= freshen(ub)
	        tv
	      } else TypeRange(freshen(lb), freshen(ub))(t.prov)
	    case t @ FunctionType(l, r) => FunctionType(freshen(l), freshen(r))(t.prov)
	    case t @ ComposedType(p, l, r) => ComposedType(p, freshen(l), freshen(r))(t.prov)
	    case t @ RecordType(fs) => RecordType(fs.mapValues(_.update(freshen, freshen)))(t.prov)
	    case t @ TupleType(fs) => TupleType(fs.mapValues(freshen))(t.prov)
	    case t @ ArrayType(ar) => ArrayType(freshen(ar))(t.prov)
	    case n @ NegType(neg) => NegType(freshen(neg))(n.prov)
	    case e @ ExtrType(_) => e
	    case p @ ProvType(und) => ProvType(freshen(und))(p.prov)
	    case p @ ProxyType(und) => freshen(und)
	    case _: ClassTag | _: TraitTag => ty
	    case tr @ TypeRef(d, ts) => TypeRef(d, ts.map(freshen(_)))(tr.prov)
	  }
	  freshen(ty)
	}
*/
func (l level) freshenAboveWithRigidify(limit level, type_ simpleType, rigidify bool) simpleType {
	return l.freshen(limit, type_, rigidify, make(map[typeVariableID]simpleType))
}

func (l level) freshen(limit level, type_ simpleType, rigidify bool, freshened map[typeVariableID]simpleType) simpleType {
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
		// so we have rigidify

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
		var currentLowerBound simpleType = topType // we use the fact that A & Top = A
		for _, lb := range type_.lowerBounds {
			freshLb := l.freshen(limit, lb, rigidify, freshened)
			currentLowerBound = intersectionOf(currentLowerBound, freshLb, unionOpts{})
		}
		freshV.lowerBounds = append(freshV.lowerBounds, currentLowerBound)

		var currentUpperBound simpleType = bottomType // we use the fact that A | Bot = A
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
		return funcType{
			args: l.freshen(limit, type_.args, rigidify, freshened),
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
		var fields []simpleType = make([]simpleType, len(type_.fields))
		for i, field := range type_.fields {
			fields[i] = l.freshen(limit, field, rigidify, freshened)
		}
		return tupleType{
			fields:         fields,
			withProvenance: type_.withProvenance,
		}
	case namedTupleType:
		var fields []util.Pair[ast.Var, simpleType] = make([]util.Pair[ast.Var, simpleType], len(type_.fields))
		for i, field := range type_.fields {
			fields[i] = util.Pair[ast.Var, simpleType]{
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
		var typeArgs []simpleType = make([]simpleType, len(type_.typeArgs))
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
