package types

import (
	"fmt"
	"github.com/benbjohnson/immutable"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"iter"
	"reflect"
	"slices"
)

type TypeDefinition struct {
	defKind       ast.TypeDefKind
	name          typeName
	typeParamArgs []util.Pair[typeName, typeVariable]
	typeVars      []typeVariable
	bodyType      simpleType
	baseClasses   immutable.Set[typeName]
	from          ast.Positioner
}

func (d *TypeDefinition) allBaseClasses(ctx TypeCtx) immutable.Set[typeName] {
	builder := util.NewEmptySet[typeName]()
	d.allBaseClassesHelper(ctx, builder)
	return immutable.NewSet[typeName](nil, builder.AsSlice()...)
}

func (d *TypeDefinition) allBaseClassesHelper(ctx TypeCtx, traversed util.MSet[typeName]) {
	if traversed.Contains(d.name) {
		return
	}
	traversed.Add(d.name)
	for def := range util.SetIterator[typeName](d.baseClasses) {
		t, ok := ctx.typeDefs[def]
		if ok {
			t.allBaseClassesHelper(ctx, traversed)
		}
	}
}

func (d *TypeDefinition) typeParameters() iter.Seq[simpleType] {
	return func(yield func(simpleType) bool) {
		for _, arg := range d.typeParamArgs {
			if !yield(arg.Snd) {
				return
			}
		}
	}
}

func isNameReserved(name typeName) bool {
	panic("TODO implement me")
}

var emptySetTypeName = immutable.NewSet[string](immutable.NewHasher(""))
var emptySetTypeID = immutable.NewSet[typeVariableID](immutable.NewHasher(uint(1)))

// typeTypeDefs processes newDefs and returns a new TypeCtx with the new names
func (ctx *TypeCtx) typeTypeDefs(newDefs []TypeDefinition, oldDefs map[string]TypeDefinition) *TypeCtx {
	definitions := make(map[string]TypeDefinition, len(ctx.typeDefs))
	// copy the map
	for def, t := range oldDefs {
		definitions[def] = t
	}
	for _, newDef := range newDefs {
		prov := &typeProvenance{positioner: newDef.from, desc: "type definition"}
		name := newDef.name
		ctx.currentPos = prov.positioner
		rightParents := ctx.typeDefRightParents(newDef)
		regular := ctx.typeDefCheckRegular(newDef, newDef.bodyType, nil, prov.positioner)
		if rightParents && regular {
			definitions[name] = newDef
		}
	}

	newCtx := *ctx
	newCtx.typeDefs = definitions
	return &newCtx
}

func (ctx *TypeCtx) typeDefRightParents(typeDef TypeDefinition) bool {
	switch typeDef.defKind {
	case ast.KindAlias:
		return ctx.checkCycle(typeDef.bodyType, emptySetTypeName, emptySetTypeID)
	case ast.KindClass, ast.KindTrait:
		if !ctx.checkParents(typeDef, typeDef.bodyType, nil) {
			return false
		}
		return ctx.checkParents(typeDef, typeDef.bodyType, nil) &&
			ctx.checkCycle(typeDef.bodyType, emptySetTypeName.Add(typeDef.name), emptySetTypeID) &&
			ctx.checkAbstractAddConstructors()
	}
	panic("unreachable: unknown type definition kind: " + typeDef.defKind.String())
}

// checkParents is called by typeDefRightParents
func (ctx *TypeCtx) checkParents(originalTypeDef TypeDefinition, typ simpleType, parentsClasses []typeRef) bool {
	switch typ := typ.(type) {
	case objectTag:
		return true
	case typeRef:
		otherTypeDef, ok := ctx.typeDefs[typ.defName]
		if !ok {
			// corresponds to
			// https://github.com/hkust-taco/mlstruct/blob/mlstruct/shared/src/main/scala/mlscript/TypeDefs.scala#L234
			// which does not seem to handle the missing case, as the map has no default
			ctx.addFailure("compiler error: unhandled branch path for fetching type definition", ctx.currentPos)
			panic("TODO handle missing handle of branch")
		}
		switch otherTypeDef.defKind {
		case ast.KindClass:
			if originalTypeDef.defKind == ast.KindClass {
				parentsClasses = append(parentsClasses, typ)
				if len(parentsClasses) != 0 {
					ctx.addFailure(fmt.Sprintf("%s %s cannot inherit from class %s as it already inherits from %s", originalTypeDef.defKind.String(), originalTypeDef.name, otherTypeDef.name, parentsClasses[0]), ctx.currentPos)
					return false
				}
				return true
			} else {
				return ctx.checkParents(originalTypeDef, ctx.expand(typ), parentsClasses)
			}
		case ast.KindAlias:
			ctx.addFailure("cannot inherit from type alias", ctx.currentPos)
			return false
		case ast.KindTrait:
			return ctx.checkParents(originalTypeDef, ctx.expand(typ), parentsClasses)
		default:
			panic("unreachable: unexpected type definition kind: " + otherTypeDef.defKind.String())
		}
	default:
		panic("unreachable: unknown type reference: " + typ.String() + "of type" + reflect.TypeOf(typ).String())
	}

}

// checkCycle returns true when type_ has no cycles
func (ctx *TypeCtx) checkCycle(
	type_ simpleType,
	traversedNames immutable.Set[typeName],
	traversedVars immutable.Set[typeVariableID],
) bool {
	switch typ := type_.(type) {
	case typeRef:
		if traversedNames.Has(typ.defName) {
			ctx.addFailure(fmt.Sprintf("illegal cycle detected fpr type=%s", typ.defName), ctx.currentPos)
			return false
		}
		return ctx.checkCycle(ctx.expand(typ), traversedNames.Add(typ.defName), traversedVars)
	case unionType:
		return ctx.checkCycle(typ.lhs, traversedNames, traversedVars) &&
			ctx.checkCycle(typ.rhs, traversedNames, traversedVars)
	case negType:
		return ctx.checkCycle(typ.negated, traversedNames, traversedVars)
	case typeRange:
		return ctx.checkCycle(typ.upperBound, traversedNames, traversedVars) &&
			ctx.checkCycle(typ.lowerBound, traversedNames, traversedVars)
	case typeVariable:
		if traversedVars.Has(typ.id) {
			return true
		}
		varsAndThis := traversedVars.Add(typ.id)
		for _, bound := range typ.lowerBounds {
			if !ctx.checkCycle(bound, traversedNames, varsAndThis) {
				return false
			}
		}
		for _, bound := range typ.upperBounds {
			if !ctx.checkCycle(bound, traversedNames, varsAndThis) {
				return false
			}
		}
		return true
	case extremeType, objectTag, funcType, arrayBase:
		return true

	default:
		panic("unreachable case hit for type: " + reflect.TypeOf(typ).String())
	}

}

// checkAbstractAddConstructors is defined as a variable 'checkAbstractAddCtors' in the
// scala mlstruct implementation
// it is memoized as a lazy val in the original implementation but is in fact only called once
// so it can be implemented as a function
// invariant: only called once during typeDefRightParents
func (ctx *TypeCtx) checkAbstractAddConstructors() bool {
	panic("TODO implement me (methods necessary)")
}

// implementation wise it is completed but TODO make a typeError to accumulate failures with their locations
func (ctx *TypeCtx) typeDefCheckRegular(typeDef TypeDefinition, typ simpleType, reached *immutable.Map[string, []simpleType], pos ast.Positioner) bool {
	if reached == nil {
		reached = immutable.NewMap[string, []simpleType](immutable.NewHasher(""))
	}
	if typ, ok := typ.(typeRef); ok {
		if existingTs, isPresent := reached.Get(typ.defName); isPresent {
			// Note: this check *has* to be relatively syntactic because
			//    the termination of constraint solving relies on recursive type occurrences
			//    obtained from unrolling a recursive type to be *equal* to the original type
			//    and to have the same has hashCode (see: the use of a cache MutSet)
			if typeDef.name == typ.defName && !util.SlicesEquivalent(existingTs, typ.typeArgs) {
				ctx.addFailure(fmt.Sprintf(
					"type definition is not regular - it occurs within itself as %s, but it is defined as %s, at: %s",
					ctx.expand(typ),
					ctx.expand(typeRef{defName: typ.defName, typeArgs: slices.Collect(typeDef.typeParameters())})), pos)
				return false

			}
			return true
		}
		return ctx.typeDefCheckRegular(typeDef, ctx.expandWith(typ, false), reached.Set(typ.defName, typ.typeArgs), nil)
	}
	for childT := range typ.children(false) {
		if !ctx.typeDefCheckRegular(typeDef, childT, reached, pos) {
			return false
		}
	}
	return true
}
