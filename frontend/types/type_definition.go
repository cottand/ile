package types

import (
	"fmt"
	"github.com/benbjohnson/immutable"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"iter"
	"reflect"
	"slices"
)

type TypeDefinition struct {
	defKind       ir.TypeDefKind
	name          typeName
	typeParamArgs []util.Pair[typeName, *typeVariable]
	typeVars      []typeVariable
	// typeVarVariances correctly returns invariant when not found, because it is the zero value of varianceInfo
	typeVarVariances map[TypeVarID]varianceInfo
	bodyType         SimpleType
	baseClasses      set.Collection[typeName]
	from             ir.Positioner
}

func (d *TypeDefinition) allBaseClasses(ctx TypeCtx) set.Collection[typeName] {
	builder := set.New[typeName](1)
	d.allBaseClassesHelper(ctx, builder)
	return builder
}

func (d *TypeDefinition) allBaseClassesHelper(ctx TypeCtx, traversed set.Collection[typeName]) {
	if traversed.Contains(d.name) {
		return
	}
	traversed.Insert(d.name)
	for def := range d.baseClasses.Items() {
		t, ok := ctx.typeDefs[def]
		if ok {
			t.allBaseClassesHelper(ctx, traversed)
		}
	}
}

func (d *TypeDefinition) typeParameters() iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		for _, arg := range d.typeParamArgs {
			if !yield(arg.Snd) {
				return
			}
		}
	}
}

func isNameReserved(name typeName) bool {
	return false
}

var emptySetTypeName = immutable.NewSet[string](immutable.NewHasher(""))
var emptySetTypeID = immutable.NewSet[TypeVarID](immutable.NewHasher(uint64(1)))

// typeTypeDefs processes newDefs and returns a new TypeCtx with the new names
func (ctx *TypeCtx) typeTypeDefs(newDefs []TypeDefinition, oldDefs map[string]TypeDefinition) *TypeCtx {
	definitions := make(map[string]TypeDefinition, len(ctx.typeDefs))
	// copy the map
	for def, t := range oldDefs {
		definitions[def] = t
	}
	for _, newDef := range newDefs {
		prov := &typeProvenance{Range: ir.RangeOf(newDef.from), desc: "type definition"}
		name := newDef.name
		ctx.currentPos = prov.Range
		rightParents := ctx.typeDefRightParents(newDef)
		regular := ctx.typeDefCheckRegular(newDef, newDef.bodyType, nil, prov.Range)
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
	case ir.KindAlias:
		return ctx.checkCycle(typeDef.bodyType, emptySetTypeName, emptySetTypeID)
	case ir.KindClass, ir.KindTrait:
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
func (ctx *TypeCtx) checkParents(originalTypeDef TypeDefinition, typ SimpleType, parentsClasses []typeRef) bool {
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
		case ir.KindClass:
			if originalTypeDef.defKind == ir.KindClass {
				parentsClasses = append(parentsClasses, typ)
				if len(parentsClasses) != 0 {
					ctx.addFailure(fmt.Sprintf("%s %s cannot inherit from class %s as it already inherits from %s", originalTypeDef.defKind.String(), originalTypeDef.name, otherTypeDef.name, parentsClasses[0]), ctx.currentPos)
					return false
				}
				return true
			} else {
				return ctx.checkParents(originalTypeDef, ctx.expand(typ, expandOpts{}), parentsClasses)
			}
		case ir.KindAlias:
			ctx.addFailure("cannot inherit from type alias", ctx.currentPos)
			return false
		case ir.KindTrait:
			return ctx.checkParents(originalTypeDef, ctx.expand(typ, expandOpts{}), parentsClasses)
		default:
			panic("unreachable: unexpected type definition kind: " + otherTypeDef.defKind.String())
		}
	default:
		panic("unreachable: unknown type reference: " + typ.String() + "of type" + reflect.TypeOf(typ).String())
	}

}

// checkCycle returns true when type_ has no cycles
func (ctx *TypeCtx) checkCycle(
	type_ SimpleType,
	traversedNames immutable.Set[typeName],
	traversedVars immutable.Set[TypeVarID],
) bool {
	switch typ := type_.(type) {
	case typeRef:
		if traversedNames.Has(typ.defName) {
			ctx.addFailure(fmt.Sprintf("illegal cycle detected fpr type=%s", typ.defName), ctx.currentPos)
			return false
		}
		return ctx.checkCycle(ctx.expand(typ, expandOpts{}), traversedNames.Add(typ.defName), traversedVars)
	case unionType:
		return ctx.checkCycle(typ.lhs, traversedNames, traversedVars) &&
			ctx.checkCycle(typ.rhs, traversedNames, traversedVars)
	case negType:
		return ctx.checkCycle(typ.negated, traversedNames, traversedVars)
	case typeRange:
		return ctx.checkCycle(typ.upperBound, traversedNames, traversedVars) &&
			ctx.checkCycle(typ.lowerBound, traversedNames, traversedVars)
	case *typeVariable:
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
	case extremeType, objectTag, funcType, arrayBase, recordType:
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

// implementation wise it is completed but TODO make a typeError to accumulate Failures with their locations
func (ctx *TypeCtx) typeDefCheckRegular(typeDef TypeDefinition, typ SimpleType, reached *immutable.Map[string, []SimpleType], pos ir.Positioner) bool {
	if reached == nil {
		reached = immutable.NewMap[string, []SimpleType](immutable.NewHasher(""))
	}
	if typ, ok := typ.(typeRef); ok {
		if existingTs, isPresent := reached.Get(typ.defName); isPresent {
			// Note: this check *has* to be relatively syntactic because
			//    the termination of constraint solving relies on recursive type occurrences
			//    obtained from unrolling a recursive type to be *equal* to the original type
			//    and to have the same has hashCode (see: the use of a cache MutSet)
			if typeDef.name == typ.defName && !util.SlicesEquivalent(existingTs, typ.typeArgs) {
				ctx.addFailure(fmt.Sprintf(
					"type definition is not regular - it occurs within itself as %s, but it is defined as %s",
					ctx.expand(typ, expandOpts{}),
					ctx.expand(typeRef{defName: typ.defName, typeArgs: slices.Collect(typeDef.typeParameters())}, expandOpts{})), pos)
				return false

			}
			return true
		}
		return ctx.typeDefCheckRegular(typeDef, ctx.expand(typ, expandOpts{withoutParamTags: true}), reached.Set(typ.defName, typ.typeArgs), nil)
	}
	for childT := range typ.children(false) {
		if !ctx.typeDefCheckRegular(typeDef, childT, reached, pos) {
			return false
		}
	}
	return true
}
