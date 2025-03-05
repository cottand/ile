package types

import (
	"errors"
	"fmt"
	"github.com/benbjohnson/immutable"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"iter"
	"reflect"
	"slices"
)

type typeDefKind uint8

const (
	_ typeDefKind = iota
	// 'class' is a whole new type in Go and ile semantics
	kindClass
	kindAlias
	kindTrait
)

func (k typeDefKind) String() string {
	switch k {
	case kindClass:
		return "class"
	case kindAlias:
		return "type alias"
	case kindTrait:
		return "trait"
	default:
		return "invalid"
	}
}

type typeDefinition struct {
	defKind       typeDefKind
	name          typeName
	typeParamArgs []util.Pair[typeName, typeVariable]
	typeVars      []typeVariable
	bodyType      simpleType
	baseClasses   immutable.Set[typeName]
	from          ast.Positioner
}

func (d *typeDefinition) allBaseClasses(ctx TypeCtx) immutable.Set[typeName] {
	builder := util.NewEmptySet[typeName]()
	d.allBaseClassesHelper(ctx, builder)
	return immutable.NewSet[typeName](nil, builder.AsSlice()...)
}

func (d *typeDefinition) allBaseClassesHelper(ctx TypeCtx, traversed util.MSet[typeName]) {
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

func (d *typeDefinition) typeParameters() iter.Seq[simpleType] {
	return func(yield func(simpleType) bool) {
		for _, arg := range d.typeParamArgs {
			if !yield(arg.Snd) {
				return
			}
		}
	}
}

func (ctx *TypeCtx) processTypeDefs(newDefs []ast.Type) *TypeCtx {
	panic("implement me")
}

var emptySetTypeName = immutable.NewSet[string](immutable.NewHasher(""))
var emptySetTypeID = immutable.NewSet[typeVariableID](immutable.NewHasher(uint(1)))

// typeTypeDefs processes newDefs and returns a new TypeCtx with the new names
func (ctx *TypeCtx) typeTypeDefs(newDefs []typeDefinition) (*TypeCtx, error) {
	definitions := make(map[string]typeDefinition, len(ctx.typeDefs))
	// copy the map
	for def, t := range ctx.typeDefs {
		definitions[def] = t
	}
	for _, newDef := range newDefs {
		prov := &typeProvenance{positioner: newDef.from, desc: "type definition"}
		name := newDef.name
		rightParents, err := ctx.typeDefrightParents(newDef, prov)
		if err != nil {
			return nil, err
		}
		regular, err := ctx.typeDefCheckRegular(newDef, newDef.bodyType, nil)
		if err != nil {
			return nil, err
		}
		if rightParents && regular {
			definitions[name] = newDef
		}
	}

	newCtx := *ctx
	newCtx.typeDefs = definitions
	return &newCtx, nil
}
func (ctx *TypeCtx) typeDefrightParents(typeDef typeDefinition, prov *typeProvenance) (bool, error) {
	switch typeDef.defKind {
	case kindAlias:
		return ctx.checkCycle(typeDef.bodyType, prov, emptySetTypeName, emptySetTypeID)
	case kindClass, kindTrait:
		if !ctx.checkParents(typeDef.bodyType) {
			return false, nil
		}
		cycleOk, err := ctx.checkCycle(typeDef.bodyType, prov, emptySetTypeName.Add(typeDef.name), emptySetTypeID)
		if err != nil {
			return false, fmt.Errorf("checking cycle of parent types: %w", err)
		}
		return cycleOk && ctx.checkAbstractAddConstructors(), nil
	}
	panic("unreachable: unknown type definition kind: " + typeDef.defKind.String())
}

func (ctx *TypeCtx) checkParents(typ simpleType) bool {
	panic("implement me")

}

// checkCycle returns true when type_ has no cycles
func (ctx *TypeCtx) checkCycle(
	type_ simpleType,
	prov *typeProvenance,
	traversedNames immutable.Set[typeName],
	traversedVars immutable.Set[typeVariableID],
) (bool, error) {
	switch typ := type_.(type) {
	case typeRef:
		if traversedNames.Has(typ.defName) {
			return false, fmt.Errorf("illegal cycle detected fpr type=%s, at=%v", typ.defName, prov)
		}
		return ctx.checkCycle(typ.expand(ctx), prov, traversedNames.Add(typ.defName), traversedVars)
	case unionType:
		left, err1 := ctx.checkCycle(typ.lhs, prov, traversedNames, traversedVars)
		right, err2 := ctx.checkCycle(typ.rhs, prov, traversedNames, traversedVars)
		return left && right, errors.Join(err1, err2)
	case negType:
		return ctx.checkCycle(typ.negated, prov, traversedNames, traversedVars)
	case typeRange:
		left, err1 := ctx.checkCycle(typ.upperBound, prov, traversedNames, traversedVars)
		right, err2 := ctx.checkCycle(typ.lowerBound, prov, traversedNames, traversedVars)
		return left && right, errors.Join(err1, err2)
	case typeVariable:
		if traversedVars.Has(typ.id) {
			return true, nil
		}
		varsAndThis := traversedVars.Add(typ.id)
		for _, bound := range typ.lowerBounds {
			ok, err := ctx.checkCycle(bound, prov, traversedNames, varsAndThis)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
		}
		for _, bound := range typ.upperBounds {
			ok, err := ctx.checkCycle(bound, prov, traversedNames, varsAndThis)
			if err != nil {
				return false, err
			}
			if !ok {
				return false, nil
			}
		}
		return true, nil
	case extremeType, objectTag, funcType, arrayBase:
		return true, nil

	default:
		panic("unreachable case hit for type: " + reflect.TypeOf(typ).String())
	}

}

// checkAbstractAddConstructors is defined as a variable 'checkAbstractAddCtors' in the
// scala mlstruct implementation
func (ctx *TypeCtx) checkAbstractAddConstructors() bool {
	panic("implement me")
}

func (ctx *TypeCtx) typeDefCheckRegular(typeDef typeDefinition, typ simpleType, reached *immutable.Map[string, []simpleType]) (bool, error) {
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
				return false, fmt.Errorf(
					"type definition is not regular - it occurs within itself as %s, but it is defined as %s, at: %s",
					typ.expand(ctx),
					typeRef{defName: typ.defName, typeArgs: slices.Collect(typeDef.typeParameters())}.expand(ctx),
					"TODO: LOC here",
				)
			}
			return true, nil
		}
		return ctx.typeDefCheckRegular(typeDef, typ.expandWith(ctx, false), reached.Set(typ.defName, typ.typeArgs))
	}
	for childT := range typ.children(false) {
		ok, err := ctx.typeDefCheckRegular(typeDef, childT, reached)
		if err != nil {
			return false, err
		}
		if !ok {
			return false, nil
		}
	}
	return true, nil
}
