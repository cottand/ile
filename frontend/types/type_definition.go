package types

import (
	"errors"
	"fmt"
	"github.com/benbjohnson/immutable"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"reflect"
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

func (ctx *TypeCtx) processTypeDefs(newDefs []ast.Type) *TypeCtx {
	panic("implement me")
}

func (ctx *TypeCtx) rightParents(def typeDefinition, prov *typeProvenance) (bool, error) {
	if def.defKind == kindAlias {
		return ctx.checkCycle(def.bodyType, prov, immutable.NewSet[typeName](nil), immutable.NewSet[typeVariableID](nil))
	}
	if def.defKind == kindClass || def.defKind == kindTrait {

	}
	panic("implement me")
}

func (ctx *TypeCtx) checkParents(typ simpleType) {
	panic("implement me")

}

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

func (ctx *TypeCtx) doTypeDefs(newDefs []ast.Type) *TypeCtx {
	oldDefs := make(map[string]typeDefinition, len(ctx.typeDefs))
	// copy the map
	for def, t := range ctx.typeDefs {
		oldDefs[def] = t
	}
	for _, newDef := range newDefs {
		_ = &typeProvenance{positioner: newDef, desc: "type definition"}

	}

	newCtx := *ctx
	newCtx.typeDefs = oldDefs
	return &newCtx
}
