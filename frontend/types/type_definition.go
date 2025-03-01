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

var emptySetTypeName = immutable.NewSet[string](immutable.NewHasher(""))
var emptySetTypeID = immutable.NewSet[typeVariableID](immutable.NewHasher(uint(1)))

func (ctx *TypeCtx) rightParents(typeDef typeDefinition, prov *typeProvenance) (bool, error) {
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

func (ctx *TypeCtx) doTypeDefs(newDefs []typeDefinition) (*TypeCtx, error) {
	definitions := make(map[string]typeDefinition, len(ctx.typeDefs))
	// copy the map
	for def, t := range ctx.typeDefs {
		definitions[def] = t
	}
	for _, newDef := range newDefs {
		prov := &typeProvenance{positioner: newDef.from, desc: "type definition"}
		name := newDef.name
		rightParents, err := ctx.rightParents(newDef, prov)
		if err != nil {
			return nil, err
		}
		if rightParents && checkRegular(newDef.bodyType, nil) {
			definitions[name] = newDef
		}
	}

	newCtx := *ctx
	newCtx.typeDefs = definitions
	return &newCtx, nil
}

func checkRegular(typ simpleType, reached *immutable.Map[string, []simpleType]) bool {
	if reached == nil {
		reached = immutable.NewMap[string, []simpleType](immutable.NewHasher(""))
	}
	panic("implement me")
}
