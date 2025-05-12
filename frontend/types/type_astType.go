package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/util"
	"maps"
	"slices"
)

// typeAstType returns the SimpleType corresponding to an ast.Type
// it is called typeType in the Scala reference
func (ctx *TypeCtx) typeAstType(
	typ ast.Type,
	vars map[typeName]SimpleType,
	dontSimplify bool,
	newDefsInfo map[string]util.Pair[ast.TypeDefKind, int],
) (ret SimpleType, typeVars []typeVariable) {
	defer func() {
		logger.Debug("assigned type representation to ast.Type", "ast.Type", typ, "type", ret)
	}()
	localVars := make(map[typeName]typeVariable)
	newCtx := &typeAstTypeContext{
		TypeCtx:      ctx,
		vars:         vars,
		dontSimplify: dontSimplify,
		newDefsInfo:  newDefsInfo,
		tempVars:     make(map[string]SimpleType),
		localVars:    localVars,
	}
	ret = newCtx.typeAstTypeRec(typ)
	return ret, slices.Collect(maps.Values(localVars))
}

type typeAstTypeContext struct {
	*TypeCtx
	vars         map[string]SimpleType
	dontSimplify bool
	newDefsInfo  map[string]util.Pair[ast.TypeDefKind, int]
	localVars    map[typeName]typeVariable
	tempVars     map[string]SimpleType
}

func (ctx *typeAstTypeContext) typeNamedType(name typeName, pos ast.Range) (kind ast.TypeDefKind, nParams int, ok bool) {
	foundInNewDefs, ok := ctx.newDefsInfo[name]
	if ok {
		return foundInNewDefs.Fst, foundInNewDefs.Snd, true
	}
	foundTypeDef, ok := ctx.typeDefs[name]
	if ok {
		return foundTypeDef.defKind, len(foundTypeDef.typeVars), true
	}
	ctx.addError(ilerr.New(ilerr.NewUndefinedVariable{
		Positioner: pos,
		Name:       name,
	}))
	return ast.TypeDefKind(0), 0, false
}
func (ctx *typeAstTypeContext) typeAstTypeRec(typ ast.Type) SimpleType {

	switch typ := typ.(type) {
	case *ast.AnyType:
		return extremeType{
			polarity: false,
			withProvenance: typeProvenance{
				Range:  ast.RangeOf(typ),
				desc:   "any type",
				isType: true,
			}.embed(),
		}
	case *ast.NothingType:
		return extremeType{
			polarity: true,
			withProvenance: typeProvenance{
				Range:  ast.RangeOf(typ),
				desc:   "nothing type",
				isType: true,
			}.embed(),
		}
	case *ast.IntersectionType:
		opts := unionOpts{
			prov: typeProvenance{
				Range:  ast.RangeOf(typ),
				desc:   "type intersection",
				isType: true,
			},
		}
		return intersectionOf(ctx.typeAstTypeRec(typ.Left), ctx.typeAstTypeRec(typ.Right), opts)
	case *ast.UnionType:
		opts := unionOpts{
			prov: typeProvenance{
				Range:  ast.RangeOf(typ),
				desc:   "type union",
				isType: true,
			},
		}
		return unionOf(ctx.typeAstTypeRec(typ.Left), ctx.typeAstTypeRec(typ.Right), opts)

	case *ast.TypeName:
		prov := typeProvenance{
			Range:  ast.RangeOf(typ),
			desc:   "type name",
			isType: true,
		}
		foundVar, ok := ctx.vars[typ.Name]
		if ok {
			return foundVar
		}
		_, nParams, ok := ctx.typeNamedType(typ.Name, ast.RangeOf(typ))
		if !ok {
			return errorType()
		}
		if nParams != 0 {
			ctx.addError(ilerr.New(ilerr.NewExpectedTypeParams{
				Positioner:     ast.RangeOf(typ),
				Name:           typ.Name,
				ExpectedParams: nParams,
			}))
			return errorType()
		}
		return typeRef{
			defName:        typ.Name,
			withProvenance: prov.embed(),
		}
	case *ast.FnType:
		var retType SimpleType
		if typ.Return == nil {
			retType = ctx.newTypeVariable(typeProvenance{Range: typ.Range, desc: "type declaration", isType: true}, "", nil, nil)
		} else {
			retType = ctx.typeAstTypeRec(typ.Return)
		}
		prov := typeProvenance{
			Range:  ast.RangeOf(typ),
			desc:   "function type",
			isType: true,
		}
		argTypes := make([]SimpleType, 0, len(typ.Args))
		for _, arg := range typ.Args {
			var argType SimpleType
			if arg == nil {
				argType = ctx.newTypeVariable(typeProvenance{Range: typ.Range, desc: "type declaration", isType: true}, "", nil, nil)
			} else {
				argType = ctx.typeAstTypeRec(arg)
			}
			argTypes = append(argTypes, argType)
		}
		return funcType{
			args:           argTypes,
			ret:            retType,
			withProvenance: prov.embed(),
		}
	default:
		panic(fmt.Sprintf("typeAstType: implement me for: %s (%T)", ast.TypeString(typ), typ))
	}
}
