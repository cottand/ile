package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"maps"
	"slices"
)

// typeIrType returns the SimpleType corresponding to an ast.Type
// it is called typeType in the Scala reference
func (ctx *TypeCtx) typeIrType(
	typ ir.Type,
	vars map[typeName]SimpleType,
	dontSimplify bool,
	newDefsInfo map[string]util.Pair[ir.TypeDefKind, int],
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
	newDefsInfo  map[string]util.Pair[ir.TypeDefKind, int]
	localVars    map[typeName]typeVariable
	tempVars     map[string]SimpleType
}

func (ctx *typeAstTypeContext) typeNamedType(name typeName, pos ir.Range) (kind ir.TypeDefKind, nParams int, ok bool) {
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
	return ir.TypeDefKind(0), 0, false
}
func (ctx *typeAstTypeContext) typeAstTypeRec(typ ir.Type) SimpleType {

	switch typ := typ.(type) {
	case *ir.AnyType:
		return extremeType{
			polarity: false,
			withProvenance: typeProvenance{
				Range:  ir.RangeOf(typ),
				desc:   "any type",
				isType: true,
			}.embed(),
		}
	case *ir.NothingType:
		return extremeType{
			polarity: true,
			withProvenance: typeProvenance{
				Range:  ir.RangeOf(typ),
				desc:   "nothing type",
				isType: true,
			}.embed(),
		}
	case *ir.IntersectionType:
		opts := unionOpts{
			prov: typeProvenance{
				Range:  ir.RangeOf(typ),
				desc:   "type intersection",
				isType: true,
			},
		}
		return intersectionOf(ctx.typeAstTypeRec(typ.Left), ctx.typeAstTypeRec(typ.Right), opts)
	case *ir.UnionType:
		opts := unionOpts{
			prov: typeProvenance{
				Range:  ir.RangeOf(typ),
				desc:   "type union",
				isType: true,
			},
		}
		return unionOf(ctx.typeAstTypeRec(typ.Left), ctx.typeAstTypeRec(typ.Right), opts)

	case *ir.TypeName:
		prov := typeProvenance{
			Range:  ir.RangeOf(typ),
			desc:   "type name",
			isType: true,
		}
		foundVar, ok := ctx.vars[typ.Name]
		if ok {
			return foundVar
		}
		_, nParams, ok := ctx.typeNamedType(typ.Name, ir.RangeOf(typ))
		if !ok {
			return errorType()
		}
		if nParams != 0 {
			ctx.addError(ilerr.New(ilerr.NewExpectedTypeParams{
				Positioner:     ir.RangeOf(typ),
				Name:           typ.Name,
				ExpectedParams: nParams,
			}))
			return errorType()
		}
		return typeRef{
			defName:        typ.Name,
			withProvenance: prov.embed(),
		}
	case *ir.FnType:
		var retType SimpleType
		if typ.Return == nil {
			retType = ctx.newTypeVariable(typeProvenance{Range: typ.Range, desc: "type declaration", isType: true}, "", nil, nil)
		} else {
			retType = ctx.typeAstTypeRec(typ.Return)
		}
		prov := typeProvenance{
			Range:  ir.RangeOf(typ),
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
	case *ir.Literal:
		return classTag{
			id:      typ,
			parents: typ.BaseTypes(),
			withProvenance: typeProvenance{
				Range:  typ.Range,
				desc:   "type literal",
				isType: true,
			}.embed(),
		}
	case *ir.RecordType:
		prov := typeProvenance{
			Range: ir.RangeOf(typ),
			desc:  "record type",
		}

		labelOccurrences := make(map[string][]ir.Positioner, len(typ.Fields))
		for _, nameField := range typ.Fields {
			name, field := nameField.Name, nameField.Type
			labelOccurrences[name.Name] = append(labelOccurrences[name.Name], field)
		}

		for label, positioners := range labelOccurrences {
			if len(positioners) > 1 {
				ctx.addError(ilerr.New(ilerr.NewRepeatedRecordField{
					Positioner: typ,
					Names:      positioners,
					Name:       label,
				}))
			}
		}
		record := recordType{
			fields:         nil,
			withProvenance: prov.embed(),
		}
		for _, nameField := range typ.Fields {
			name, field := nameField.Name, nameField.Type

			valueType := ctx.typeAstTypeRec(field.Out)
			if field.In != nil {
				ctx.addFailure(fmt.Sprintf("unsupported non-readonly field for record type in %v", field), field.In)
			}
			fieldProv := typeProvenance{
				Range: ir.RangeOf(field),
				desc:  "record field",
			}
			record.fields = append(record.fields, recordField{
				name: name,
				type_: newFieldTypeUpperBound(valueType, fieldProv),
			})
		}
		return newRecordType(record)
	default:
		panic(fmt.Sprintf("typeIrType: implement me for the %T %s", typ, ir.TypeString(typ)))
	}
}
