package types

import (
	"fmt"
	"go/constant"
	gotypes "go/types"

	"github.com/cottand/ile/frontend/ir"
)

func isAny(t gotypes.Type) bool {
	interf, ok := t.Underlying().(*gotypes.Interface)
	return ok && interf.NumMethods() == 0
}

func convertGoType(obj gotypes.Object) ir.Type {
	pkg := obj.Pkg().Path()
	prov := typeProvenance{
		originPackage: pkg,
		desc:          "go type",
		Range: ir.Range{
			PosStart: obj.Pos(),
			PosEnd:   obj.Pos(),
		},
	}
	// ideally we embed all the provenance info, but we are dealing with ir.Type here so we
	// will just set the range for hash deduping
	in := prov.Range

	var ileType ir.Type
	asConst, _ := obj.(*gotypes.Const)
	switch t := obj.Type().(type) {
	case *gotypes.Alias:
		if isAny(t) {
			return &ir.AnyType{
				Positioner: in,
			}
		}
		panic("handle alias " + obj.Name())
	case *gotypes.Basic:
		switch t.Kind() {
		case gotypes.Bool:
			return ir.BoolType
		case gotypes.Invalid:
		case gotypes.Int:
			// TODO account for 32-bit systems
			return ir.IntType
		case gotypes.Int8:
		case gotypes.Int16:
		case gotypes.Int64:
			return ir.IntType
		case gotypes.UntypedInt:
			return ir.IntLiteral(asConst.Val().ExactString(), in)
		case gotypes.Uint:
		case gotypes.Uint16:
		case gotypes.Uint32:
		case gotypes.Uint64:
		case gotypes.Uintptr:
		case gotypes.Float32:
		case gotypes.Float64:
			return ir.FloatType
		case gotypes.Complex64:
		case gotypes.Complex128:
		case gotypes.String:
			return ir.StringType
		case gotypes.UnsafePointer:
		case gotypes.UntypedBool:
			return ir.NewBool(constant.BoolVal(asConst.Val()), in)
		case gotypes.UntypedRune:
		case gotypes.UntypedFloat:
			return ir.FloatLiteral(t.String(), in)
		case gotypes.UntypedComplex:
		case gotypes.UntypedString:
			return ir.StringLiteral(asConst.Val().ExactString(), in)
		case gotypes.UntypedNil:
			return ir.NilType
		//gotypes.Byte:
		case gotypes.Uint8:
		//gotypes.Rune
		case gotypes.Int32:
		default:
			panic("unreachable")
		}
	case *gotypes.Slice:
		goType := convertGoType(gotypes.NewTypeName(obj.Pos(), obj.Pkg(), obj.Name(), t.Elem()))
		if goType == nil {
			return nil
		}
		return &ir.AppliedType{
			Base: ir.TypeName{
				Name:       ir.ListTypeName,
				Range:      ir.Range{},
				Provenance: "go slice",
			},
			Args: []ir.Type{goType},
		}
	case *gotypes.Signature:
		var ret ir.Type
		if t.Recv() == nil {
			ret = &ir.FnType{
				Return: ir.NilType,
			}
		}
		if t.Results().Len() > 1 {
			return nil
		}
		if t.Results().Len() == 1 {
			ret = convertGoType(t.Results().At(0))
		}
		var params []ir.Type
		if t.Params() == nil {
			params = []ir.Type{}
		}
		for i := range t.Params().Len() {
			param := t.Params().At(i)

			goType := convertGoType(param)
			if goType == nil {
				return nil
			}
			var next ir.Type
			//if t.Variadic() && i == t.Params().Len()-1 {
			//	next = &ir.ListType{ElementType: goType}
			//} else {
			next = goType
			//}
			params = append(params, next)
		}
		return &ir.FnType{
			Args:   params,
			Return: ret,
		}
	}
	return ileType
}


func goProvenanceFor(object gotypes.Object) typeProvenance {
	return typeProvenance{
		Range: ir.Range{
			PosStart: object.Pos(),
			PosEnd:   object.Pos(),
		},
		desc:             fmt.Sprintf("imported go value"),
		isType:           true,
		originName:       object.Name(),
		originPackage:    object.Pkg().Path(),
		underlyingGoType: object,
	}
}
