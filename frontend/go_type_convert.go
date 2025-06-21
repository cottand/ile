package frontend

import (
	"github.com/cottand/ile/frontend/ir"
	gotypes "go/types"
)

func convertGoType(obj gotypes.Object, in ir.Range) ir.Type {
	var ileType ir.Type
	asConst, _ := obj.(*gotypes.Const)
	switch t := obj.Type().(type) {
	case *gotypes.Basic:
		switch t.Kind() {
		case gotypes.Bool:
			return ir.BoolType
		case gotypes.Invalid:
		case gotypes.Int:
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
			return ir.FloatLiteral(t.String(), in)
		case gotypes.Complex64:
		case gotypes.Complex128:
		case gotypes.String:
			return ir.StringLiteral(t.String(), in)
		case gotypes.UnsafePointer:
		case gotypes.UntypedBool:
		case gotypes.UntypedRune:
		case gotypes.UntypedFloat:
			return ir.FloatLiteral(t.String(), in)
		case gotypes.UntypedComplex:
		case gotypes.UntypedString:
		case gotypes.UntypedNil:
			return ir.NilType
		//gotypes.Byte:
		case gotypes.Uint8:
		//gotypes.Rune
		case gotypes.Int32:
		default:
			panic("unreachable")
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
			ret = convertGoType(t.Results().At(0), ir.Range{})
		}
		var params []ir.Type
		if t.Params() == nil {
			params = []ir.Type{}
		}
		for i := 0; i < t.Params().Len(); i++ {
			param := t.Params().At(i)
			params = append(params, convertGoType(param, ir.Range{}))
		}
		return &ir.FnType{
			Args:   params,
			Return: ret,
		}
	}
	return ileType
}
