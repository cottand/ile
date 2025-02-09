package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	gotypes "go/types"
)

func convertGoType(p gotypes.Type) ast.TypeAnnotation {
	var ileType ast.TypeAnnotation
	switch t := p.(type) {
	case *gotypes.Basic:
		switch t.Kind() {
		case gotypes.Bool:
			return ast.TConst{Name: "Bool"}
		case gotypes.Invalid:
		case gotypes.Int:
		case gotypes.Int8:
		case gotypes.Int16:
		case gotypes.Int64:
			return ast.TConst{Name: "Int"}
		case gotypes.UntypedInt:
			return ast.TComptimeInt{}
		case gotypes.Uint:
		case gotypes.Uint16:
		case gotypes.Uint32:
		case gotypes.Uint64:
		case gotypes.Uintptr:
		case gotypes.Float32:
		case gotypes.Float64:
			return ast.TConst{Name: "Float"}
		case gotypes.Complex64:
		case gotypes.Complex128:
		case gotypes.String:
			return ast.TConst{Name: "String"}
		case gotypes.UnsafePointer:
		case gotypes.UntypedBool:
		case gotypes.UntypedRune:
		case gotypes.UntypedFloat:
			return ast.TConst{Name: "Float"}
		case gotypes.UntypedComplex:
		case gotypes.UntypedString:
		case gotypes.UntypedNil:
			return ast.TConst{Name: "Nil"}
		//gotypes.Byte:
		case gotypes.Uint8:
		//gotypes.Rune
		case gotypes.Int32:
		default:
			panic("unreachable")
		}
	case *gotypes.Signature:
		var ret ast.TypeAnnotation
		if t.Recv() == nil {
			ret = ast.TArrow{
				Return: ast.TConst{
					Name: "Nil",
				},
			}
		}
		if t.Results().Len() > 1 {
			return nil
		}
		if t.Results().Len() == 1 {
			ret = convertGoType(t.Results().At(0).Type())
		}
		var params []ast.TypeAnnotation
		if t.Params() == nil {
			params = []ast.TypeAnnotation{}
		}
		for i := 0; i < t.Params().Len(); i++ {
			param := t.Params().At(i)
			params = append(params, convertGoType(param.Type()))
		}
		return ast.TArrow{
			Args:   params,
			Return: ret,
		}
	}
	return ileType
}
