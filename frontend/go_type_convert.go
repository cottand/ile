package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	gotypes "go/types"
)

func convertGoType(p gotypes.Type, in ast.Range) ast.Type {
	var ileType ast.Type
	switch t := p.(type) {
	case *gotypes.Basic:
		switch t.Kind() {
		case gotypes.Bool:
			return ast.BoolType
		case gotypes.Invalid:
		case gotypes.Int:
		case gotypes.Int8:
		case gotypes.Int16:
		case gotypes.Int64:
			return ast.IntType
		case gotypes.UntypedInt:
			return ast.IntLiteral(t.String(), in)
		case gotypes.Uint:
		case gotypes.Uint16:
		case gotypes.Uint32:
		case gotypes.Uint64:
		case gotypes.Uintptr:
		case gotypes.Float32:
		case gotypes.Float64:
			return ast.FloatLiteral(t.String(), in)
		case gotypes.Complex64:
		case gotypes.Complex128:
		case gotypes.String:
			return ast.StringLiteral(t.String(), in)
		case gotypes.UnsafePointer:
		case gotypes.UntypedBool:
		case gotypes.UntypedRune:
		case gotypes.UntypedFloat:
			return ast.FloatLiteral(t.String(), in)
		case gotypes.UntypedComplex:
		case gotypes.UntypedString:
		case gotypes.UntypedNil:
			return ast.NilType
		//gotypes.Byte:
		case gotypes.Uint8:
		//gotypes.Rune
		case gotypes.Int32:
		default:
			panic("unreachable")
		}
	case *gotypes.Signature:
		var ret ast.Type
		if t.Recv() == nil {
			ret = &ast.FnType{
				Return: ast.NilType,
			}
		}
		if t.Results().Len() > 1 {
			return nil
		}
		if t.Results().Len() == 1 {
			ret = convertGoType(t.Results().At(0).Type(), ast.Range{})
		}
		var params []ast.Type
		if t.Params() == nil {
			params = []ast.Type{}
		}
		for i := 0; i < t.Params().Len(); i++ {
			param := t.Params().At(i)
			params = append(params, convertGoType(param.Type(), ast.Range{}))
		}
		return &ast.FnType{
			Args:   params,
			Return: ret,
		}
	}
	return ileType
}
