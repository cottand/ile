package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
)

var builtinProv = newOriginProv(ast.Range{}, "builtin", "builtin")

// anyClassTag is called Object in the reference scala implementation
var anyClassTag = classTag{
	id:             &ast.Var{Name: ast.AnyBuiltinType},
	parents:        util.MSet[typeName]{},
	withProvenance: withProvenance{builtinProv},
}

var intType = classTag{
	id:             &ast.Var{Name: ast.IntBuiltinType},
	parents:        util.NewSetOf(ast.AnyBuiltinType),
	withProvenance: withProvenance{builtinProv},
}

// universe are the built-in bindings
func universe() map[string]typeInfo {
	return map[string]typeInfo{
		"+": funcType{
			args:           []SimpleType{intType, intType},
			ret:            intType,
			withProvenance: withProvenance{builtinProv},
		},
	}
}
