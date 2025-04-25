package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/hashicorp/go-set/v3"
)

var builtinProv = newOriginProv(ast.Range{}, "builtin", "builtin")

// anyClassTag is called Object in the reference scala implementation
var anyClassTag = classTag{
	id:             &ast.Var{Name: ast.AnyTypeName},
	parents:        set.New[typeName](0),
	withProvenance: withProvenance{builtinProv},
}

var intType = classTag{
	id:             &ast.Var{Name: ast.IntBuiltinTypeName},
	parents:        set.From([]typeName{ast.AnyTypeName}),
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

var errorTypeInstance = classTag{
	id:      &ast.Var{Name: "Error"},
	parents: set.New[typeName](0),
	withProvenance: withProvenance{
		provenance: typeProvenance{
			desc: "Error",
		},
	},
}
