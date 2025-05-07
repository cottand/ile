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
	id:             &ast.Var{Name: ast.IntTypeName},
	parents:        set.From([]typeName{ast.AnyTypeName}),
	withProvenance: withProvenance{builtinProv},
}

var trueType = classTag{
	id:             &ast.Var{Name: ast.TrueName},
	parents:        set.From([]typeName{ast.AnyTypeName}),
	withProvenance: withProvenance{builtinProv},
}
var falseType = classTag{
	id:             &ast.Var{Name: ast.FalseName},
	parents:        set.From([]typeName{ast.AnyTypeName}),
	withProvenance: withProvenance{builtinProv},
}

var boolType = unionType{
	lhs:            trueType,
	rhs:            falseType,
	withProvenance: withProvenance{builtinProv},
}

var comparisonBinOp = funcType{
	args:           []SimpleType{intType, intType},
	ret:            boolType,
	withProvenance: withProvenance{builtinProv},
}

// universe are the built-in bindings
func universe() map[string]typeInfo {
	numberOp := funcType{
		args:           []SimpleType{intType, intType},
		ret:            intType,
		withProvenance: withProvenance{builtinProv},
	}
	return map[string]typeInfo{
		"+":           numberOp,
		"*":           numberOp,
		"-":           numberOp,
		"/":           numberOp,
		"%":           numberOp,
		ast.TrueName:  trueType,
		ast.FalseName: falseType,
		">":           comparisonBinOp,
		"<":           comparisonBinOp,
		"==":          comparisonBinOp,
		"!=":          comparisonBinOp,
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

var builtinTypes = []TypeDefinition{
	{
		defKind:          ast.KindClass,
		name:             ast.IntTypeName,
		typeVars:         nil,
		typeParamArgs:    nil,
		typeVarVariances: nil,
		bodyType:         topType,
		baseClasses:      intType.parents,
	},
}
