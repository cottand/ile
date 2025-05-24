package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/hashicorp/go-set/v3"
)

var builtinProv = newOriginProv(ast.Range{}, "builtin", "builtin")

// instantiated types that can be referenced directly
// so they cannot contain type variables, therefore, they are usable anywhere
var (
	// anyClassTag is called Object in the reference scala implementation
	anyClassTag = classTag{
		id:             &ast.Var{Name: ast.AnyTypeName},
		parents:        set.New[typeName](0),
		withProvenance: withProvenance{builtinProv},
	}
	intType = classTag{
		id:             &ast.Var{Name: ast.IntTypeName},
		parents:        set.From([]typeName{ast.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	stringType = classTag{
		id:             &ast.Var{Name: ast.StringTypeName},
		parents:        set.From([]typeName{ast.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	trueType = classTag{
		id:             &ast.Var{Name: ast.TrueName},
		parents:        set.From([]typeName{ast.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	falseType = classTag{
		id:             &ast.Var{Name: ast.FalseName},
		parents:        set.From([]typeName{ast.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	boolType = unionType{
		lhs:            trueType,
		rhs:            falseType,
		withProvenance: withProvenance{builtinProv},
	}
)

type universeStruct struct {
	any, nothing      SimpleType
	string, int       SimpleType
	true, false, bool SimpleType
	plus              SimpleType
}

func (t *Fresher) universeEnv() map[string]typeInfo {
	u := t.universe

	comparisonBinOp := func() funcType {
		tv := t.newTypeVariable(1, emptyProv, "", nil, nil)
		return funcType{
			args:           []SimpleType{tv, tv},
			ret:            boolType,
			withProvenance: withProvenance{builtinProv},
		}
	}
	numberOp := func() funcType {
		tv := t.newTypeVariable(1, emptyProv, "", nil, nil)
		return funcType{
			args: []SimpleType{tv, tv},
			ret:  tv,
		}
	}
	return map[string]typeInfo{
		"+":           u.plus,
		"*":           numberOp(),
		"-":           numberOp(),
		"/":           numberOp(),
		"%":           numberOp(),
		ast.TrueName:  u.true,
		ast.FalseName: u.true,
		">":           comparisonBinOp(),
		"<":           comparisonBinOp(),
		"==":          comparisonBinOp(),
		"!=":          comparisonBinOp(),
	}
}

// universeInit should only be called during NewFresher
func (t *Fresher) universeInit() universeStruct {
	return universeStruct{
		any:     anyClassTag,
		nothing: bottomType,
		string:  stringType,
		int:     intType,
		true:    trueType,
		false:   falseType,
		bool:    boolType,
		plus: func() SimpleType {
			//tv := t.newTypeVariable(1, emptyProv, "", nil, nil)
			tv := intType
			return funcType{
				args:           []SimpleType{tv, tv},
				ret:            tv,
				withProvenance: withProvenance{builtinProv},
			}
		}(),
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
