package ast

import (
	"github.com/cottand/ile/frontend/types"
	"go/token"
)

func BinOp(t token.Token, in Range) *Literal {
	switch t {
	case token.ADD:
		return &Literal{
			Range: in,
			Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
				tVar := env.NewVar(level)
				return &types.Arrow{
					Args:   []types.Type{tVar, tVar},
					Return: tVar,
					Method: nil,
					Source: nil,
					Flags:  types.ContainsGenericVars,
				}, nil
			},
			Syntax: "+",
		}

	default:
		panic("unimplemented binary operation for token: " + t.String())
	}
}

func StringLiteral(value string, in Range) *Literal {
	return &Literal{
		Range: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.Const{Name: "String"}, nil
		},
		Syntax: value,
	}
}

func IntLiteral(value string, in Range) *Literal {
	return &Literal{
		Range: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.Const{Name: "Int"}, nil
		},
		Syntax: value,
	}
}
