package ast

import (
	"github.com/cottand/ile/frontend/types"
	"go/ast"
	"go/token"
)

func BinOp(t token.Token, in ast.Node) *Literal {
	switch t {
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM:
		return &Literal{
			Node: in,
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
			Syntax: t.String(),
			Kind:   t,
		}

		// restrict to comparables only?
	case token.GEQ, token.LEQ, token.GTR, token.LSS:
		return &Literal{
			Node: in,
			Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
				tVar := env.NewVar(level)
				return &types.Arrow{
					Args:   []types.Type{tVar, tVar},
					Return: &types.Const{Name: "Bool"},
					Method: nil,
					Source: nil,
					Flags:  types.ContainsGenericVars,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

	case token.EQL, token.NEQ:
		return &Literal{
			Node: in,
			Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
				tVar := env.NewVar(level)
				return &types.Arrow{
					Args:   []types.Type{tVar, tVar},
					Return: &types.Const{Name: "Bool"},
					Method: nil,
					Source: nil,
					Flags:  types.ContainsGenericVars,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

	case token.LAND, token.LOR:
		return &Literal{
			Node: in,
			Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
				boolT := &types.Const{Name: "Bool"}
				return &types.Arrow{
					Args:   []types.Type{boolT, boolT},
					Return: boolT,
					Method: nil,
					Source: nil,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

	default:
		panic("unimplemented binary operation for token: " + t.String())
	}
}

func StringLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Node: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.Const{Name: "String"}, nil
		},
		Syntax: value,
		Kind:   token.STRING,
	}
}

func IntLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Node: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.Const{Name: "Int"}, nil
		},
		Syntax: value,
		Kind:   token.INT,
	}
}
