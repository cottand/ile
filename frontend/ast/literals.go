package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/types"
	"go/ast"
	"go/token"
)

func BinOp(t token.Token, in ast.Node) *Literal {
	switch t {
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM:
		return &Literal{
			Positioner: in,
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
			Positioner: in,
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
			Positioner: in,
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
			Positioner: in,
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
		Positioner: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.Const{Name: "String"}, nil
		},
		Syntax: value,
		Kind:   token.STRING,
	}
}

// IntLiteral represents a compile time integer value
//
// # It is written as a literal so we do not know if it will be used as an Int, Int32, or IntPlat
//
// Semantics for later converting to the appropriate type must follow Go's (see https://go.dev/ref/spec#Constants)
func IntLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Positioner: in,
		Construct: func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
			return &types.CompTimeConst{
				Name: "comptime Int",
				MaybeUnify: func(other types.Const) error {
					switch other.Name {
					case "Int":
						return nil

					default:
						return fmt.Errorf("an Int literal cannot be used with type %s", other.Name)
					}
				},
				DefaultType: &types.Const{Name: "Int"},
			}, nil
		},
		Syntax: value,
		Kind:   token.INT,
	}
}
