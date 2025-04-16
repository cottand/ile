package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/hmtypes"
	"go/ast"
	"go/token"
)

func BinOp(t token.Token, in ast.Node) *Literal {
	if in == nil {
		in = Range{}
	}
	switch t {
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM:

		return &Literal{
			Range: RangeOf(in),
			Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
				tVar := env.NewVar(level)
				return &hmtypes.Arrow{
					Args:   []hmtypes.Type{tVar, tVar},
					Return: tVar,
					Method: nil,
					Source: nil,
					Flags:  hmtypes.ContainsGenericVars,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

		// restrict to comparables only?
	case token.GEQ, token.LEQ, token.GTR, token.LSS:
		return &Literal{
			Range: RangeOf(in),
			Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
				tVar := env.NewVar(level)
				return &hmtypes.Arrow{
					Args:   []hmtypes.Type{tVar, tVar},
					Return: &hmtypes.Const{Name: "Bool"},
					Method: nil,
					Source: nil,
					Flags:  hmtypes.ContainsGenericVars,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

	case token.EQL, token.NEQ:
		return &Literal{
			Range: RangeOf(in),
			Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
				tVar := env.NewVar(level)
				return &hmtypes.Arrow{
					Args:   []hmtypes.Type{tVar, tVar},
					Return: &hmtypes.Const{Name: "Bool"},
					Method: nil,
					Source: nil,
					Flags:  hmtypes.ContainsGenericVars,
				}, nil
			},
			Syntax: t.String(),
			Kind:   t,
		}

	case token.LAND, token.LOR:
		return &Literal{
			Range: RangeOf(in),
			Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
				boolT := &hmtypes.Const{Name: "Bool"}
				return &hmtypes.Arrow{
					Args:   []hmtypes.Type{boolT, boolT},
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
		Range: RangeOf(in),
		Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
			return &hmtypes.Const{Name: "String"}, nil
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
		Range: RangeOf(in),
		Construct: func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
			return &hmtypes.CompTimeConst{
				Name: "comptime Int",
				MaybeUnify: func(other hmtypes.Const) error {
					switch other.Name {
					case "Int":
						return nil

					default:
						return fmt.Errorf("an Int literal cannot be used with type %s", other.Name)
					}
				},
				DefaultType: &hmtypes.Const{Name: "Int"},
			}, nil
		},
		Syntax: value,
		Kind:   token.INT,
	}
}
