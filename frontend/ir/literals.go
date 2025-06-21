package ir

import (
	"go/ast"
	"go/token"
)

func BinOp(t token.Token, in ast.Node) Expr {
	if in == nil {
		in = Range{}
	}
	switch t {
	case token.ADD, token.SUB, token.MUL, token.QUO, token.REM, token.GEQ, token.LEQ, token.GTR, token.LSS, token.EQL, token.NEQ, token.LAND, token.LOR:
		return &Var{
			Range: RangeOf(in),
			Name:  t.String(),
		}

	default:
		panic("unimplemented binary operation for token: " + t.String())
	}
}

func StringLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Range:  RangeOf(in),
		Syntax: value,
		Kind:   token.STRING,
	}
}

// IntLiteral represents a compile time integer value
//
// Semantics for later converting to the appropriate type must follow Go's (see https://go.dev/ref/spec#Constants)
func IntLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Range:  RangeOf(in),
		Syntax: value,
		Kind:   token.INT,
	}
}

// FloatLiteral represents a compile time integer value
//
// Semantics for later converting to the appropriate type must follow Go's (see https://go.dev/ref/spec#Constants)
func FloatLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Range:  RangeOf(in),
		Syntax: value,
		Kind:   token.FLOAT,
	}
}
