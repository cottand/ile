package ast

import (
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
			Syntax: t.String(),
			Kind:   t,
		}

		// restrict to comparables only?
	case token.GEQ, token.LEQ, token.GTR, token.LSS:
		return &Literal{
			Range: RangeOf(in),
			Syntax: t.String(),
			Kind:   t,
		}

	case token.EQL, token.NEQ:
		return &Literal{
			Range: RangeOf(in),
			Syntax: t.String(),
			Kind:   t,
		}

	case token.LAND, token.LOR:
		return &Literal{
			Range: RangeOf(in),
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
		Syntax: value,
		Kind:   token.STRING,
	}
}

// IntLiteral represents a compile time integer value
//
// Semantics for later converting to the appropriate type must follow Go's (see https://go.dev/ref/spec#Constants)
func IntLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Range: RangeOf(in),
		Syntax: value,
		Kind:   token.INT,
	}
}

// FloatLiteral represents a compile time integer value
//
// Semantics for later converting to the appropriate type must follow Go's (see https://go.dev/ref/spec#Constants)
func FloatLiteral(value string, in ast.Node) *Literal {
	return &Literal{
		Range: RangeOf(in),
		Syntax: value,
		Kind:   token.FLOAT,
	}
}

