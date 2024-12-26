package ir

// when adding types here, you should add them to the switch cases in:
// - ir:infer.go/TypeInfer
// - backend:compile.go/transpileExpressionToStatements
// - backend:compile.go/transpileExpr

import (
	"go/token"
)

type Expr interface {
	Node
	exprNode()
}

// A BasicLitExpr node represents a literal of basic type.
type BasicLitExpr struct {
	Range
	Kind  token.Token // token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
	Value string      // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
}

func (BasicLitExpr) exprNode() {}


type BinaryOpExpr struct {
	Range
	Op       token.Token
	Lhs, Rhs Expr
}

func (BinaryOpExpr) exprNode() {}

// IsBoolOp returns true when the operation returns a Bool regardless
// of whether its LHS and RHS are booleans (for example, 3 > 4)
func (op BinaryOpExpr) IsBoolOp() bool {
	switch op.Op {
	case token.GTR, token.LEQ, token.GEQ, token.LSS:
		return true
	default:
		return false
	}
}

type IdentifierLitExpr struct {
	Ident
}
func (IdentifierLitExpr) exprNode() {}