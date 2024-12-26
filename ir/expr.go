package ir

// when adding types here, you should add them to the switch cases in:
// - ir:infer.go/TypeInfer
// - backend:compile.go/transpileExpressionToStatements
// - backend:compile.go/transpileExpr

import (
	"github.com/cottand/ile/ir/hm"
	"go/token"
)

type Expr interface {
	hm.Expression
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
func (e BasicLitExpr) Body() hm.Expression { return e }
func (e BasicLitExpr) Name() string        { return e.Value }
func (e BasicLitExpr) Type() hm.Type {
	switch e.Kind {
	case token.STRING:
		return TypeLit{NameLit: "String"}
	case token.INT:
		return TypeLit{NameLit: "Int"}
	default:
		return TypeLit{NameLit: "ERROR!"}
	}
}
func (e BasicLitExpr) IsLit() bool { return true }

// ----------------------------------------------

type BinaryOpExpr struct {
	Range
	Op       PrimOp
	Lhs, Rhs Expr
}

func (BinaryOpExpr) exprNode() {}

//func (BinaryOpExpr) String() string { return "+" }

// IsBoolOp returns true when the operation returns a Bool regardless
// of whether its LHS and RHS are booleans (for example, 3 > 4)
func (op BinaryOpExpr) IsBoolOp() bool {
	switch op.Op.Token() {
	case token.GTR, token.LEQ, token.GEQ, token.LSS:
		return true
	default:
		return false
	}
}
func (op BinaryOpExpr) Fn() hm.Expression {
	return unaryApplication{
		f:   op.Op,
		arg: op.Rhs.(BasicLitExpr),
	}
}
func (op BinaryOpExpr) Body() hm.Expression { return op.Lhs }
func (op BinaryOpExpr) Arg() hm.Expression  { return op.Lhs }
func (op BinaryOpExpr) IsLambda() bool      { return true }


// ----------------------------------------------

type IdentifierLitExpr struct {
	Ident
}

func (IdentifierLitExpr) exprNode() {}

func (e IdentifierLitExpr) Body() hm.Expression {
	//TODO implement me
	panic("implement me")
}


// --------------------------------

// unaryApplication does not actually make part of the syntax tree,
// it is used for inference and is returned by
// - BinaryOpExpr
// to represent function application of the operation to
// its arguments
type unaryApplication struct {
	f   hm.Expression
	arg hm.Expression
}

func (n unaryApplication) Fn() hm.Expression   { return n.f }
func (n unaryApplication) Body() hm.Expression { return n.arg }
func (n unaryApplication) Arg() hm.Expression  { return n.arg }

// ------------------------------

