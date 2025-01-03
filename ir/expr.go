package ir

// when adding types here, you should add them to the switch cases in:
// - ir:infer.go/TypeInfer
// - backend:compile.go/transpileExpressionToStatements
// - backend:compile.go/transpileExpr

import (
	"fmt"
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

func (BasicLitExpr) exprNode()             {}
func (e BasicLitExpr) Body() hm.Expression { return e }
func (e BasicLitExpr) Name() string        { return e.Value }
func (e BasicLitExpr) Type() hm.Type {
	switch e.Kind {
	case token.STRING:
		return TypeLit{NameLit: "String"}
	case token.INT:
		return TypeLit{NameLit: "Int"}
	default:
		panic(fmt.Sprintf("unexpected token kind %v (%v)", e.Kind, e.Kind.String()))
	}
}
func (e BasicLitExpr) IsLit() bool { return true }
func (e BasicLitExpr) String() string {
	return e.Value
}

// ----------------------------------------------

type BinaryOpExpr struct {
	Range
	Op       PrimOp
	Lhs, Rhs Expr
}

func (BinaryOpExpr) exprNode() {}

// IsBoolOp returns true when the operation returns a Bool regardless
// of whether its LHS and Rhs are booleans (for example, 3 > 4)
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
		arg: op.Rhs,
	}
}
func (op BinaryOpExpr) Body() hm.Expression { return op.Lhs }
func (op BinaryOpExpr) Arg() hm.Expression  { return op.Lhs }
func (op BinaryOpExpr) String() string {
	return fmt.Sprintf("%v %v %v", op.Lhs, op.Op.Token().String(), op.Rhs)
}

// ----------------------------------------------

type IdentifierLitExpr struct {
	Range
	NameLit   string
	KnownType Type
}

func (IdentifierLitExpr) exprNode()             {}
func (e IdentifierLitExpr) Name() string        { return "ident:" + e.NameLit }
func (e IdentifierLitExpr) Body() hm.Expression { return e }
func (e IdentifierLitExpr) Type() hm.Type       { return nil }
func (e IdentifierLitExpr) String() string {
	return e.NameLit
}
func (e IdentifierLitExpr) AsIdent() Ident {
	return Ident{
		Range: e.Range,
		Name:  e.NameLit,
	}
}

// --------------------------------

type AssignExpr struct {
	// range of the LHS including the assignment operator
	Range
	IdentName string
	Rhs       Expr
	Remainder Expr
	Type      Type // can be nil
}

func (AssignExpr) exprNode()             {}
func (e AssignExpr) Name() string        { return "ident:" + e.IdentName }
func (e AssignExpr) Def() hm.Expression  { return e.Rhs }
func (e AssignExpr) Body() hm.Expression { return e.Remainder }
func (e AssignExpr) String() string {
	return fmt.Sprintf("%v = %v\n%v", e.IdentName, e.Rhs, e.Remainder)
}

// ------------------------------

// UnusedExpr represents an expression that is part of a block but its result is not used
type UnusedExpr struct {
	Range
	Expr      Expr
	Remainder Expr
}

func (UnusedExpr) exprNode()             {}
func (e UnusedExpr) Name() string        { return "_" }
func (e UnusedExpr) Def() hm.Expression  { return e.Expr }
func (e UnusedExpr) Body() hm.Expression { return e.Remainder }

// ------------------------------

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

// ErrorNodeExpr is for when parsing failed, but we still want an IR representation
// You should only see these in a context in which ir.CompileError is present too
type ErrorNodeExpr struct {
	Range
	Text string
}

func (ErrorNodeExpr) exprNode()             {}
func (e ErrorNodeExpr) Body() hm.Expression { return e }
