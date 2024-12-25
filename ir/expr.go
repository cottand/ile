package ir

import "go/token"

type Expr interface {
	Node
	exprNode()
}

// A BasicLit node represents a literal of basic type.
type BasicLit struct {
	Range
	Kind  token.Token // token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
	Value string      // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
}

func (BasicLit) exprNode() {}
