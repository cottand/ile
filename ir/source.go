package ir

import (
	"go/token"
)

type Node interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

type CompileError struct {
	Message string
	At *Node
	// TODO suggestions?
}



type File struct {
	PkgName      string
	Declarations []ValDecl
}

type ValDecl struct {
	Name string
	E    Expr
}
