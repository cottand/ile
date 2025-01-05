package ast

import "go/ast"

type CompileError struct {
	Message string
	At      ast.Node
}
