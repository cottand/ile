package ir

import (
	"github.com/cottand/ile/ir/hm"
	"go/token"
)

type Node interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

type Range struct {
	PosStart token.Pos
	PosEnd   token.Pos
}

func (r Range) Pos() token.Pos { return r.PosStart }
func (r Range) End() token.Pos { return r.PosEnd }

type Ident struct {
	Range
	Name string
}

type File struct {
	Range
	PkgName   string
	Values    []ValDecl
	Functions []FuncDecl
}

type ValDecl struct {
	Range
	Name string
	E    Expr
	T    Type
}

