package ir

import (
	"fmt"
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

func GetRange(e Expr) Range {
	return Range{e.Pos(), e.End()}
}

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

func (f File) String() string {
	return fmt.Sprint(f.PkgName, " values=", f.Values, " functions=", f.Functions)
}

type ValDecl struct {
	Range
	Name string
	E    Expr
	T    Type
}
