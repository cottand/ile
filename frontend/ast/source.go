package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/types"
	"go/token"
)

// Positioner allows finding the location in the original source file
// the easiest way to be a Positioner is to embed a Range
type Positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// TypeAnnotation should produce a type at the given binding-level. The constructed type may include
// types derived from variables which are already in scope (retrieved from the type-environment).
type TypeAnnotation = func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error)

type Range struct {
	PosStart token.Pos
	PosEnd   token.Pos
}

func (r Range) Pos() token.Pos { return r.PosStart }
func (r Range) End() token.Pos { return r.PosEnd }

func GetRange(e Expr) Range {
	return Range{e.Pos(), e.End()}
}

type File struct {
	Range
	PkgName      string
	Declarations []Declaration
}

func (f File) String() string {
	return fmt.Sprint(f.PkgName, "\n", f.Declarations)
}

func (f File) AsGroupedLet(in Expr) Expr {
	bindings := make([]LetBinding, len(f.Declarations))
	for i, decl := range f.Declarations {
		bindings[i] = LetBinding{
			Var: decl.Name,
			Value: decl.E,
		}
	}
	return &LetGroup{
		Vars:  bindings,
		Body:  in,
	}
}

// Declaration is a top-level declaration in a File
type Declaration struct {
	Range
	Name string
	E    Expr
	T         TypeAnnotation
	IsFunc    bool
}
