package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/hmtypes"
	"github.com/cottand/ile/internal/log"
	"go/token"
	"unicode"
	"unicode/utf8"
)

var logger = log.DefaultLogger.With("section", "ast")

// Positioner allows finding the location in the original source file.
// The easiest way to be a Positioner is to embed a Range
type Positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// TypeAnnotationFn should produce a type at the given binding-level. The constructed type may include
// types derived from variables which are already in scope (retrieved from the type-environment).
type TypeAnnotationFn = func(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error)

type Range struct {
	PosStart token.Pos
	PosEnd   token.Pos
}

func (r Range) Pos() token.Pos { return r.PosStart }
func (r Range) End() token.Pos { return r.PosEnd }

func GetRange(e Expr) Range {
	return Range{e.Pos(), e.End()}
}

var _ Positioner = (*File)(nil)
var _ Positioner = (*Declaration)(nil)

type File struct {
	Range
	PkgName      string
	Declarations []Declaration
	Imports      []Import
	GoImports    []Import
}

func (f File) String() string {
	return fmt.Sprint(f.PkgName, "\n", f.Declarations)
}

// Declaration is a top-level declaration in a File
//
// The declared type of Declaration is in E when applicable
type Declaration struct {
	Range // of the LHS including '='
	Name  string
	E     Expr
	// Comments keeps a list of lines for comments immediately preceding a declaration.
	// It may be nil if the declaration was not adjacent to any comments.
	Comments []string
}

func (d Declaration) IsPublic() bool {
	if len(d.Name) == 0 {
		return false
	}
	r, _ := utf8.DecodeRuneInString(d.Name)
	return r != utf8.RuneError && unicode.IsUpper(r)
}

type Import struct {
	Positioner
	// if Alias is the empty string it means there was no alias
	Alias      string
	ImportPath string
}
