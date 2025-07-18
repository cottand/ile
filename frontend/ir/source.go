package ir

import (
	"fmt"
	"go/token"
	"unicode"
	"unicode/utf8"
)

// Positioner allows finding the location in the original source file.
// The easiest way to be a Positioner is to embed a Range
type Positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// ExternalPositioner is a Positioner that also has a package path,
// typically for highlighting sources of external dependencies
//
// By convention; an empty PackagePath indicates the local package
type ExternalPositioner interface {
	Positioner
	PackagePath() string
}

type Range struct {
	PosStart token.Pos
	PosEnd   token.Pos
}

type ExternalRange struct {
	Range
	Package string
}

func (e ExternalRange) PackagePath() string {
	return e.Package
}

func (r Range) Pos() token.Pos { return r.PosStart }
func (r Range) End() token.Pos { return r.PosEnd }
func (r Range) String() string {
	if r.PosStart == r.PosEnd {
		return fmt.Sprintf("%v", r.PosStart)
	}
	return fmt.Sprintf("%v-%v", r.PosStart, r.PosEnd)
}

func RangeBetween(fst, snd Positioner) Range {
	return Range{fst.Pos(), snd.End()}
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
	// Type is the type annotation for the declaration, derived from inference, not from
	// the type annotation in the declaration. This should be set by the type-checker only.
	// For the optional type annotation in the declaration, use Ascribe inside E
	Type Type
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
