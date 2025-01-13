package failed

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/types"
	"strings"
)

type ErrCode int

const (
	None           ErrCode = iota
	TypeUnifyConst ErrCode = iota
	Parse
	MissingDiscardInWhen
	UndefinedVariable
	ManyPackageNamesInPackage
	MissingTypeAnnotationInPublicDeclaration
	RestrictedIdentName
)

type IleError interface {
	Error() string
	Code() ErrCode
	ast.Positioner
}

func FormatWithCode(e IleError) string {
	return fmt.Sprintf("(E%03d) %s", e.Code(), e.Error())
}

type Unclassified struct {
	From error
	ast.Positioner
}

func (e Unclassified) Error() string {
	return fmt.Sprintf("unclassified error: %v", e.From)
}
func (e Unclassified) Code() ErrCode { return None }

type NewTypeUnificationConst struct {
	ast.Positioner
	First  types.Type
	Second types.Type
}

func (e NewTypeUnificationConst) Error() string {
	return fmt.Sprintf("type mismatch: expected type '%v', but found a different type '%v'", e.First.TypeName(), e.Second.TypeName())
}
func (e NewTypeUnificationConst) Code() ErrCode { return TypeUnifyConst }

type NewParse struct {
	ast.Positioner
	ParserMessage string
	Hint          string
}

func (e NewParse) Error() string {
	return e.ParserMessage
}
func (e NewParse) Code() ErrCode { return Parse }

type NewMissingDiscardInWhen struct {
	ast.Positioner
}

func (e NewMissingDiscardInWhen) Code() ErrCode { return MissingDiscardInWhen }
func (e NewMissingDiscardInWhen) Error() string {
	return fmt.Sprintf("last case of when is missing final discard pattern '_'")
}

type NewUndefinedVariable struct {
	ast.Positioner
	Name string
}

func (e NewUndefinedVariable) Code() ErrCode { return UndefinedVariable }
func (e NewUndefinedVariable) Error() string {
	return fmt.Sprintf("variable '%s' is not defined", e.Name)
}

type NewManyPackageNamesInPackage struct {
	ast.Positioner
	Names []string
}

func (e NewManyPackageNamesInPackage) Error() string {
	return fmt.Sprintf("multiple package names in package: %v", strings.Join(e.Names, ", "))
}
func (e NewManyPackageNamesInPackage) Code() ErrCode {
	return ManyPackageNamesInPackage
}

type NewMissingTypeAnnotationInPublicDeclaration struct {
	ast.Positioner
	DeclName string
}

func (e NewMissingTypeAnnotationInPublicDeclaration) Error() string {
	return fmt.Sprintf("missing type annotation in public declaration %v", e.DeclName)
}
func (e NewMissingTypeAnnotationInPublicDeclaration) Code() ErrCode {
	return MissingTypeAnnotationInPublicDeclaration
}

type NewRestrictedIdentName struct {
	ast.Positioner
	Name string
}

func (e NewRestrictedIdentName) Error() string {
	return fmt.Sprintf("identifier name '%s' is not allowed", e.Name)
}

func (e NewRestrictedIdentName) Code() ErrCode { return RestrictedIdentName }
