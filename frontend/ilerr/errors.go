package ilerr

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/types"
	"runtime/debug"
	"strings"
)

// enableDebugErrorPrinting makes errors include their stacktrace when printed
const enableDebugErrorPrinting bool = true
const enableDebugFullStacktrace bool = false

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
	UnsupportedGoType
	TypeUnifyComptimeConst
)

type IleError interface {
	Error() string
	Code() ErrCode
	ast.Positioner

	withStack([]byte) IleError
	getStack() []byte
}

func FormatWithCode(e IleError) string {
	if enableDebugErrorPrinting && e.getStack() != nil {
		stack := string(e.getStack())
		if !enableDebugFullStacktrace {
			stack = strings.Split(stack, "\n")[6]
		}
		return fmt.Sprintf("%s:(E%03d) %s", stack, e.Code(), e.Error())
	}
	return fmt.Sprintf("(E%03d) %s", e.Code(), e.Error())
}

func New[E IleError](err E) IleError {
	return err.withStack(debug.Stack())
}

type Unclassified struct {
	From error
	ast.Positioner
	stack []byte
}

func (e Unclassified) Error() string {
	return fmt.Sprintf("unclassified error: %v", e.From)
}
func (e Unclassified) Code() ErrCode    { return None }
func (e Unclassified) getStack() []byte { return e.stack }
func (e Unclassified) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewTypeUnificationConst struct {
	ast.Positioner
	First  types.Type
	Second types.Type
	stack  []byte
}

func (e NewTypeUnificationConst) Error() string {
	return fmt.Sprintf("type mismatch: expected type '%v', but found a different type '%v'", e.First.TypeName(), e.Second.TypeName())
}
func (e NewTypeUnificationConst) Code() ErrCode    { return TypeUnifyConst }
func (e NewTypeUnificationConst) getStack() []byte { return e.stack }
func (e NewTypeUnificationConst) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewParse struct {
	ast.Positioner
	ParserMessage string
	Hint          string
	stack         []byte
}

func (e NewParse) Error() string {
	return e.ParserMessage
}
func (e NewParse) Code() ErrCode    { return Parse }
func (e NewParse) getStack() []byte { return e.stack }
func (e NewParse) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewMissingDiscardInWhen struct {
	ast.Positioner
	stack []byte
}

func (e NewMissingDiscardInWhen) Code() ErrCode { return MissingDiscardInWhen }
func (e NewMissingDiscardInWhen) Error() string {
	return fmt.Sprintf("last case of when is missing final discard pattern '_'")
}
func (e NewMissingDiscardInWhen) getStack() []byte { return e.stack }
func (e NewMissingDiscardInWhen) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewUndefinedVariable struct {
	ast.Positioner
	Name  string
	stack []byte
}

func (e NewUndefinedVariable) Code() ErrCode { return UndefinedVariable }
func (e NewUndefinedVariable) Error() string {
	return fmt.Sprintf("variable '%s' is not defined", e.Name)
}
func (e NewUndefinedVariable) getStack() []byte { return e.stack }
func (e NewUndefinedVariable) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewManyPackageNamesInPackage struct {
	ast.Positioner
	Names []string
	stack []byte
}

func (e NewManyPackageNamesInPackage) Error() string {
	return fmt.Sprintf("multiple package names in package: %v", strings.Join(e.Names, ", "))
}
func (e NewManyPackageNamesInPackage) Code() ErrCode {
	return ManyPackageNamesInPackage
}
func (e NewManyPackageNamesInPackage) getStack() []byte { return e.stack }
func (e NewManyPackageNamesInPackage) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewMissingTypeAnnotationInPublicDeclaration struct {
	ast.Positioner
	DeclName string
	stack    []byte
}

func (e NewMissingTypeAnnotationInPublicDeclaration) Error() string {
	return fmt.Sprintf("missing type annotation in public declaration %v", e.DeclName)
}
func (e NewMissingTypeAnnotationInPublicDeclaration) Code() ErrCode {
	return MissingTypeAnnotationInPublicDeclaration
}
func (e NewMissingTypeAnnotationInPublicDeclaration) getStack() []byte { return e.stack }
func (e NewMissingTypeAnnotationInPublicDeclaration) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewRestrictedIdentName struct {
	ast.Positioner
	Name  string
	stack []byte
}

func (e NewRestrictedIdentName) Error() string {
	return fmt.Sprintf("identifier name '%s' is not allowed", e.Name)
}

func (e NewRestrictedIdentName) Code() ErrCode    { return RestrictedIdentName }
func (e NewRestrictedIdentName) getStack() []byte { return e.stack }
func (e NewRestrictedIdentName) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewUnsupportedGoType struct {
	ast.Positioner
	Name  string
	stack []byte
}

func (e NewUnsupportedGoType) Code() ErrCode { return UnsupportedGoType }
func (e NewUnsupportedGoType) Error() string {
	return fmt.Sprintf("go type '%s' is not supported", e.Name)
}
func (e NewUnsupportedGoType) getStack() []byte { return e.stack }
func (e NewUnsupportedGoType) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewTypeUnificationComptimeConst struct {
	ast.Positioner
	First  types.Type
	Second types.Type
	Reason string
	stack  []byte
}

func (e NewTypeUnificationComptimeConst) Error() string {
	return fmt.Sprintf("type mismatch: type '%v', cannot accomodate '%v': %v", e.First.TypeName(), e.Second.TypeName(), e.Reason)
}
func (e NewTypeUnificationComptimeConst) Code() ErrCode    { return TypeUnifyComptimeConst }
func (e NewTypeUnificationComptimeConst) getStack() []byte { return e.stack }
func (e NewTypeUnificationComptimeConst) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}