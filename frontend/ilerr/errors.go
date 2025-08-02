package ilerr

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"go/token"
	"runtime/debug"
	"strings"
)

// enableDebugErrorPrinting makes errors include their stacktrace when printed
const enableDebugErrorPrinting bool = true
const enableDebugFullStacktrace bool = false

type ErrCode int

const (
	None ErrCode = iota
	Syntax
	MissingDiscardInWhen
	UndefinedVariable
	ManyPackageNamesInPackage
	MissingTypeAnnotationInPublicDeclaration
	RestrictedIdentName
	UnsupportedGoType
	NameRedeclaration
	TypeMismatch
	ExpectedTypeParams
	EmptyWhen
	RepeatedRecordField
)

type IleError interface {
	Error() string
	Code() ErrCode
	ir.Positioner

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

type SourceFinder interface {
	FindSnippet(positioner ir.Positioner) (string, error)
	GetLine(pos token.Pos) (string, error)
	Position(token.Pos) token.Position
}

type HighlightOpts = struct {
	// The character placed under the relevant text area
	// default is '^'
	HighlightRune rune
	// Whether not to also print the location as {filename}:{Line}:{Column}
	// next to the highlighted snippet
	HideNameLineColumn bool
	// Whether Line numbers left of the snippet are not printed
	HideLineNumbers bool
	// How many lines, in addition to the highlighted one, are shown before and after
	// the snippet
	// Default is 0
	LineBuffer int

	// Indentation of the whole returned string
	//LeftIndent int
}
type SourceHighlighter interface {
	Filename(pos token.Pos) (string, error)
	Highlight(ir.ExternalPositioner, HighlightOpts) (string, error)
}

// FormatWithCodeAndSource formats an error with an inline highlight of the code snippet(s) that produced it.
// This is useful for displaying errors in the CLI.
func FormatWithCodeAndSource(e IleError, highlighter SourceHighlighter) string {
	highlightOpts := HighlightOpts{LineBuffer: 1}
	highlight, err := highlighter.Highlight(ir.ExternalRange{Range: ir.RangeOf(e)}, highlightOpts)
	if err != nil {
		highlight = fmt.Sprintf("| (failed to show source: %s)", err)
	}

	stack := ""
	if enableDebugErrorPrinting && e.getStack() != nil {
		stack = string(e.getStack())
		if !enableDebugFullStacktrace {
			stack = strings.TrimSpace(strings.Split(stack, "\n")[6])
			stack = stack[:len(stack)-7]
		}
		stack = "\n    error produced at " + stack
	}

	additionalText := strings.Builder{}

	var mismatch NewTypeMismatch
	if errors.As(e, &mismatch) {
		// Add provenance hint if available
		if mismatch.ProvenanceHint != "" {
			additionalText.WriteString(fmt.Sprintf("hint: %s", mismatch.ProvenanceHint))
			if mismatch.ProvenanceHintPos != nil {
				hintPos, err := highlighter.Highlight(mismatch.ProvenanceHintPos, highlightOpts)
				if err == nil {
					additionalText.WriteString(fmt.Sprintf(":%s\n", hintPos))
				}
			}
		}

		//Add flow hint if available
		if mismatch.FlowHint != "" {
			additionalText.WriteString(fmt.Sprintf(" %s", mismatch.FlowHint))
			if mismatch.FlowHintPos != nil {
				flowPos, err := highlighter.Highlight(mismatch.FlowHintPos, highlightOpts)
				if err == nil {
					additionalText.WriteString(fmt.Sprintf(":%s\n", flowPos))
				}
			}
		}

		//Add type stack hints if available
		for _, hint := range mismatch.ExpectedStackHints {
			if hint.Type == "" {
				additionalText.WriteString(fmt.Sprintf("%s\n", hint.Description))
			} else {
				additionalText.WriteString(fmt.Sprintf("%s: %s\n", hint.Description, hint.Type))
			}

			if hint.Positioner != nil {
				hintPos, err := highlighter.Highlight(hint.Positioner, highlightOpts)
				if err == nil {
					additionalText.WriteString(fmt.Sprintf("%s\n", hintPos))
				}
			}
		}

		for i, hint := range mismatch.ActualStackHints {
			if i == 0 {
				additionalText.WriteString("actual type comes from ")
			}
			if hint.Description == "" {
				continue
			}
			if hint.Type == "" {
				additionalText.WriteString(hint.Description)
			} else {
				additionalText.WriteString(fmt.Sprintf("%s: %s", hint.Description, hint.Type))
			}

			if hint.Positioner != nil {
				hintPos, err := highlighter.Highlight(hint.Positioner, highlightOpts)
				if err == nil {
					additionalText.WriteString(fmt.Sprintf("\ndeclared at: %s", hintPos))
				}
			}
		}
	}

	highlight = fmt.Sprintf("%s\n(E%03d) %s", highlight, e.Code(), e.Error())

	return highlight + stack + "\n" + additionalText.String()
}

func New[E IleError](err E) IleError {
	return err.withStack(debug.Stack())
}

type Unclassified struct {
	From error
	ir.Positioner
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

type NewSyntax struct {
	ir.Positioner
	ParserMessage string
	Hint          string
	// Antlr sometimes does not successfully return the exact Positioner.
	// In this edge case only, we can use Line, Column
	Line, Column int
	stack        []byte
	// Whether to use the Line, Column fields rather than ir.Positioner
	UseLineColumn bool
}

func (e NewSyntax) Error() string {
	return e.ParserMessage
}
func (e NewSyntax) Code() ErrCode    { return Syntax }
func (e NewSyntax) getStack() []byte { return e.stack }
func (e NewSyntax) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewMissingDiscardInWhen struct {
	ir.Positioner
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
	ir.Positioner
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
	ir.Positioner
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
	ir.Positioner
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
	ir.Positioner
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
	ir.Positioner
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

type NewNameRedeclaration struct {
	ir.Positioner
	Other ir.Positioner
	Name  string
	stack []byte
}

func (e NewNameRedeclaration) Error() string {
	// TODO ideally this should include Other
	return fmt.Sprintf("name '%s' is already declared", e.Name)
}
func (e NewNameRedeclaration) Code() ErrCode    { return NameRedeclaration }
func (e NewNameRedeclaration) getStack() []byte { return e.stack }
func (e NewNameRedeclaration) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewTypeMismatch struct {
	ir.Positioner
	AtDescription string

	Actual, Expected string

	Reason string
	stack  []byte

	// hint metadata
	ProvenanceHint    string
	ProvenanceHintPos ir.ExternalPositioner

	// type stack information
	FlowHint    string
	FlowHintPos ir.ExternalPositioner

	// ExpectedStackHints contains information about where the expected type came from
	// expected type arises from desc X from desc Y ....
	ExpectedStackHints []TypeStackHint

	// ActualStackHints contains information about where the actual type came from
	// actual type comes from desc X from desc Y ...
	ActualStackHints []TypeStackHint
}

// TypeStackHint represents a hint about a type in the constraint stack
type TypeStackHint struct {
	Description string
	Type        string
	Positioner  ir.ExternalPositioner
}

func (e NewTypeMismatch) Error() string {
	return fmt.Sprintf(`
type mismatch %s: 
  actual type:     %s
  expected type:   %s
  ...because %s`,
		e.AtDescription, e.Actual, e.Expected, e.Reason)
}
func (e NewTypeMismatch) Code() ErrCode    { return TypeMismatch }
func (e NewTypeMismatch) getStack() []byte { return e.stack }
func (e NewTypeMismatch) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewExpectedTypeParams struct {
	ir.Positioner
	Name           string
	ExpectedParams int
	stack          []byte
}

func (e NewExpectedTypeParams) Error() string {
	return fmt.Sprintf("expected %d type parameters for '%s'", e.ExpectedParams, e.Name)
}
func (e NewExpectedTypeParams) Code() ErrCode    { return ExpectedTypeParams }
func (e NewExpectedTypeParams) getStack() []byte { return e.stack }
func (e NewExpectedTypeParams) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewEmptyWhen struct {
	ir.Positioner
	stack []byte
}

func (e NewEmptyWhen) Error() string {
	return "when block is empty, but at least one case is required"
}
func (e NewEmptyWhen) Code() ErrCode    { return EmptyWhen }
func (e NewEmptyWhen) getStack() []byte { return e.stack }
func (e NewEmptyWhen) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}

type NewRepeatedRecordField struct {
	// of the record
	ir.Positioner
	// of name occurrences
	Names []ir.Positioner
	Name  string
	stack []byte
}

func (e NewRepeatedRecordField) Error() string {
	return fmt.Sprintf("record field '%s' is already defined", e.Name)
}
func (e NewRepeatedRecordField) Code() ErrCode    { return RepeatedRecordField }
func (e NewRepeatedRecordField) getStack() []byte { return e.stack }
func (e NewRepeatedRecordField) withStack(stack []byte) IleError {
	e.stack = stack
	return e
}
