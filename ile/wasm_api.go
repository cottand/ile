//go:build js && wasm

package ile

import (
	"bytes"
	"fmt"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend/ilerr"
	"go/format"
	"go/token"
	"strings"
	"syscall/js"
)

// CheckAndShowTypes does a frontend pass of program
// and prints the inferred types of the program's top-level
// declarations, or alternatively displays errors messages if the
// program does not compile, type-check, or parse
func CheckAndShowTypes(_ js.Value, args []js.Value) (ret any) {
	defer func() {
		if r := recover(); r != nil {
			ret = "compiler panicked: " + fmt.Sprint(r)
		}
	}()

	program := args[0].String()
	pkg, errs, err := NewPackageFromBytes([]byte(program), "program.ile")
	if err != nil {
		return fmt.Sprintf("the compiler encountered a failure:\n\n%s", err)
	}
	if errs.HasError() {
		sb := strings.Builder{}
		sb.WriteString("the program has the following errors:\n")
		for _, ileError := range errs.Errors() {
			formatted := ilerr.FormatWithCodeAndSource(ileError, pkg)
			sb.WriteString(formatted)
			sb.WriteByte('\n')
		}
		return sb.String()
	}

	typesStr, err := pkg.DisplayTypes()
	if err != nil {
		return fmt.Sprintf("the compiler encountered a failure:\n%w", err)
	}
	return typesStr
}

// CompileAndShowGoOutput does a full pass of program
// and returns the inferred types of the program's top-level
// declarations, or alternatively displays errors messages if the
// program does not compile, type-check, or parse.
// It also returns the resulting code-generated go output.
//
// output: { error: string } | { types: string, goOutput: string }
func CompileAndShowGoOutput(_ js.Value, args []js.Value) (ret any) {
	errorObj := func(err string) any {
		return js.ValueOf(map[string]any{
			"error": err,
		})
	}

	okResultObj := func(types string, goOutput string) any {
		return js.ValueOf(map[string]any{
			"types":    types,
			"goOutput": goOutput,
		})
	}
	defer func() {
		if r := recover(); r != nil {
			ret = errorObj("compiler panicked: " + fmt.Sprint(r))
		}
	}()

	program := args[0].String()
	pkg, errs, err := NewPackageFromBytes([]byte(program), "program.ile")
	if errs.HasError() {
		sb := strings.Builder{}
		sb.WriteString("the program has the following errors:\n")
		for _, ileError := range errs.Errors() {
			formatted := ilerr.FormatWithCodeAndSource(ileError, pkg)
			sb.WriteString(formatted)
			sb.WriteByte('\n')
		}
		return errorObj(sb.String())
	}
	if err != nil {
		return errorObj(fmt.Sprintf("the compiler encountered a failure:\n\n%s", err))
	}

	typesStr, err := pkg.DisplayTypes()
	if err != nil {
		return errorObj(fmt.Sprintf("the compiler encountered a failure:\n%s", err))
	}

	tp := backend.NewTranspiler(pkg.TypeCtx)
	goAstFiles, err := tp.TranspilePackage(pkg.Name(), pkg.Syntax())
	if err != nil {
		return errorObj(fmt.Sprintf("the compiler encountered a failure:\n%s", err))
	}

	first := goAstFiles[0]

	sourceBuf := bytes.NewBuffer(nil)
	err = format.Node(sourceBuf, &token.FileSet{}, &first)
	if err != nil {
		return errorObj(fmt.Sprintf("the interpreter encountered a failure:\n%s", err))
	}
	return okResultObj(typesStr, sourceBuf.String())
}
