//go:build wasm
// +build wasm

package main

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/ile"
	"strings"
	"syscall/js"
)

// checkAndShowTypes does a frontend pass of program
// and prints the inferred types of the program's top-level
// declarations, or alternatively displays errors messages if the
// program does not compile, type-check, or parse
func checkAndShowTypes(_ js.Value, args []js.Value) any {
	program := args[0].String()
	pkg, errs, err := ile.NewPackageFromBytes([]byte(program), "program.ile")
	if err != nil {
		return fmt.Sprintf("the compiler encountered a failure:\n%w", err)
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

func main() {
	js.Global().Set("CheckAndShowTypes", js.FuncOf(checkAndShowTypes))

	// wait indefinitely so that Go does not terminate execution
	// and the function remains available
	<-make(chan struct{})
}
