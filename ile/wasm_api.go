//go:build js && wasm

package ile

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
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
