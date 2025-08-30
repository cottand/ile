//go:build js && wasm

package ile

import (
	"bytes"
	"errors"
	"fmt"
	"go/build"
	"go/format"
	"go/token"
	"strings"
	"syscall/js"

	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/traefik/yaegi/interp"
	"github.com/traefik/yaegi/stdlib"
)

// CheckAndShowTypes does a frontend pass of program
// and prints the inferred types of the program's top-level
// declarations, or alternatively displays errors messages if the
// program does not compile, type-check, or parse
var CheckAndShowTypes = asPromise(checkAndShowTypes)
func checkAndShowTypes(_ js.Value, args []js.Value) (ret any, err error) {
	program := args[0].String()
	pkg, errs, err := NewPackageFromBytes([]byte(program), "program.ile")
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}
	if errs.HasError() {
		sb := strings.Builder{}
		sb.WriteString("the program has the following errors:\n")
		for _, ileError := range errs.Errors() {
			formatted := ilerr.FormatWithCodeAndSource(ileError, pkg)
			sb.WriteString(formatted)
			sb.WriteByte('\n')
		}
		return nil, errors.New(sb.String())
	}

	typesStr, err := pkg.DisplayTypes()
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}
	return typesStr, nil
}

// CompileAndShowGoOutput does a full pass of program
// and returns the inferred types of the program's top-level
// declarations, or alternatively displays errors messages if the
// program does not compile, type-check, or parse.
// It also returns the resulting code-generated go output.
//
// output: { types: string, goOutput: string }
var CompileAndShowGoOutput = asPromise(compileAndShowGoOutput)
func compileAndShowGoOutput(_ js.Value, args []js.Value) (ret any, err error) {
	okResultObj := func(types string, goOutput string) any {
		return js.ValueOf(map[string]any{
			"types":    types,
			"goOutput": goOutput,
		})
	}

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
		return nil, fmt.Errorf(sb.String())
	}
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}

	typesStr, err := pkg.DisplayTypes()
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}

	tp := backend.NewTranspiler(pkg.TypeCtx)
	goAstFiles, err := tp.TranspilePackage(pkg.Name(), pkg.Syntax())
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}

	first := goAstFiles[0]

	sourceBuf := bytes.NewBuffer(nil)
	err = format.Node(sourceBuf, &token.FileSet{}, &first)
	if err != nil {
		return nil, fmt.Errorf("the compiler encountered a failure:\n%w", err)
	}
	return okResultObj(typesStr, sourceBuf.String()), nil
}

// InterpretGo takes a Go program as a string and returns the stdout, if any
var InterpretGo = asPromise(interpretGo)

func interpretGo(_ js.Value, args []js.Value) (ret any, err error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("expected 1 argument, got %d", len(args))
	}
	goSource := args[0].String()
	stdout := bytes.NewBuffer(nil)

	i := interp.New(interp.Options{GoPath: build.Default.GOPATH, Stdout: stdout, Stderr: stdout})
	err = i.Use(stdlib.Symbols)
	if err != nil {
		return nil, fmt.Errorf("error loading Go interpreter: %w", err)
	}

	prog, err := i.Compile(goSource)
	if err != nil {
		return nil, fmt.Errorf("error during evaluation: %w", err)
	}
	_, err = i.Execute(prog)
	if err != nil {
		return nil, fmt.Errorf("error during execution: %w", err)
	}
	return stdout.String(), nil
}

// asPromise implemented based on
// https://stackoverflow.com/questions/67437284/how-to-throw-js-error-from-go-web-assembly
//
// It takes a normal JS-API function that also returns an error, and returns function
// that returns a promise which
// completes when the function completes, and can be used to catch errors, if any
//
//   Ie, it transforms a (js.Value, []js.Value) -> (err, any)
//                into a (js.Value, []js.Value) -> Promise<any>
//
func asPromise(function func(js.Value, []js.Value) (any, error)) any {
	return js.FuncOf(func(this js.Value, args []js.Value) any {
		handler := js.FuncOf(func(_ js.Value, promiseArgs []js.Value) any {
			resolve := promiseArgs[0]
			reject := promiseArgs[1]

			go func() {
				defer func() {
					if r := recover(); r != nil {
						errorConstructor := js.Global().Get("Error")
						errorObject := errorConstructor.New(fmt.Sprintf("%s", r))
						reject.Invoke(errorObject)
					}
				}()

				data, err := function(this, args)
				if err != nil {
					// err should be an instance of `error`, eg `errors.New("some error")`
					errorConstructor := js.Global().Get("Error")
					errorObject := errorConstructor.New(err.Error())
					reject.Invoke(errorObject)
				} else {
					resolve.Invoke(js.ValueOf(data))
				}
			}()

			return nil
		})
		promiseConstructor := js.Global().Get("Promise")
		return promiseConstructor.New(handler)
	})
}
