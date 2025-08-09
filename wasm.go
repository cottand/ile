//go:build js && wasm

package main

import (
	"github.com/cottand/ile/ile"
	"syscall/js"
)

func main() {
	js.Global().Set("CheckAndShowTypes", js.FuncOf(ile.CheckAndShowTypes))
	js.Global().Set("CompileAndShowGoOutput", js.FuncOf(ile.CompileAndShowGoOutput))
	js.Global().Set("InterpretGo", ile.InterpretGo)

	// wait indefinitely so that Go does not terminate execution
	// and the function remains available
	<-make(chan struct{})
}
