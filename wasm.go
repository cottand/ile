//go:build js && wasm

package main

import (
	"syscall/js"

	"github.com/cottand/ile/ile"
)

func main() {
	js.Global().Set("CheckAndShowTypes", ile.CheckAndShowTypes)
	js.Global().Set("CompileAndShowGoOutput", ile.CompileAndShowGoOutput)
	js.Global().Set("InterpretGo", ile.InterpretGo)

	// wait indefinitely so that Go does not terminate execution
	// and the function remains available
	<-make(chan struct{})
}
