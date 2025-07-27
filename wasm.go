//go:build js && wasm

package main

import (
	"github.com/cottand/ile/ile"
	"syscall/js"
)

func main() {
	js.Global().Set("CheckAndShowTypes", js.FuncOf(ile.CheckAndShowTypes))

	// wait indefinitely so that Go does not terminate execution
	// and the function remains available
	<-make(chan struct{})
}
