package frontend

import (
	"github.com/cottand/ile/ir"
)

func InferencePhase(file ir.File) (ir.File, []ir.CompileError) {
	var cErrs = make([]ir.CompileError, 0)
	file, _ = file.TypeInfer()
	// TODO hadle error from failed inference as an ir.CompilerErr
	return file, cErrs
}
