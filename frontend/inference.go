package frontend

import (
	"github.com/cottand/ile/ir"
)

func InferencePhase(file ir.File) (ir.File, []ir.CompileError) {
	var cErrs = make([]ir.CompileError, 0)
	return file.TypeInfer(), cErrs
}
