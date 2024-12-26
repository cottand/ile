package frontend

import (
	"github.com/cottand/ile/ir"
)

func InferencePhase(file ir.File) (ir.File, []*ir.CompileError) {
	file, inferenceErr := file.TypeInfer()

	return file, inferenceErr
}
