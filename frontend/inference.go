package frontend

import (
	"github.com/cottand/ile/ir"
)

func InferencePhase(file ir.File) (ir.File, []*ir.CompileError) {
	var errs []*ir.CompileError

	for i, funDecl := range file.Functions {
		t, errs2 := funDecl.TypeInfer()
		if errs2 != nil {
			errs = append(errs, errs2...)
			continue
		}
		fType, ok := t.(ir.FuncType)
		if !ok {
			return ir.File{}, []*ir.CompileError{{Message: "Unexpected implementor error for FuncType"}}
		}
		funDecl.Result = fType.Result
		file.Functions[i] = funDecl
	}

	for i, valDecl := range file.Values {
		t, errs2 := valDecl.TypeInfer()
		if len(errs2) > 0 {
			errs = append(errs, errs2...)
			continue
		}
		valType, ok := t.(ir.Type)
		if !ok {
			return ir.File{}, []*ir.CompileError{{Message: "Unexpected implementor error for Type"}}
		}
		valDecl.T = valType
		file.Values[i] = valDecl
	}

	return file, errs
}
