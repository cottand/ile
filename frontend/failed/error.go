package failed

import "go/ast"

type CompileResult struct {
	errs []IleError
}

func (r *CompileResult) With(err ...IleError) *CompileResult {
	if r == nil {
		return &CompileResult{errs: err}
	}
	for _, err := range err {
		r.errs = append(r.errs, err)
	}
	return r
}

func (r *CompileResult) Merge(err *CompileResult) *CompileResult {
	if r == nil {
		return err
	}
	if err == nil {
		return r
	}
	if len(err.errs) == 0 {
		return r
	}
	return r.With(err.errs...)
}

func (r *CompileResult) Errors() []IleError {
	return r.errs
}

func (r *CompileResult) HasError() bool {
	return len(r.errs) > 0
}

type CompileError struct {
	Message string
	At      ast.Node
}
