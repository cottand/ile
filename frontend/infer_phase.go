package frontend

import (
	"errors"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/failed"
	"github.com/cottand/ile/frontend/infer"
	"github.com/cottand/ile/frontend/types"
)

func inferencePhase(file ast.File) (ast.File, *failed.CompileResult) {
	errs := &failed.CompileResult{}
	newDecls := make([]ast.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		ctx := infer.NewContext()
		env := infer.NewTypeEnv(nil)
		// TODO I would rather embed a runtime with hardcoded symbols than this black-boxy approach
		env.Declare("true", &types.Const{Name: "Bool"})
		env.Declare("false", &types.Const{Name: "Bool"})
		inferred, err := ctx.Annotate(file.AsGroupedLet(&ast.Var{Name: decl.Name}), env)
		if err != nil {
			var ileErr failed.IleError
			ok := errors.As(err, &ileErr)
			if !ok {
				errs = errs.With(failed.Unclassified{
					From:       err,
					Positioner: decl,
				})
			} else {
				errs = errs.With(ileErr)
			}
		}
		// should not fail as this was the original type of expr
		inferredAsGroupedLet := inferred.(*ast.LetGroup)
		// currentDecl should now be annotated with a type!
		currentDecl := inferredAsGroupedLet.Vars[i]
		decl.E = currentDecl.Value
		newDecls[i] = decl
	}
	file.Declarations = newDecls
	return file, errs
}
