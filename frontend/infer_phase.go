package frontend

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/infer"
)

func inferencePhase(file ast.File) (ast.File, []ast.CompileError) {
	var errs []ast.CompileError
	newDecls := make([]ast.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		ctx := infer.NewContext()
		env := infer.NewTypeEnv(nil)
		inferred, err := ctx.Annotate(file.AsGroupedLet(&ast.Var{Name: decl.Name}), env)
		if err != nil {
			errs = append(errs, ast.CompileError{
				Message: fmt.Sprintf("could not infer %s: %v", decl.Name, err),
				At:      decl,
			})
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
