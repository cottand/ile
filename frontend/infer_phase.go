package frontend

import (
	"errors"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/infer"
	"github.com/cottand/ile/frontend/types"
)

// Universe is the *infer.TypeEnv which corresponds to all symbols that do not need imports to be used,
// and as such do not constitute a Package
var Universe = func() *infer.TypeEnv {
	env := infer.NewTypeEnv(nil)
	env.Declare("True", &types.Const{Name: "Bool"})
	env.Declare("False", &types.Const{Name: "Bool"})
	return env
}()

// inferencePhase mutates pkg to assign types from inference as well as existing ast.TypeAnnotation
func inferencePhase(pkg *Package) (*ilerr.Errors, error) {
	errs := &ilerr.Errors{}

	groupedPkg, err := pkg.AsGroupedLet()
	if groupedPkg == nil {
		return nil, nil
	}
	errs = errs.Merge(err)
	for i, file := range pkg.Syntax {
		for j, decl := range file.Declarations {
			ctx := infer.NewContext()
			env := infer.NewTypeEnv(Universe)

			groupedPkg.Body = &ast.Var{
				Name: decl.Name,
			}
			newExpr, err := ctx.Annotate(groupedPkg, env)
			if err != nil {
				var ileErr ilerr.IleError
				ok := errors.As(err, &ileErr)
				if !ok {
					errs = errs.With(ilerr.Unclassified{
						From:       err,
						Positioner: file,
					})
				} else {
					errs = errs.With(ileErr)
				}
			}
			asLetGroup := newExpr.(*ast.LetGroup)
			for _, annotated := range asLetGroup.Vars {
				if annotated.Var == decl.Name {
					// skip over the imports legGroup as imports will be in scope in the compile phase
					file.Declarations[j].E = annotated.Value.(*ast.LetGroup).Body
					packageLogger.Debug("successfully annotated decl", "name", decl.Name)
				}
			}
		}
		pkg.Syntax[i] = file
	}
	return errs, nil
}
