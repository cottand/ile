package frontend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/infer"
	"github.com/cottand/ile/frontend/types"
)

// inferencePhase mutates pkg to assign types from inference as well as existing ast.TypeAnnotation
func inferencePhase(pkg *Package) (*ilerr.Errors, error) {
	errs := &ilerr.Errors{}
	newDecls := make([]ast.Declaration, len(pkg.declarations))

	envWithImports := infer.NewTypeEnv(nil)
	for _, imp := range pkg.imports {
		for _, decl := range imp.PublicDeclarations() {
			annotated, ok := decl.E.(ast.TAnnotated)
			if !ok {
				return errs, fmt.Errorf("expected annotated declaration type for %v in %v", decl.Name, imp.Path())
			}
			annotation := annotated.GetTAnnotation()
			if annotation == nil {
				panic(fmt.Sprintf("expected annotated type in public declaration %v in package %v", decl.Name, imp.Path()))
			}
			constructed, err := annotation.ConstructType(envWithImports, 0, nil)
			if err != nil {
				return errs, fmt.Errorf("failed to construct annotated for %v in %v: %v", decl.Name, imp.Path(), err)
			}
			envWithImports.Assign(decl.Name, constructed)
		}
	}

	for i, decl := range pkg.declarations {
		ctx := infer.NewContext()
		env := infer.NewTypeEnv(envWithImports)

		// TODO I would rather embed a runtime with hardcoded symbols than this black-boxy approach
		env.Declare("true", &types.Const{Name: "Bool"})
		env.Declare("false", &types.Const{Name: "Bool"})

		inferred, err := ctx.Annotate(pkg.AsGroupedLet(&ast.Var{Name: decl.Name}), env)
		if err != nil {
			var ileErr ilerr.IleError
			ok := errors.As(err, &ileErr)
			if !ok {
				errs = errs.With(ilerr.Unclassified{
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
	pkg.declarations = newDecls
	return errs, nil
}
