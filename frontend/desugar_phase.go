package frontend

import (
	"github.com/cottand/ile/frontend/ast"
)

func desugarPhase(file ast.File) (ast.File, []ast.CompileError) {
	var errs []ast.CompileError
	newDecls := make([]ast.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		newDecls[i] = decl
		newDecls[i].E = decl.E.Transform(func(expr ast.Expr) ast.Expr {
			desugared, err := desugarExpr(expr)
			if err != nil {
				errs = append(errs, err...)
			}
			return desugared
		})
	}
	file.Declarations = newDecls
	return file, errs
}

func desugarExpr(expr ast.Expr) (ast.Expr, []ast.CompileError) {
	switch expr := expr.(type) {
	case *ast.When:
		// change discard pattern in bool when for 'true' (effectively meaning 'else')
		for i := range expr.Cases {
			pred, ok := expr.Cases[i].Predicate.(*ast.Var)
			if ok && pred.Name == "_" {
				expr.Cases[i].Predicate = &ast.Var{Name: "true"}
			}
		}
	}
	return expr, nil
}
