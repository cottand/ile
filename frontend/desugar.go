package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"github.com/cottand/ile/util"
	"strings"
)

var logger = log.DefaultLogger.With("section", "desugar")

func DesugarPhase(file ast.File) (ast.File, *ilerr.Errors) {
	var res *ilerr.Errors
	newDecls := make([]ast.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		newDecl, newImport, err := desugarDeclPragmas(decl)
		res = res.Merge(err)
		if newImport != nil {
			file.Imports = append(file.Imports, *newImport)
		}
		newDecls[i] = newDecl
		newDecls[i].E = newDecl.E.Transform(func(expr ast.Expr) ast.Expr {
			desugared, err := desugarExpr(expr)
			res = res.Merge(err)
			return desugared
		})
	}
	file.Declarations = newDecls
	return file, res
}

func desugarExpr(expr ast.Expr) (ast.Expr, *ilerr.Errors) {
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

// FIXME actually, pragmas do not scale to interfaces and structs (iotas??)
//
//	  in practice, we'd would be asking people to redeclare everything in ile because we are too lazy
//	  to parse it!
//
//		when we implement the LSP there should be some hint
func desugarDeclPragmas(declaration ast.Declaration) (ast.Declaration, *ast.Import, *ilerr.Errors) {
	logger.Debug("external pragma badly formatted, exiting", "declName", declaration.Name)
	isPragma := len(declaration.Comments) != 0 && strings.HasPrefix(declaration.Comments[0], "ile:")
	if !isPragma {
		return declaration, nil, nil
	}

	trimmed := strings.TrimPrefix(declaration.Comments[0], "ile:")
	split, rem := util.StringTakeUntil(trimmed, ' ')
	if len(split) < 1 {
		return declaration, nil, nil
	}
	switch split {
	case "external":
		trimmedWithoutQuote := strings.TrimPrefix(rem, "\"")
		importPath, remainder := util.StringTakeUntil(trimmedWithoutQuote, '"')
		// remainder corresponds to the imported identifier
		remainder = strings.TrimSpace(remainder)
		if remainder == "" || strings.Contains(remainder, " ") {
			logger.Debug("external pragma badly formatted, exiting", "declName", declaration.Name)
			return declaration, nil, nil
		}
		logger.Debug("found properly formatted external pragma", "declName", declaration.Name)
		importAlias := util.MangledIdentFrom(declaration, "external_import_"+declaration.Name)
		import_ := ast.Import{
			Alias:      importAlias,
			ImportPath: importPath,
		}
		// TODO support non functions
		//  https://github.com/cottand/ile/issues/6
		fn, isFunc := declaration.E.(*ast.Func)
		if isFunc {
			call := &ast.Call{
				// TODO replace with qualified var in ast when implemented
				//  https://github.com/cottand/ile/issues/5
				Func: &ast.Var{Name: importAlias + "." + remainder},
				Args: nil,
			}
			for _, arg := range fn.ArgNames {
				call.Args = append(call.Args, &ast.Var{Name: arg})
			}
			fn.Body = call
			declaration.E = fn
			return declaration, &import_, nil
		}
	}
	return declaration, nil, nil
}
