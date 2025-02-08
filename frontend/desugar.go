package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"strings"
)

var logger = log.DefaultLogger.With("section", "desugar")

func DesugarPhase(file ast.File) (ast.File, *ilerr.Errors) {
	var res *ilerr.Errors

	newDecls := make([]ast.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		newDecls[i] = decl
		newDecls[i].E = decl.E.Transform(func(expr ast.Expr) ast.Expr {
			// insert expression desugaring here
			return expr
		})
	}
	file.Declarations = newDecls

	// split Go and ile imports
	var goImports []ast.Import
	var newImports []ast.Import
	for _, import_ := range file.Imports {
		if cut, found := strings.CutPrefix(import_.ImportPath, GoImportDirectivePrefix); found {
			goImports = append(goImports, ast.Import{
				Positioner: import_.Positioner,
				Alias:      import_.Alias,
				ImportPath: cut,
			})
		} else {
			newImports = append(newImports, import_)
		}
	}
	file.Imports = newImports
	file.GoImports = goImports

	return file, res
}

func desguarImports() {

}

const GoImportDirectivePrefix = "go:"
