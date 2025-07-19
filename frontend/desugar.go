package frontend

import (
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"strings"
)


func DesugarPhaseOld(file ir.File) (ir.File, *ilerr.Errors) {
	var res *ilerr.Errors

	newDecls := make([]ir.Declaration, len(file.Declarations))
	for i, decl := range file.Declarations {
		newDecls[i] = decl
		newDecls[i].E = decl.E.Transform(func(expr ir.Expr) ir.Expr {
			// insert expression desugaring here
			return expr
		})
	}
	file.Declarations = newDecls

	// split Go and ile imports
	var goImports []ir.Import
	var newImports []ir.Import
	for _, import_ := range file.Imports {
		if cut, found := strings.CutPrefix(import_.ImportPath, GoImportDirectivePrefix); found {
			goImports = append(goImports, ir.Import{
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

const GoImportDirectivePrefix = "go:"
