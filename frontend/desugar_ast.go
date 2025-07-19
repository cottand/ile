package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"strings"
)

// DesugarPhase accepts an ast.File and returns an ir.File and any errors encountered during the conversion.
// It follows the pattern of DesugarPhaseOld but constructs an IR from an AST.
func DesugarPhase(file ast.File) (ir.File, *ilerr.Errors) {
	res := &ilerr.Errors{}

	irFile := ir.File{
		Range:        ir.Range{PosStart: file.Range.PosStart, PosEnd: file.Range.PosEnd},
		PkgName:      file.PkgName,
		Declarations: []ir.Declaration{},
		Imports:      []ir.Import{},
		GoImports:    []ir.Import{},
	}

	// Split Go and ile imports
	for _, import_ := range file.Imports {
		if cut, found := strings.CutPrefix(import_.ImportPath, GoImportDirectivePrefix); found {
			irFile.GoImports = append(irFile.GoImports, ir.Import{
				Positioner: ir.Range{PosStart: import_.Range.PosStart, PosEnd: import_.Range.PosEnd},
				Alias:      import_.Alias,
				ImportPath: cut,
			})
		} else {
			irFile.Imports = append(irFile.Imports, ir.Import{
				Positioner: ir.Range{PosStart: import_.Range.PosStart, PosEnd: import_.Range.PosEnd},
				Alias:      import_.Alias,
				ImportPath: import_.ImportPath,
			})
		}
	}

	// Convert declarations
	for _, decl := range file.Declarations {
		var irExpr ir.Expr
		var irType ir.Type

		// Convert expression
		if decl.Value != nil {
			irExpr = desugarExpr(decl.Value)
		}

		// Convert type annotation
		if decl.TypeAnn != nil {
			irType = ConvertType(decl.TypeAnn)

			// If we have both an expression and a type annotation, wrap the expression in an Ascribe
			if irExpr != nil {
				irExpr = &ir.Ascribe{
					Expr:  irExpr,
					Type_: irType,
					Range: ir.Range{PosStart: decl.Range.PosStart, PosEnd: decl.Range.PosEnd},
				}
			}
		}

		irDecl := ir.Declaration{
			Range:    ir.Range{PosStart: decl.Range.PosStart, PosEnd: decl.Range.PosEnd},
			Name:     decl.Name,
			Comments: decl.Comments,
			E:        irExpr,
			Type:     irType,
		}
		irFile.Declarations = append(irFile.Declarations, irDecl)
	}

	return irFile, res
}
