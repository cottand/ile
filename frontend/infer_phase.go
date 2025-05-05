package frontend

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/internal/log"
	gopackages "golang.org/x/tools/go/packages"
)

//// Universe is the *infer.TypeEnv which corresponds to all symbols that do not need Imports to be used,
//// and as such do not constitute a Package
//var Universe = func() *infer.TypeEnv {
//	env := infer.NewTypeEnv(nil)
//	env.Declare("True", &hmtypes.Const{Name: "Bool"})
//	env.Declare("False", &hmtypes.Const{Name: "Bool"})
//	return env
//}()

var inferenceLogger = log.DefaultLogger.With("section", "inference")

type InferenceEnv struct {
	// GoImports references Imports by path
	GoImports map[string]*gopackages.Package
	// Imports references Imports by path
	Imports map[string]PackagePublicEnv
	Syntax  []ast.File
}

type PackagePublicEnv = map[string]ast.Type

// InferencePhase mutates pkg to assign types from inference as well as existing ast.TypeAnnotation.
// It should populate both pkg's Syntax and declarations
func InferencePhase(env InferenceEnv, ctx *types.TypeCtx) ([]ast.File, *ilerr.Errors, error) {
	errs := &ilerr.Errors{}
	files := make([]ast.File, len(env.Syntax))

	groupedPkg, err := asGroupedLet(env)
	if groupedPkg == nil {
		return files, nil, nil
	}
	errs = errs.Merge(err)

	vars := make(map[types.TypeVarID]types.SimpleType)
	for i, file := range env.Syntax {
		for j, decl := range file.Declarations {

			groupedPkg.Body = &ast.Var{
				Name: decl.Name,
			}
			_ = ctx.TypeExpr(groupedPkg, vars)
			if len(ctx.Failures) != 0 {
				panic("TODO handle this more gracefully")
			}
			errs = errs.With(ctx.Errors...)

			typed := ctx.TypeOf(decl.E)
			inferenceLogger.Debug("found type for decl", "name", decl.Name, "type", typed.ShowIn(ast.DumbShowCtx, 0))
			decl.Type = typed
			file.Declarations[j] = decl
		}
		files[i] = file
	}
	return files, errs, nil
}

// asGroupedLet allows defining an ad-hoc AST where expr can be evaluated or analysed
// in the context of this Package, by putting the ast.Declaration it contains in scope
func asGroupedLet(env InferenceEnv) (*ast.LetGroup, *ilerr.Errors) {
	var declBindings []ast.LetBinding
	errs := &ilerr.Errors{}
	if len(env.Syntax) == 0 {
		return nil, errs
	}
	for _, file := range env.Syntax {
		var importBindings []ast.LetBinding
		// accumulate Imports for this file
		for _, goImport := range file.GoImports {
			goPkgImport, ok := env.GoImports[goImport.ImportPath]
			if !ok {
				inferenceLogger.Warn("file import not found in package", "path", goImport.ImportPath)
				continue
			}
			// TODO https://github.com/cottand/ile/issues/14
			//   we ignore unsupported Go types until we plan to support them all
			record, _ := goPkgAsRecord(goImport, goPkgImport)
			if record == nil {
				continue
			}
			importBindings = append(importBindings, ast.LetBinding{
				Var: goImport.Alias,
				// TODO this can be replaced with an ident that points to a single instance record
				//   in the typeEnv, rather than calling this for every alias
				Value: record,
			})
		}
		for _, import_ := range file.Imports {
			pkgImport, ok := env.Imports[import_.ImportPath]
			if !ok {
				inferenceLogger.Warn("file import not found in package", "path", import_.ImportPath)
				continue
			}

			importBindings = append(importBindings, ast.LetBinding{
				Var: import_.Alias,
				// TODO this can be replaced with an ident that points to a single instance record
				//   in the typeEnv, rather than calling this for every alias
				Value: asRecord(pkgImport),
			})
		}

		// add this file's declarations to the letGroup
		// for each declaration we add the imported packages with their aliases
		// via a nested LetGroup
		for _, decl := range file.Declarations {
			declBindings = append(declBindings, ast.LetBinding{
				Var: decl.Name,
				Value: &ast.LetGroup{
					Vars: importBindings,
					Body: decl.E,
				},
			})
		}
	}
	return &ast.LetGroup{
		Vars: declBindings,
		Body: nil,
	}, errs
}

func goPkgAsRecord(at ast.Positioner, pkg *gopackages.Package) (_ *ast.RecordExtend, errs *ilerr.Errors) {
	var labels []ast.LabelValue
	for _, decl := range pkg.Types.Scope().Names() {
		obj := pkg.Types.Scope().Lookup(decl)
		if obj == nil {
			inferenceLogger.Warn("could not find object", "name", decl, "goPkgPath", pkg.PkgPath)
			continue
		}
		if !obj.Exported() {
			continue
		}

		t := convertGoType(obj.Type(), ast.Range{PosStart: obj.Pos(), PosEnd: obj.Pos()})
		if t == nil {
			errs = errs.With(ilerr.New(ilerr.NewUnsupportedGoType{
				Positioner: at,
				Name:       obj.Type().String(),
			}))
			continue
		}
		labels = append(labels, ast.LabelValue{
			Label: decl,
			Value: &ast.Ascribe{
				Expr:  &ast.Var{Name: fmt.Sprint("< ", decl, "@", pkg.PkgPath, " >")},
				Type_: t,
			},
		})

	}
	return &ast.RecordExtend{
		Record: &ast.RecordEmpty{},
		Labels: labels,
	}, errs
}

func asRecord(pkg PackagePublicEnv) *ast.RecordExtend {
	var recordEntries []ast.LabelValue
	for declName, type_ := range pkg {
		// we could swap out the literal with the actual expression and that should work, but we only
		// care about its type (and it is known, we do not need to infer it) so we just use an artificially
		// annotated literal
		exprValue := &ast.Ascribe{
			Expr: &ast.Var{
				// the string below can be changed safely as it has no meaning, it's for debugging
				Name: declName,
			},
			Type_: type_,
		}
		recordEntries = append(recordEntries, ast.LabelValue{
			Label: declName,
			Value: exprValue,
		})
	}
	return &ast.RecordExtend{
		Record: &ast.RecordEmpty{},
		Labels: recordEntries,
	}
}
