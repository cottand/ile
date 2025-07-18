package frontend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	gopackages "golang.org/x/tools/go/packages"
	"log/slog"
)

//// Universe is the *infer.TypeEnv which corresponds to all symbols that do not need Imports to be used,
//// and as such do not constitute a Package
//var Universe = func() *infer.TypeEnv {
//	env := infer.NewTypeEnv(nil)
//	env.Declare("True", &hmtypes.Const{Name: "Bool"})
//	env.Declare("False", &hmtypes.Const{Name: "Bool"})
//	return env
//}()


type InferenceEnv struct {
	// GoImports references Imports by path
	GoImports map[string]*gopackages.Package
	// Imports references Imports by path
	Imports map[string]PackagePublicEnv
	Syntax  []ir.File
}

type PackagePublicEnv = map[string]ir.Type

// InferencePhase mutates pkg to assign types from inference as well as existing ast.TypeAnnotation.
// It should populate both pkg's Syntax and declarations
func InferencePhase(env InferenceEnv, ctx *types.TypeCtx) ([]ir.File, *ilerr.Errors, error) {
	var inferenceLogger = slog.With("section", "inference")
	errs := &ilerr.Errors{}
	files := make([]ir.File, len(env.Syntax))

	ctx, groupedPkg, err := asGroupedLetWithImports(env, ctx)
	if groupedPkg == nil {
		return files, nil, nil
	}
	errs = errs.Merge(err)

	vars := make(map[string]types.SimpleType)
	totalFailures := make([]error, 0)
	for i, file := range env.Syntax {
		for j, decl := range file.Declarations {

			groupedPkg.Body = &ir.Var{
				Name: decl.Name,
			}
			_ = ctx.TypeExpr(groupedPkg, vars)
			totalFailures = append(totalFailures, ctx.Failures...)
			if len(ctx.Failures) > 0 {
				continue
			}
			errs = errs.With(ctx.Errors...)

			typed := ctx.TypeOf(decl.E)
			inferenceLogger.Debug("found type for decl", "name", decl.Name, "type", typed.ShowIn(ir.DumbShowCtx, 0))
			decl.Type = typed
			file.Declarations[j] = decl
		}
		files[i] = file
	}
	if len(totalFailures) > 0 {
		return files, nil, fmt.Errorf("failures found:\n %s", errors.Join(totalFailures...))
	}
	return files, errs, nil
}

func identifierForPackage(pkgPath string) string {
	return fmt.Sprintf("<%s>", pkgPath)
}

func processImports(ctx *types.TypeCtx, env InferenceEnv) *types.TypeCtx {
	typeDefs := make([]ir.TypeDefinition, 0, len(env.Imports)+len(env.GoImports))
	bindings := make(map[string]ir.Type, len(env.Imports)+len(env.GoImports))

	for _, goImport := range env.GoImports {
		typeDef, err := importGoAsRecordTypeDef(goImport)
		ctx.Errors = append(ctx.Errors, err.Errors()...)
		typeDefs = append(typeDefs, typeDef)
		bindings[identifierForPackage(goImport.PkgPath)] = &ir.TypeName{Name: typeDef.Name.Name, Range: ir.Range{}}
	}

	for path, pkg := range env.Imports {
		typeDef := importAsRecordTypeDef(pkg)
		typeDefs = append(typeDefs, typeDef)
		bindings[identifierForPackage(path)] = &ir.TypeName{Name: typeDef.Name.Name, Range: ir.Range{}}
	}
	// add the type definitions for the import, and then the binding to reference those imports
	// (so the binding will have the type of the type definition, which will be a record with all the exported decls)
	ctx = ctx.ProcessTypeDefs(typeDefs).WithBindings(bindings)

	return ctx
}

// importAsRecordTypeDef creates a TypeDefinition for a Go package that corresponds
// to a record type, where each exported identifier is a field of the record.
func importGoAsRecordTypeDef(pkg *gopackages.Package) (def ir.TypeDefinition, errs *ilerr.Errors) {
	var inferenceLogger = slog.With("section", "inference")
	fields := make([]ir.RecordField, 0, len(pkg.Types.Scope().Names()))

	for _, decl := range pkg.Types.Scope().Names() {
		obj := pkg.Types.Scope().Lookup(decl)
		if obj == nil {
			inferenceLogger.Warn("could not find object", "name", decl, "goPkgPath", pkg.PkgPath)
			continue
		}
		if !obj.Exported() {
			continue
		}
		fields = append(fields, ir.RecordField{Name: ir.Var{Name: decl}, Type: ir.FieldType{Out: &ir.GoType{
			Underlying:  obj,
			PackagePath: pkg.PkgPath,
			PackageName: pkg.Name,
			// SourceFile is left empty for now as we don't have direct access to the source file information
		}}})
	}
	return ir.TypeDefinition{
		Kind: ir.KindAlias,
		Name: ir.TypeName{
			Name: pkg.PkgPath,
		},
		Body: &ir.RecordType{
			Fields: fields,
		},
		//Provenance: fmt.Sprintf("Go package (imported from %s)", pkg.PkgPath),
	}, errs
}

// TODO NOT DONE
func importAsRecordTypeDef(pkg PackagePublicEnv) ir.TypeDefinition {

	var recordEntries []ir.LabelValue
	for declName, type_ := range pkg {
		// we could swap out the literal with the actual expression and that should work, but we only
		// care about its type (and it is known, we do not need to infer it) so we just use an artificially
		// annotated literal
		exprValue := &ir.Ascribe{
			Expr: &ir.Var{
				// the string below can be changed safely as it has no meaning, it's for debugging
				Name: declName,
			},
			Type_: type_,
		}
		recordEntries = append(recordEntries, ir.LabelValue{
			Label: ir.Var{Name: declName},
			Value: exprValue,
		})
	}
	return ir.TypeDefinition{}
}

// asGroupedLetWithImports allows defining an ad-hoc AST where expr can be evaluated or analysed
// in the context of this Package, by putting the ast.Declaration it contains in scope
// as well as adding all imports of env to ctx
func asGroupedLetWithImports(env InferenceEnv, ctx *types.TypeCtx) (*types.TypeCtx, *ir.LetGroup, *ilerr.Errors) {
	var inferenceLogger = slog.With("section", "inference")
	var declBindings []ir.LetBinding
	errs := &ilerr.Errors{}
	if len(env.Syntax) == 0 {
		return nil, nil, errs
	}

	// adds the imports as names to the TypeCtx
	ctx = processImports(ctx, env)

	for _, file := range env.Syntax {
		var importBindings []ir.LetBinding
		// accumulate Imports for this file
		for _, goImport := range file.GoImports {
			_, ok := env.GoImports[goImport.ImportPath]
			if !ok {
				inferenceLogger.Warn("file import not found in package", "path", goImport.ImportPath)
				continue
			}
			importBindings = append(importBindings, ir.LetBinding{
				Var: goImport.Alias,
				// the context now has a record type for this package, referenced from the import path
				// (thanks to processImports)
				Value: &ir.Var{
					Name:  identifierForPackage(goImport.ImportPath),
					Range: ir.RangeOf(goImport),
				},
			})
		}

		for _, import_ := range file.Imports {
			_, ok := env.Imports[import_.ImportPath]
			if !ok {
				inferenceLogger.Warn("file import not found in package", "path", import_.ImportPath)
				continue
			}

			importBindings = append(importBindings, ir.LetBinding{
				Var: import_.Alias,
				// TODO this can be replaced with an ident that points to a single instance record
				//   in the typeEnv, rather than calling this for every alias
				Value: &ir.Var{
					Name:  identifierForPackage(import_.ImportPath),
					Range: ir.RangeOf(import_),
				},
			})
		}

		// add this file's declarations to the letGroup
		// for each declaration we add the imported packages with their aliases
		// via a nested LetGroup
		for _, decl := range file.Declarations {
			declBindings = append(declBindings, ir.LetBinding{
				Var: decl.Name,
				Value: &ir.LetGroup{
					Vars:  importBindings,
					Body:  decl.E,
					Range: decl.Range,
				},
			})
		}
	}
	return ctx, &ir.LetGroup{
		Vars: declBindings,
		Body: nil,
	}, errs
}
