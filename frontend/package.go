package frontend

import (
	"embed"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"go/token"
	gotypes "go/types"
	gopackages "golang.org/x/tools/go/packages"
	"io/fs"
	"os"
	"path"
	"testing/fstest"
)

const GoImportDirectivePrefix = "go:"

func goLoadPkgsConfig() *gopackages.Config {
	return &gopackages.Config{
		Mode: gopackages.NeedName | gopackages.NeedImports | gopackages.NeedDeps | gopackages.NeedTypesInfo | gopackages.NeedTypes,
	}
}

var packageLogger = log.DefaultLogger.With("section", "package")

// Package is a single build unit for a program
// Packages can import each other,
// and the declarations in a package are desugared.
type Package struct {
	// TODO set path and name when we have modules
	//  https://github.com/cottand/ile/issues/8
	name, path string
	imports    map[string]*Package
	goImports  map[string]*gopackages.Package

	// declarations contains the public top-level declarations of this Package.
	// For a well-formed Package, you can expect them all to have a ast.TypeAnnotation,
	// but incomplete packages may have type-less identifiers
	declarations map[string]ast.TypeAnnotation
	Syntax       []ast.File
	fSet         *token.FileSet
	errors       *ilerr.Errors
	//typeInfo     *infer.TypeEnv
}

type readFileDirFS interface {
	fs.ReadFileFS
	fs.ReadDirFS
}

// LoadPackage returns a Package, where dir is the root folder for that package
//
// Only single-file filesets are supported TODO https://github.com/cottand/ile/issues/10
func LoadPackage(dir readFileDirFS, config PkgCompileSettings) (*Package, error) {
	dirPath := config.Dir
	if dirPath == "" {
		dirPath = "."
	}
	// for now, ile projects must always be a single astFile
	files, err := dir.ReadDir(dirPath)
	if err != nil {
		return nil, err
	}
	if len(files) == 0 || files[0].IsDir() {
		return nil, err
	}
	if len(files) > 1 {
		packageLogger.Warn("multiple files found, but we do not support multi-astFile projects - using the first one")
	}
	file := files[0]
	fileOpen, err := dir.ReadFile(path.Join(dirPath, file.Name()))
	if err != nil {
		return nil, err
	}

	pkg := &Package{
		// TODO set path and name when we have modules
		//  https://github.com/cottand/ile/issues/8
		path:         "ilePackageNameless",
		name:         "ilePackageNameless",
		imports:      make(map[string]*Package),
		goImports:    make(map[string]*gopackages.Package),
		declarations: make(map[string]ast.TypeAnnotation),
	}
	fSet := token.NewFileSet()
	_ = fSet.AddFile(file.Name(), -1, len(fileOpen))

	// parse phase
	astFile, compileErrors, err := ParseToAST(string(fileOpen))
	pkg.errors = pkg.errors.Merge(compileErrors)
	if err != nil {
		return nil, fmt.Errorf("parse to AST: %w", err)
	}

	// desugar phase
	astFile, errorsDesugar := DesugarPhase(astFile)
	pkg.errors = pkg.errors.Merge(errorsDesugar)

	// add file to package
	pkg.Syntax = append(pkg.Syntax, astFile)
	for _, decl := range astFile.Declarations {
		if decl.IsPublic() {
			pkg.declarations[decl.Name] = decl.E.GetTAnnotation()
		}
	}

	// gopackages resolution phase
	if len(astFile.GoImports) > 0 {
		tmpGoDir, err := tmpGoCompileDir()
		if err != nil {
			return nil, fmt.Errorf("failed to create temporary go directory: %w", err)
		}
		if tmpGoDir != "" {
			defer os.RemoveAll(tmpGoDir)
		}
		cfg := goLoadPkgsConfig()
		var goImportPaths []string
		for _, goImport := range astFile.GoImports {
			goImportPaths = append(goImportPaths, goImport.ImportPath)
		}
		//cfg.Dir = tmpGoDir
		goPkgs, err := gopackages.Load(cfg, goImportPaths...)
		if err != nil {
			return nil, fmt.Errorf("failed to load Go gopackages: %w", err)
		}
		for _, goPkg := range goPkgs {
			if goPkg.Errors != nil {
				return nil, fmt.Errorf("errors when loading Go gopackages: %v", goPkg.Errors)
			}
			pkg.goImports[goPkg.PkgPath] = goPkg
		}
	}

	// inference phase
	errorsInference, err := inferencePhase(pkg)
	pkg.errors = pkg.errors.Merge(errorsInference)
	if err != nil {
		return pkg, err
	}

	return pkg, nil
}

func tmpGoModTemplate() string {
	return `
module ileProject

go 1.23.3
`
}

func tmpGoCompileDir() (at string, err error) {
	dir, err := os.MkdirTemp("", "ile-project-*")
	if err != nil {
		return "", err
	}
	packageLogger.Debug("created tmp dir", "path", dir)

	f, err := os.Create(path.Join(dir, "go.mod"))
	if err != nil {
		return "", err
	}
	_, err = f.WriteString(tmpGoModTemplate())
	return dir, err
}

func (p *Package) addImport(other *Package) *Package {
	if p.imports[other.path] == nil {
		p.imports[other.path] = other
	}
	for _, o := range other.imports {
		p.addImport(o)
	}
	return p
}

func (p *Package) Name() string {
	return p.name
}

func (p *Package) Path() string {
	return p.path
}

func (p *Package) AsRecord() *ast.RecordExtend {
	var recordEntries []ast.LabelValue
	for declName, type_ := range p.declarations {
		// we could swap out the literal with the actual expression and that should work, but we only
		// care about its type (and it is known, we do not need to infer it) so we just use an artificially
		// annotated literal
		exprValue := &ast.Literal{
			// the string below can be changed safely as it has no meaning, it's for debugging
			Syntax:    fmt.Sprint("< ", declName, "@", p.path, " >"),
			Construct: type_.ConstructType,
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

// AsGroupedLet allows defining an ad-hoc AST where expr can be evaluated or analysed
// in the context of this Package, by putting the ast.Declaration it contains in scope
func (p *Package) AsGroupedLet() (*ast.LetGroup, *ilerr.Errors) {
	var declBindings []ast.LetBinding
	errs := &ilerr.Errors{}
	if len(p.Syntax) == 0 {
		return nil, errs
	}
	for _, file := range p.Syntax {
		var importBindings []ast.LetBinding
		// accumulate imports for this file
		for _, goImport := range file.GoImports {
			goPkgImport, ok := p.goImports[goImport.ImportPath]
			if !ok {
				packageLogger.Warn("file import not found in package", "path", goImport.ImportPath)
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
			pkgImport, ok := p.imports[import_.ImportPath]
			if !ok {
				packageLogger.Warn("file import not found in package", "path", import_.ImportPath)
				continue
			}

			importBindings = append(importBindings, ast.LetBinding{
				Var: import_.Alias,
				// TODO this can be replaced with an ident that points to a single instance record
				//   in the typeEnv, rather than calling this for every alias
				Value: pkgImport.AsRecord(),
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

//go:embed builtins/builtins.ile
var builtinsEmbed embed.FS

// BuiltinsPackage is a meta Package that corresponds to things that will be included without imports
func BuiltinsPackage() *Package {
	pkg, err := LoadPackage(builtinsEmbed, PkgCompileSettings{
		Dir:             "builtins",
		disableBuiltins: true,
	})

	if err != nil {
		packageLogger.Error("failed to load builtin package", "err", err)
		panic(err.Error())
	}

	pkg.path = "ile/builtins"
	pkg.name = "builtins"
	return pkg
}

type PkgCompileSettings struct {
	// Dir is the path of the folder in the filesystem where the package is located
	// the default is `.`
	Dir             string
	disableBuiltins bool
}

// NewPackageFromBytes does all frontend passes end-to-end for a single file, meant for testing
func NewPackageFromBytes(data []byte) (*Package, *ilerr.Errors, error) {
	filesystem := fstest.MapFS{
		"test.ile": &fstest.MapFile{
			Data: data,
		},
	}
	pkg, err := LoadPackage(filesystem, PkgCompileSettings{})
	if err != nil {
		return nil, nil, err
	}
	pkg.name = "main"

	return pkg, pkg.errors, nil
}

func goPkgAsRecord(at ast.Positioner, pkg *gopackages.Package) (_ *ast.RecordExtend, errs *ilerr.Errors) {
	var labels []ast.LabelValue
	for _, decl := range pkg.Types.Scope().Names() {
		obj := pkg.Types.Scope().Lookup(decl)
		if obj == nil {
			packageLogger.Warn("could not find object", "name", decl, "goPkgPath", pkg.PkgPath)
			continue
		}
		if !obj.Exported() {
			continue
		}

		t := convertGoType(obj.Type())
		if t == nil {
			errs = errs.With(ilerr.New(ilerr.NewUnsupportedGoType{
				Positioner: at,
				Name:       obj.Type().String(),
			}))
			continue
		}
		labels = append(labels, ast.LabelValue{
			Label: decl,
			Value: &ast.Literal{
				Syntax:    fmt.Sprint("< ", decl, "@", pkg.PkgPath, " >"),
				Construct: t.ConstructType,
			},
		})

	}
	return &ast.RecordExtend{
		Record: &ast.RecordEmpty{},
		Labels: labels,
	}, errs
}

func convertGoType(p gotypes.Type) ast.TypeAnnotation {
	var ileType ast.TypeAnnotation
	switch t := p.(type) {
	case *gotypes.Basic:
		switch t.Kind() {
		case gotypes.Bool:
			return ast.TConst{Name: "Bool"}
		case gotypes.Invalid:
		case gotypes.Int:
		case gotypes.Int8:
		case gotypes.Int16:
		case gotypes.Int64:
			return ast.TConst{Name: "Int"}
		case gotypes.Uint:
		case gotypes.Uint16:
		case gotypes.Uint32:
		case gotypes.Uint64:
		case gotypes.Uintptr:
		case gotypes.Float32:
		case gotypes.Float64:
			return ast.TConst{Name: "Float"}
		case gotypes.Complex64:
		case gotypes.Complex128:
		case gotypes.String:
			return ast.TConst{Name: "String"}
			// TODO we do not have good support for untyped
		case gotypes.UnsafePointer:
		case gotypes.UntypedBool:
		case gotypes.UntypedInt:
		case gotypes.UntypedRune:
		case gotypes.UntypedFloat:
			packageLogger.Debug("converted untyped float", "type", t)
			return ast.TConst{Name: "Float"}
		case gotypes.UntypedComplex:
		case gotypes.UntypedString:
		case gotypes.UntypedNil:
			return ast.TConst{Name: "Nil"}
		//gotypes.Byte:
		case gotypes.Uint8:
		//gotypes.Rune
		case gotypes.Int32:
		}
	case *gotypes.Signature:
		var ret ast.TypeAnnotation
		if t.Recv() == nil {
			ret = ast.TArrow{
				Return: ast.TConst{
					Name: "Nil",
				},
			}
		}
		if t.Results().Len() > 1 {
			return nil
		}
		if t.Results().Len() == 1 {
			ret = convertGoType(t.Results().At(0).Type())
		}
		var params []ast.TypeAnnotation
		if t.Params() == nil {
			params = []ast.TypeAnnotation{}
		}
		for i := 0; i < t.Params().Len(); i++ {
			param := t.Params().At(i)
			params = append(params, convertGoType(param.Type()))
		}
		return ast.TArrow{
			Args:   params,
			Return: ret,
		}
	}
	return ileType
}
