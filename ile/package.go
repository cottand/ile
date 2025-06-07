package ile

import (
	"fmt"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/internal/log"
	"go/format"
	"go/token"
	gopackages "golang.org/x/tools/go/packages"
	"io/fs"
	"os"
	"path"
	"testing/fstest"
)

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
	declarations map[string]ast.Type
	syntax       []ast.File
	fSet         *token.FileSet
	errors       *ilerr.Errors
	TypeCtx      *types.TypeCtx

	//typeInfo     *infer.TypeEnv
}

func (p *Package) Syntax() []ast.File {
	return p.syntax
}

type readFileDirFS interface {
	fs.ReadFileFS
	fs.ReadDirFS
}

// LoadPackage returns a Package, where dir is the root folder for that package
//
// Only single-file filesets are supported TODO https://github.com/cottand/ile/issues/10
func LoadPackage(dir readFileDirFS, config PkgLoadSettings) (*Package, error) {
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
		packageLogger.Warn("multiple Syntax found, but we do not support multi-astFile projects - using the first one")
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
		declarations: make(map[string]ast.Type),
		TypeCtx:      types.NewEmptyTypeCtx(),
	}
	fSet := token.NewFileSet()
	_ = fSet.AddFile(file.Name(), -1, len(fileOpen))

	// parse phase
	astFile, compileErrors, err := frontend.ParseToAST(string(fileOpen))
	pkg.errors = pkg.errors.Merge(compileErrors)
	if err != nil {
		return nil, fmt.Errorf("parse to AST: %w", err)
	}

	// desugar phase
	astFile, errorsDesugar := frontend.DesugarPhase(astFile)
	pkg.errors = pkg.errors.Merge(errorsDesugar)

	// add file to package
	pkg.syntax = append(pkg.syntax, astFile)
	for _, decl := range astFile.Declarations {
		if decl.IsPublic() {
			pkg.declarations[decl.Name] = decl.Type
		}
	}

	// gopackages resolution phase
	if len(astFile.GoImports) > 0 {
		tmpGoDir, err := tmpGoCompileDir()
		if err != nil {
			return nil, fmt.Errorf("failed to create temporary go directory: %w", err)
		}
		if tmpGoDir != "" {
			defer func(path string) {
				_ = os.RemoveAll(path)
			}(tmpGoDir)
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
	var errorsInference *ilerr.Errors
	pkg.syntax, errorsInference, err = frontend.InferencePhase(pkg.inferenceEnv(), pkg.TypeCtx)
	pkg.errors = pkg.errors.Merge(errorsInference)
	return pkg, err
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

func (p *Package) inferenceEnv() (env frontend.InferenceEnv) {
	env.Syntax = p.syntax
	env.GoImports = p.goImports
	env.Imports = make(map[string]frontend.PackagePublicEnv)
	for name, pkg := range p.imports {
		env.Imports[name] = pkg.declarations
	}
	return env
}

type PkgLoadSettings struct {
	// Dir is the path of the folder in the filesystem where the package is located
	// the default is `.`
	Dir string
}

// NewPackageFromBytes does all frontend passes end-to-end for a single file, meant for testing
func NewPackageFromBytes(data []byte) (*Package, *ilerr.Errors, error) {
	filesystem := fstest.MapFS{
		"test.ile": &fstest.MapFile{
			Data: data,
		},
	}
	pkg, err := LoadPackage(filesystem, PkgLoadSettings{})
	if err != nil && pkg == nil {
		return nil, nil, err
	}
	pkg.name = "test"
	return pkg, pkg.errors, err
}

// WriteTranspiledModule writes this Package as a Go module in dir
func (p *Package) WriteTranspiledModule(dir string) error {
	goModFilePath := path.Join(dir, "go.mod")
	goModFile, err := os.Create(goModFilePath)
	if err != nil {
		return fmt.Errorf("create go.mod: %w", err)
	}
	_, err = goModFile.WriteString(tmpGoModTemplate())
	if err != nil {
		return fmt.Errorf("write go.mod: %w", err)
	}

	tp := backend.NewTranspiler(p.TypeCtx)
	goFiles, err := tp.TranspilePackage(p.Name(), p.syntax)

	for i, goAstFile := range goFiles {
		filePath := path.Join(dir, fmt.Sprintf("%s.%d.go", p.name, i))
		file, err := os.Create(filePath)
		if err != nil {
			return fmt.Errorf("create %s: %w", filePath, err)
		}
		err = format.Node(file, token.NewFileSet(), &goAstFile)
		if err != nil {
			return fmt.Errorf("format %s: %w", filePath, err)
		}
	}
	return err
}
