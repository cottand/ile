package frontend

import (
	"embed"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"go/token"
	"golang.org/x/tools/go/packages"
	"io/fs"
	"iter"
	"path"
	"testing/fstest"
)

var packageLogger = log.DefaultLogger.With("section", "package")

// Package is a single build unit for a program
// Packages can import each other,
// and the declarations in a package are desugared.
type Package struct {
	// TODO set path and name when we have modules
	//  https://github.com/cottand/ile/issues/8
	name, path   string
	imports      map[string]*Package
	goImports    map[string]*packages.Package
	declarations []ast.Declaration
	fSet         *token.FileSet
	errors       *ilerr.Errors
	//typeInfo     *infer.TypeEnv
}

type readFileDirFS interface {
	fs.ReadFileFS
	fs.ReadDirFS
}

// LoadPackage returns a Package, where dir is the root folder for that package
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
		return nil, fmt.Errorf("no files found")
	}
	if len(files) > 1 {
		packageLogger.Warn("multiple files found, but we do not support multi-astFile projects - using the first one")
	}
	file := files[0]
	fileOpen, err := dir.ReadFile(path.Join(dirPath, file.Name()))
	if err != nil {
		return nil, err
	}

	fSet := token.NewFileSet()
	_ = fSet.AddFile(file.Name(), -1, len(fileOpen))

	// parse phase
	astFile, compileErrors, err := ParseToAST(string(fileOpen))
	if err != nil {
		return nil, err
	}

	// desugar phase
	astFile, errorsDesugar := DesugarPhase(astFile)
	compileErrors = compileErrors.Merge(errorsDesugar)

	pkg := &Package{
		// TODO set path and name when we have modules
		//  https://github.com/cottand/ile/issues/8
		path:    "ilePackageNameless",
		name:    "ilePackageNameless",
		imports: make(map[string]*Package),
	}
	for _, decl := range astFile.Declarations {
		pkg.declarations = append(pkg.declarations, decl)
	}
	pkg.errors = pkg.errors.Merge(compileErrors)

	if !config.disableBuiltins {
		pkg.imports["builtins"] = BuiltinsPackage()
	}

	// inference phase
	errorsInference, err := inferencePhase(pkg)
	pkg.errors = pkg.errors.Merge(errorsInference)
	if err != nil {
		return pkg, err
	}

	return pkg, nil
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

func (p *Package) AsGroupedLet(in ast.Expr) *ast.LetGroup {
	bindings := make([]ast.LetBinding, len(p.declarations))
	for i, decl := range p.declarations {
		bindings[i] = ast.LetBinding{
			Var:   decl.Name,
			Value: decl.E,
		}
	}
	return &ast.LetGroup{
		Vars: bindings,
		Body: in,
	}
}

func (p *Package) Declarations() []ast.Declaration {
	return p.declarations
}

func (p *Package) PublicDeclarations() iter.Seq2[int, ast.Declaration] {
	return func(yield func(int, ast.Declaration) bool) {
		i := 0
		for _, decl := range p.Declarations() {
			if decl.IsPublic() {
				if !yield(i, decl) {
					return
				}
				i += 1
			}
		}
	}
}

//go:embed builtins/builtins.ile
var builtinsEmbed embed.FS

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
