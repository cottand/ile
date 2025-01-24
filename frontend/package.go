package frontend

import (
	"embed"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/failed"
	"iter"
	"maps"
	"slices"
)

// Package is a single build unit for a program
// Packages can import each other,
// and the declarations in a package are desugared.
type Package struct {
	name, path   string
	imports      map[string]*Package
	declarations []ast.Declaration

	//scope      *infer.TypeEnv
}

// NewPackage returns a valid package from a set of files
//
// Note a valid file has type signatures on all its public Declarations
func NewPackage(path, name string, from iter.Seq[ast.File]) (*Package, *failed.CompileResult) {
	namesFound := make(map[string]int)
	decls := make([]ast.Declaration, 0)
	res := &failed.CompileResult{}
	for f := range from {
		namesFound[f.PkgName]++
		for _, decl := range f.Declarations {
			//if decl.IsPublic() {
			decls = append(decls, decl)
			//}
		}
	}
	if len(namesFound) > 1 {
		res = res.With(failed.NewManyPackageNamesInPackage{
			Positioner: decls[0],
			Names:      slices.Collect(maps.Keys(namesFound)),
		})
	}

	pkg := Package{
		path: path,
		name: name,
		//scope: infer.NewTypeEnv(nil),
		imports:      make(map[string]*Package),
		declarations: decls,
	}

	return &pkg, res
}

func (p *Package) Import(other *Package) *Package {
	if p.imports[other.path] == nil {
		p.imports[other.path] = other
	}
	for _, o := range other.imports {
		p.Import(o)
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

func Builtins() *Package {
	open, err := builtinsEmbed.Open("builtins/builtins.ile")
	if err != nil {
		panic(err)
	}
	pkg, _, _ := ParseReaderToPackage(open, PkgCompileSettings{
		disableBuiltins: true,
	})
	pkg.path = "ile/builtins"
	pkg.name = "builtins"
	return pkg
}
