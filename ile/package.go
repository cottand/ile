package ile

import (
	"fmt"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/parser"
	"go/format"
	"go/token"
	gopackages "golang.org/x/tools/go/packages"
	"io/fs"
	"log/slog"
	"os"
	"path"
	"strings"
	"testing/fstest"
)

func goLoadPkgsConfig() *gopackages.Config {
	return &gopackages.Config{
		Mode: gopackages.NeedName | gopackages.NeedImports | gopackages.NeedDeps | gopackages.NeedTypesInfo | gopackages.NeedTypes | gopackages.NeedFiles,
	}
}

var packageLogger = slog.With("section", "package")

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
	declarations map[string]ir.Type
	syntax       []ir.File
	fSet         *token.FileSet
	errors       *ilerr.Errors
	TypeCtx      *types.TypeCtx
	// originalSource is an index of filename to the character array with the original text source of each file.
	// It is used to reconstruct error messages with source text
	originalSource map[string][]rune

	//typeInfo     *infer.TypeEnv
}

func (p *Package) Syntax() []ir.File {
	return p.syntax
}

type readFileDirFS interface {
	fs.ReadFileFS
	fs.ReadDirFS
}

// LoadPackage returns a Package, where dir is the root folder for that package
//
// Only single-file filesets are supported TODO https://github.com/cottand/ile/issues/10
func LoadPackage(dir fs.FS, config PkgLoadSettings) (*Package, error) {
	dirPath := config.Dir
	if dirPath == "" {
		dirPath = "."
	}

	asReadWriteFileDirFS, ok := dir.(readFileDirFS)
	if !ok {
		return nil, fmt.Errorf("dir must be a ReadFileDirFS (TODO should support more file systems)")
	}

	// for now, ile projects must always be a single astFile
	files, err := asReadWriteFileDirFS.ReadDir(dirPath)
	if err != nil {
		return nil, err
	}
	if len(files) == 0 || files[0].IsDir() {
		return nil, err
	}
	if len(files) > 1 {
		packageLogger.Warn("multiple files found, but we do not support multiple file projects yet - using the first one")
	}
	file := files[0]
	fileOpen, err := asReadWriteFileDirFS.ReadFile(path.Join(dirPath, file.Name()))
	if err != nil {
		return nil, err
	}

	pkg := &Package{
		// TODO set path and name when we have modules
		//  https://github.com/cottand/ile/issues/8
		path:           "ilePackageNameless",
		imports:        make(map[string]*Package),
		goImports:      make(map[string]*gopackages.Package),
		declarations:   make(map[string]ir.Type),
		TypeCtx:        types.NewEmptyTypeCtx(),
		originalSource: make(map[string][]rune),
		fSet:           token.NewFileSet(),
	}

	fileInfo, err := file.Info()
	if err != nil {
		return nil, err
	}
	// keep track of source
	fileName := path.Join(config.MetadataRootDir, config.Dir, fileInfo.Name())
	fileAsString := string(fileOpen)
	fileAsRunes := []rune(fileAsString)
	tokenFile := pkg.fSet.AddFile(fileName, -1, len(fileAsRunes))
	tokenFile.AddLine(0)
	for i, c := range fileAsRunes {
		if c == '\n' {
			tokenFile.AddLine(i)
		}
	}
	pkg.originalSource[fileName] = fileAsRunes

	// parse phase
	astFile, compileErrors, err := parser.ParseToAST(fileAsString)
	pkg.errors = pkg.errors.Merge(compileErrors)
	if err != nil {
		return nil, fmt.Errorf("parse to AST: %w", err)
	}
	// if at this stage we already found ilerr errors, it is unlikely
	// we will be able to progress to inference because we won't have a proper astFile yet
	// so we bail early
	if pkg.errors.HasError() {
		return pkg, nil
	}
	// set package name while we are at it
	pkg.name = astFile.PkgName

	// desugar phase
	irFile, errorsDesugar := frontend.DesugarPhase(astFile)
	pkg.errors = pkg.errors.Merge(errorsDesugar)

	// add file to package
	pkg.syntax = append(pkg.syntax, irFile)
	for _, decl := range irFile.Declarations {
		if decl.IsPublic() {
			pkg.declarations[decl.Name] = decl.Type
		}
	}

	// gopackages resolution phase
	if len(irFile.GoImports) > 0 {
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
		for _, goImport := range irFile.GoImports {
			goImportPaths = append(goImportPaths, goImport.ImportPath)
		}
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

func (p *Package) Errors() *ilerr.Errors {
	return p.errors
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

	// MetadataRootDir the path of the root of the package, for debugging info
	MetadataRootDir string
}

// NewPackageFromBytes does all frontend passes end-to-end for a single file, meant for testing
func NewPackageFromBytes(data []byte, fileName string) (*Package, *ilerr.Errors, error) {
	filesystem := fstest.MapFS{
		path.Base(fileName): &fstest.MapFile{
			Data: data,
		},
	}
	pkg, err := LoadPackage(filesystem, PkgLoadSettings{MetadataRootDir: path.Dir(fileName)})
	if err != nil || pkg == nil {
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

type SourceFinder interface {
	FindSnippet(start, end token.Pos) (string, error)
	GetLine(pos token.Pos) (string, error)
	Position(token.Pos) token.Position
}

func findSnippet(fSet *token.FileSet, positioner ir.Positioner) (string, error) {
	startPosition := fSet.Position(positioner.Pos())
	endPosition := fSet.Position(positioner.End())
	if startPosition.Filename != endPosition.Filename {
		return "", fmt.Errorf("start and end positions are in different files")
	}
	snippetFileBytes, err := os.ReadFile(startPosition.Filename)
	if err != nil {
		return "", fmt.Errorf("could not read file %s: %w", startPosition.Filename, err)
	}
	if startPosition.Line != endPosition.Line {
		return "", fmt.Errorf("start and end positions are in different lines not implemented yet")
	}
	// go package filesets measure offsets in bytes, not runes
	if path.Ext(startPosition.Filename) == ".go" {
		snippetLine := snippetFileBytes[startPosition.Offset:(endPosition.Offset + 1)]
		return string(snippetLine), nil
	}
	snippetFile := []rune(string(snippetFileBytes))
	snippetLine := snippetFile[startPosition.Offset:(endPosition.Offset + 1)]
	return string(snippetLine), nil
}

// findLineInFileset returns the line in the original source fset for the given token.Pos
func findLineInFileset(pos token.Pos, fset *token.FileSet) (string, error) {
	file := fset.File(pos)
	if file == nil {
		return "", fmt.Errorf("could not find file for position %d", pos)
	}
	lines := file.Lines()
	line := file.Line(pos)
	if len(lines) < line {
		return "", fmt.Errorf("could not find line %d in file %s (which has %d lines)", line, file.Name(), len(lines))
	}
	var lineEnd token.Pos
	// if this was the last line
	if line+1 > file.LineCount() {
		lineEnd = token.Pos(int(file.Pos(-1)) + file.Size())
	} else {
		lineEnd = file.LineStart(line+1) - 1
	}

	snippet, err := findSnippet(fset, ir.Range{PosStart: file.LineStart(line), PosEnd: lineEnd})
	if err != nil {
		return "", fmt.Errorf("could not find snippet for line %d in file %s: %w", line, file.Name(), err)
	}
	snippet = strings.Trim(snippet, "\n")
	return snippet, nil
}

func (p *Package) Position(t token.Pos) token.Position {
	return p.fSet.Position(t)
}

// Highlight prints a snippet with the source surrounding the given ir.Positioner
// highlighting the specific text corresponding to the ir.Positioner
// with the given highlightChar
func (p *Package) Highlight(highlightChar rune, pos ir.ExternalPositioner) (string, error) {
	fSet := p.fSet
	if externalPath := pos.PackagePath(); externalPath != "" && externalPath != p.path {
		import_, ok := p.imports[externalPath]
		if ok {
			return import_.Highlight(highlightChar, pos)
		}
		goImport, ok := p.goImports[externalPath]
		if !ok {
			return "", fmt.Errorf("could not find import %s", externalPath)
		}
		fSet = goImport.Fset
	}

	line, err := findLineInFileset(pos.Pos(), fSet)
	if err != nil {
		return "", err
	}
	startPosition := fSet.Position(pos.Pos())
	columnStart := startPosition.Column
	columnEnd := fSet.Position(pos.End()).Column + 1
	indent := strings.Repeat(" ", columnStart-1)
	highlight := strings.Repeat(string(highlightChar), columnEnd-columnStart)
	return fmt.Sprintf(`
%s:
   | %s
   | %s
`, startPosition.String(), line, indent+highlight), nil
}

// DisplayTypes outputs a string which shows each declaration name and its inferred or assigned type
// in the same lines as the original source
func (p *Package) DisplayTypes() (string, error) {
	sb := strings.Builder{}
	for _, file := range p.syntax {
		for _, decl := range file.Declarations {
			sb.WriteString(fmt.Sprintf("%s: %s\n", decl.Name, ir.TypeString(decl.Type)))
		}
	}

	return sb.String(), nil
}
