package frontend

import (
	"bytes"
	"errors"
	"fmt"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/failed"
	"github.com/cottand/ile/parser"
	"go/token"
	"io"
	"os"
	"path"
)

type CompilationCandidate struct {
	fileset  *token.FileSet
	astFiles []*ast.File
}

// FromFilesystem creates a token.FileSet out of a file path
//
// - if file is a single file, then token.FileSet is a 1-file fileset
//
// - if file is a directory, then FromFilesystem creates a Fileset out of the .ile files
// present in file
func FromFilesystem(filePath string) (fs *token.FileSet, astFiles []ast.File, res *failed.CompileResult, err error) {
	fs = token.NewFileSet()
	file := path.Clean(filePath)
	res = &failed.CompileResult{}
	stat, err := os.Stat(file)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("could not stat %s: %s", file, err)
	}

	if !stat.IsDir() {
		if path.Ext(stat.Name()) != ".ile" {
			return nil, nil, nil, fmt.Errorf("%s is not a .ile", file)
		}
		_ = fs.AddFile(path.Base(file), -1, int(stat.Size()))
		open, err := os.Open(file)
		defer open.Close()
		if err != nil {
			return nil, nil, nil, fmt.Errorf("could not open %s: %s", file, err)
		}
		var astFile ast.File
		astFile, res, err = ParseToAST(open)
		astFiles = append(astFiles, astFile)
		return fs, astFiles, res, err
	}

	open, err := os.ReadDir(file)
	if err != nil {
		return nil, nil, nil, fmt.Errorf("could not read files in %s: %v", file, err)
	}

	for _, f := range open {
		info, err := f.Info()
		if err != nil || !info.IsDir() || path.Ext(f.Name()) != ".ile" {
			continue
		}
		_ = fs.AddFile(path.Base(file), -1, int(info.Size()))

		open, err := os.Open(path.Join(file, f.Name()))

		if err != nil {
			return nil, nil, nil, fmt.Errorf("could not open %s: %s", file, err)
		}
		var astFile ast.File
		astFile, thisRes, err := ParseToAST(open)
		res = res.Merge(thisRes)
		_ = open.Close()
		astFiles = append(astFiles, astFile)
	}
	return fs, astFiles, res, err
}

// ParseToAST returns an ir.File without any additional processing,
// like type inference
// See ParseToIR
func ParseToAST(input io.Reader) (ast.File, *failed.CompileResult, error) {
	copyComments := bytes.NewBuffer(nil)
	_, _ = copyComments.ReadFrom(input)

	iStream := antlr.NewIoStream(copyComments)
	copyComments.Reset()
	// TODO figure out getting comments on a stream to parse pragmas
	//iStream2 := antlr.NewIoStream(copyComments)
	//commentStream := antlr.NewCommonTokenStream(parser.NewIleLexer(iStream), 2)
	//commentStream.Fill()
	tStream := antlr.NewCommonTokenStream(parser.NewIleLexer(iStream), antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{
		//commentStream: commentStream,
	}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ast.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.result()
	return f, compileErrors, nil
}

type PkgCompileSettings struct {
	disableBuiltins bool
}

// ParseReaderToPackage does all frontend passes end-to-end for a single file, meant for testing
func ParseReaderToPackage(reader io.Reader, settings PkgCompileSettings) (*Package, *failed.CompileResult, error) {
	file, compileErrors, err := ParseToAST(reader)
	if err != nil {
		return nil, compileErrors, err
	}
	withDesugar, errorsDesugar := desugarPhase(file)
	compileErrors = compileErrors.Merge(errorsDesugar)

	pkg, res := NewPackage("", "main", func(yield func(ast.File) bool) {
		yield(withDesugar)
	})

	if !settings.disableBuiltins {
		pkg.Import(Builtins())
	}

	compileErrors = compileErrors.Merge(res)

	withInference, errorsInference := inferencePhase(pkg)
	compileErrors = compileErrors.Merge(withInference)
	if errorsInference != nil {
		return nil, compileErrors, errorsInference
	}
	return pkg, compileErrors, nil
}
