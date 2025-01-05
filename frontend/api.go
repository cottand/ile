package frontend

import (
	"errors"
	"fmt"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/parser"
	"go/token"
	"io"
	"os"
	"path"
)

func FilesetFrom(file string) (*token.FileSet, *token.File, error) {
	file = path.Clean(file)
	open, err := os.Stat(file)
	if err != nil {
		return nil, nil, fmt.Errorf("could not stat %s: %v", file, err)
	}

	fs := token.NewFileSet()
	fsFile := fs.AddFile(path.Base(file), -1, int(open.Size()))
	return fs, fsFile, nil
}

// ParseToAST returns an ir.File without any additional processing,
// like type inference
// See ParseToIR
func ParseToAST(input io.Reader) (ast.File, []*ast.CompileError, error) {
	iStream := antlr.NewIoStream(input)
	lexer := parser.NewIleLexer(iStream)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ast.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.Result()
	return f, compileErrors, nil
}

// ParseToIR is like ParseToAST but does any additional processing needed
// to produce valid and correct Go code
func ParseToIR(reader io.Reader) (ast.File, []*ast.CompileError, error) {
	file, compileErrors, err := ParseToAST(reader)
	if err != nil {
		return ast.File{}, compileErrors, err
	}
	withInference, moreCompileErrors := InferencePhase(file)
	moreCompileErrors = append(compileErrors, moreCompileErrors...)
	return withInference, moreCompileErrors, nil
}

func InferencePhase(file ast.File) (ast.File, []*ast.CompileError) {
	var errs []*ast.CompileError
	for _, decl := range file.Declarations {
		ctx := NewContext()
		env := NewTypeEnv(nil)
		inferred, err := ctx.Infer(file.AsGroupedLet(&ast.Var{Name: decl.Name}), env)
		if err != nil {
			errs = append(errs, &ast.CompileError{
				Message: fmt.Sprintf("could not infer %s: %v", decl.Name, err),
				At:      decl,
			})
		}
		fmt.Printf("inferred for %v: %v\n", decl.Name, inferred)
	}
	return file, errs
}
