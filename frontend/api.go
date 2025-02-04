package frontend

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"github.com/cottand/ile/parser"
	"go/token"
)

type CompilationCandidate struct {
	fileset  *token.FileSet
	astFiles []*ast.File
}

var feLogger = log.DefaultLogger.With("section", "frontend")

// ParseToAST returns an ir.File without any additional processing,
// like type inference
func ParseToAST(data string) (ast.File, *ilerr.Errors, error) {
	iStream := antlr.NewInputStream(data)
	tStream := antlr.NewCommonTokenStream(parser.NewIleLexer(iStream), antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{
		Logger: feLogger.With("section", "antlrWalker"),
	}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ast.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.result()
	return f, compileErrors, nil
}
