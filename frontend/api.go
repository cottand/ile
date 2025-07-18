package frontend

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/parser"
	"go/token"
	"log/slog"
)

type CompilationCandidate struct {
	fileset  *token.FileSet
	astFiles []*ir.File
}

// ParseToAST returns an ir.File without any additional processing,
// like type inference
func ParseToAST(data string) (ir.File, *ilerr.Errors, error) {
	iStream := antlr.NewInputStream(data)
	tStream := antlr.NewCommonTokenStream(parser.NewIleLexer(iStream), antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{
		Logger: slog.With("section", "frontend").With("section", "antlrWalker"),
	}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ir.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.result()
	return f, compileErrors, nil
}
