package parser

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"log/slog"
)

// ParseToAST parses the given source code and returns an AST representation.
// Unlike the original ParseToAST function, this one returns an ast.File instead of an ir.File.
func ParseToAST(data string) (ast.File, *ilerr.Errors, error) {
	iStream := antlr.NewInputStream(data)
	tStream := antlr.NewCommonTokenStream(NewIleLexer(iStream), antlr.TokenDefaultChannel)
	p := NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{
		Logger: slog.With("section", "frontend").With("section", "parser2"),
	}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ast.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.result()
	return f, compileErrors, nil
}
