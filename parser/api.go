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
	lexer := NewIleLexer(iStream)
	errListener := errorListener{}
	lexer.AddErrorListener(&errListener)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()
	sourceFile := p.SourceFile()

	l := &listener{
		Logger: slog.With("section", "frontend").With("section", "parser2"),
	}

	if len(errListener.Errors) != 0 {
		var err ilerr.Errors
		return ast.File{}, err.With(errListener.Errors...), nil
	}

	walker.Walk(l, sourceFile)

	// new errors might have arisen while walking
	if len(errListener.Errors) != 0 {
		var err ilerr.Errors
		return ast.File{}, err.With(errListener.Errors...), nil
	}

	// if there were no errListener errs, but there were visitErrors, it means we got
	// unexpected output on a well-formed AST
	if len(l.visitErrors) != 0 {
		return ast.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.result()
	return f, compileErrors, nil
}
