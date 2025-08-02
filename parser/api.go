package parser

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"go/token"
	"log/slog"
)

// ParseToAST parses the given source code and returns an AST representation.
// Unlike the original ParseToAST function, this one returns an ast.File instead of an ir.File.
//
// fset is needed to access file information, but it is not mutated
func ParseToAST(data string, fset *token.File) (ast.File, *ilerr.Errors, error) {
	iStream := antlr.NewInputStream(data)
	lexer := NewIleLexer(iStream)
	errListener := errorListener{
		fset: fset,
	}
	lexer.RemoveErrorListeners()
	lexer.AddErrorListener(&errListener)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	antlrParser := NewIleParser(tStream)
	antlrParser.RemoveErrorListeners()
	antlrParser.AddErrorListener(&errListener)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{
		Logger: slog.With("section", "frontend").With("section", "parser2"),
	}

	sourceFile := antlrParser.SourceFile()
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
