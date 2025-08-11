package parser

import (
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"go/token"
)

type errorListener struct {
	*antlr.DefaultErrorListener // Embed default which ensures we fit the interface
	Errors                      []ilerr.IleError
	fset                        *token.File
}

func (e *errorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, ex antlr.RecognitionException) {
	var start, end token.Pos
	if ex == nil || ex.GetOffendingToken() == nil {
		if line > e.fset.LineCount() {
			line = e.fset.LineCount() - 1
		}
		start = e.fset.LineStart(line) + token.Pos(column)
		end = start
	} else {
		start = token.Pos(ex.GetOffendingToken().GetStart())
		end = start
	}
	e.Errors = append(e.Errors, ilerr.New(ilerr.NewSyntax{
		Positioner: ast.Range{
			PosStart: start,
			PosEnd:   end,
		},
		Line:          line,
		Column:        column,
		ParserMessage: msg,
	}))
}
