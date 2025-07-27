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
}

func (e *errorListener) SyntaxError(recognizer antlr.Recognizer, offendingSymbol interface{}, line, column int, msg string, ex antlr.RecognitionException) {
	e.Errors = append(e.Errors, ilerr.New(ilerr.NewSyntax{
		Positioner: ast.Range{
			PosStart: token.Pos(ex.GetOffendingToken().GetStart()),
			PosEnd:   token.Pos(ex.GetOffendingToken().GetStop()),
		},
		ParserMessage: msg,
	}))
}
