package frontend

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/ir"
	"github.com/cottand/ile/parser"
)

// ParseToAST returns an ir.File without any additional processing,
// like type inference
// See ParseToIR
func ParseToAST(input string) (ir.File, []*ir.CompileError, error) {
	iStream := antlr.NewInputStream(input)
	lexer := parser.NewIleLexer(iStream)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{}

	walker.Walk(l, p.SourceFile())

	if len(l.visitErrors) != 0 {
		return ir.File{}, nil, errors.Join(l.visitErrors...)
	}

	f, compileErrors := l.Result()
	return f, compileErrors, nil
}

// ParseToIR is like ParseToAST but does any additional processing needed
// to produce valid and correct Go code
func ParseToIR(input string) (ir.File, []*ir.CompileError, error) {
	file, compileErrors, err := ParseToAST(input)
	if err != nil {
		return ir.File{}, compileErrors, err
	}
	withInference, moreCompileErrors := InferencePhase(file)
	moreCompileErrors = append(compileErrors, moreCompileErrors...)
	return withInference, moreCompileErrors, nil
}
