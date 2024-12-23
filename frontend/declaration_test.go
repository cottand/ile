package frontend

import (
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/ir"
	"github.com/cottand/ile/parser"
	"github.com/stretchr/testify/assert"
	"testing"
)

func testAntlrParse(input string) (ir.File, []ir.CompileError) {
	iStream := antlr.NewInputStream(input)
	lexer := parser.NewIleLexer(iStream)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{}

	walker.Walk(l, p.SourceFile())

	return l.Result()
}

func TestNoNewlineEndError(t *testing.T) {
	t.Skip("antlr intellij plugin complains but error node not visited ??")
	file := `
package main

a = 1`

	_, errs := testAntlrParse(file)

	assert.NotEmpty(t, errs)
}

func TestPackageDirective(t *testing.T) {
	file := `
package main
`
	src, _ := testAntlrParse(file)

	assert.Equal(t, "main", src.PkgName)
}

func TestDeclLiteral(t *testing.T) {
	file := `
package main

hello = 1
`
	src, _ := testAntlrParse(file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
}
