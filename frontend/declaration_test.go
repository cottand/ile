package frontend

import (
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/ir"
	"github.com/cottand/ile/parser"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func testAntlrParse(t *testing.T, input string) (ir.File, []ir.CompileError) {
	iStream := antlr.NewInputStream(input)
	lexer := parser.NewIleLexer(iStream)
	tStream := antlr.NewCommonTokenStream(lexer, antlr.TokenDefaultChannel)
	p := parser.NewIleParser(tStream)

	walker := antlr.NewIterativeParseTreeWalker()

	l := &listener{}

	walker.Walk(l, p.SourceFile())

	assert.NoError(t, l.VisitErrors())

	return l.Result()
}

func TestNoNewlineEndError(t *testing.T) {
	t.Skip("antlr intellij plugin complains but error node not visited ??")
	file := `
package main

a = 1`

	_, errs := testAntlrParse(t, file)

	assert.NotEmpty(t, errs)
}

func TestPackageDirective(t *testing.T) {
	file := `
package main
`
	src, _ := testAntlrParse(t, file)

	assert.Equal(t, "main", src.PkgName)
}

func TestDeclLiteral(t *testing.T) {
	file := `
package main

hello = 1
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ir.BasicLit{}, fst.E)
	assert.Equal(t, "1", fst.E.(*ir.BasicLit).Value)
	assert.Equal(t, token.INT, fst.E.(*ir.BasicLit).Kind)
}

func TestStrLiteral(t *testing.T) {
	file := `
package main

hello = "aa"
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ir.BasicLit{}, fst.E)
	assert.Equal(t, token.STRING, fst.E.(*ir.BasicLit).Kind)
	assert.Equal(t, "aa", fst.E.(*ir.BasicLit).Value)
}
