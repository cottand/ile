package frontend

import (
	"github.com/cottand/ile/ir"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func testAntlrParse(t *testing.T, input string) (ir.File, []*ir.CompileError) {
	f, cErrs, errs := ParseToAST(input)
	assert.NoError(t, errs)
	return f, cErrs
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

	assert.Len(t, src.Values, 1)
	fst := src.Values[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, ir.BasicLitExpr{}, fst.E)
	assert.Equal(t, "1", fst.E.(ir.BasicLitExpr).Value)
	assert.Equal(t, token.INT, fst.E.(ir.BasicLitExpr).Kind)
}

func TestStrLiteral(t *testing.T) {
	file := `
package main

hello = "aa"
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Values, 1)
	fst := src.Values[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, ir.BasicLitExpr{}, fst.E)
	assert.Equal(t, token.STRING, fst.E.(ir.BasicLitExpr).Kind)
	assert.Equal(t, "aa", fst.E.(ir.BasicLitExpr).Value)
}

func TestListener_ExitFunctionDecl(t *testing.T) {
	file := `
package main

fn hello() { 1 }

`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Functions, 1)
	fst := src.Functions[0]
	assert.Equal(t, "hello", fst.NameLit)
}

func TestListener_ExitFunctionDeclParams(t *testing.T) {
	file := `
package main

fn hello(i Int, ii Int) { 1 }
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Functions, 1)
	fn := src.Functions[0]
	assert.Equal(t, "hello", fn.NameLit)

	assert.Len(t, fn.Params, 2)
	assert.Equal(t, "i", fn.Params[0].Name.Name)
	assert.Equal(t, "ii", fn.Params[1].Name.Name)

	assert.Equal(t, "Int", fn.Params[0].T.(ir.TypeLit).NameLit)
}

func TestExitFunction_Body(t *testing.T) {
	file := `
package main

fn hello(i Int, ii Int) { 1 }
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Functions, 1)
	fn := src.Functions[0]

	assert.IsType(t, ir.BasicLitExpr{}, fn.BodyLit)
	assert.Equal(t, "1", fn.BodyLit.(ir.BasicLitExpr).Value)
	assert.Equal(t, token.INT, fn.BodyLit.(ir.BasicLitExpr).Kind)
}
