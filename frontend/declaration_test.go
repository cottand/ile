package frontend_test

import (
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/stretchr/testify/assert"
	"testing"
)

func testAntlrParse(t *testing.T, input string) (ir.File, *ilerr.Errors) {
	f, cErrs, errs := frontend.ParseToIR(input)
	assert.NoError(t, errs)
	return f, cErrs
}

func TestNoNewlineEndError(t *testing.T) {
	t.Skip("antlr intellij plugin complains but error node not visited ??")
	file := `
package main

val a = 1`

	_, errs := testAntlrParse(t, file)

	assert.NotEmpty(t, errs.Errors())
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

val hello = 1
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ir.Literal{}, fst.E)
	assert.Equal(t, "1", fst.E.(*ir.Literal).Syntax)
}

func TestStrLiteral(t *testing.T) {
	file := `
package main

val hello = "aa"
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ir.Literal{}, fst.E)
	assert.Equal(t, "aa", fst.E.(*ir.Literal).Syntax)
}

func TestListener_ExitFunctionDecl(t *testing.T) {
	file := `
package main

fn hello() { 1 }

`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
}

func TestListener_ExitFunctionDeclParams(t *testing.T) {
	file := `
package main

fn hello(i: Int, ii: Int) { 1 }
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	assert.IsType(t, &ir.Ascribe{}, src.Declarations[0].E)
	as := src.Declarations[0].E.(*ir.Ascribe)
	assert.IsType(t, &ir.Func{}, as.Expr)
	fn := as.Expr.(*ir.Func)

	assert.Len(t, fn.ArgNames, 2)
	assert.Equal(t, "i", fn.ArgNames[0])
	assert.Equal(t, "ii", fn.ArgNames[1])

}

func TestExitFunction_Body(t *testing.T) {
	file := `
package main

fn hello(i: Int, ii: Int) { 1 }
`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	assert.IsType(t, &ir.Ascribe{}, src.Declarations[0].E)
	as := src.Declarations[0].E.(*ir.Ascribe)
	assert.IsType(t, &ir.Func{}, as.Expr)
	fn := as.Expr.(*ir.Func)

	assert.IsType(t, &ir.Literal{}, fn.Body)
	assert.Equal(t, "1", fn.Body.(*ir.Literal).Syntax)
}
