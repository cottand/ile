package frontend_test

import (
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/stretchr/testify/assert"
	"testing"
)

func testAntlrParse(t *testing.T, input string) (ast.File, *ilerr.Errors) {
	f, cErrs, errs := frontend.ParseToAST(input)
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
	assert.IsType(t, &ast.Literal{}, fst.E)
	assert.Equal(t, "1", fst.E.(*ast.Literal).Syntax)
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
	assert.IsType(t, &ast.Literal{}, fst.E)
	assert.Equal(t, "aa", fst.E.(*ast.Literal).Syntax)
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
	assert.IsType(t, &ast.Ascribe{}, src.Declarations[0].E)
	as := src.Declarations[0].E.(*ast.Ascribe)
	assert.IsType(t, &ast.Func{}, as.Expr)
	fn := as.Expr.(*ast.Func)

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
	assert.IsType(t, &ast.Ascribe{}, src.Declarations[0].E)
	as := src.Declarations[0].E.(*ast.Ascribe)
	assert.IsType(t, &ast.Func{}, as.Expr)
	fn := as.Expr.(*ast.Func)

	assert.IsType(t, &ast.Literal{}, fn.Body)
	assert.Equal(t, "1", fn.Body.(*ast.Literal).Syntax)
}

func TestExitOperand(t *testing.T) {
	file := `
package main

val a = 1 + a

`
	src, _ := testAntlrParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "a", fst.Name)

	assert.IsType(t, &ast.Call{}, fst.E)
	fn := fst.E.(*ast.Call).Func.(*ast.Literal)

	assert.Equal(t, fn.Syntax, "+")
	assert.IsType(t, fst.E.(*ast.Call).Args[0].(*ast.Literal).Syntax, "1")
}
