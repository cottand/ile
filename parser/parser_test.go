package parser_test

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/parser"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func testParse(t *testing.T, input string) (ast.File, *ilerr.Errors) {
	f, cErrs, errs := parser.ParseToAST(input, &token.File{})
	assert.NoError(t, errs)
	return f, cErrs
}

func TestNoPanics(t *testing.T) {
	files := map[string]string{
		"empty program": ``,
		"program with val": `
package main
val
`,
		"program with v": `
package main
v
`,
		"program with v, no newline": `
package main
v`,
	}

	for name, file := range files {
		t.Run(name, func(t *testing.T) {
			assert.NotPanics(t, func() {
				_, _, _ = parser.ParseToAST(file, nil)
			})
		})
	}
}

func TestPackageDirective(t *testing.T) {
	file := `
package main
`
	src, _ := testParse(t, file)

	assert.Equal(t, "main", src.PkgName)
}

func TestDeclLiteral(t *testing.T) {
	file := `
package main

val hello = 1
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ast.Literal{}, fst.Value)
	assert.Equal(t, "1", fst.Value.(*ast.Literal).Value)
	assert.Equal(t, token.INT, fst.Value.(*ast.Literal).Kind)
}

func TestStrLiteral(t *testing.T) {
	file := `
package main

val hello = "aa"
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ast.Literal{}, fst.Value)
	assert.Equal(t, "aa", fst.Value.(*ast.Literal).Value)
	assert.Equal(t, token.STRING, fst.Value.(*ast.Literal).Kind)
}

func TestFunctionDecl(t *testing.T) {
	file := `
package main

fn hello() { 1 }
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ast.FuncLit{}, fst.Value)

	funcLit := fst.Value.(*ast.FuncLit)
	assert.Len(t, funcLit.Params, 0)

	// Check that the function type annotation is correct
	assert.IsType(t, &ast.FuncType{}, fst.TypeAnn)
	funcType := fst.TypeAnn.(*ast.FuncType)
	assert.Len(t, funcType.ParamTypes, 0)
	assert.Nil(t, funcType.ReturnType)

	// Check that the function body is correct
	assert.IsType(t, &ast.BlockExpr{}, funcLit.Body)
	literal := funcLit.Body.(*ast.BlockExpr).Expressions[0].(*ast.Literal)
	assert.Equal(t, "1", literal.Value)
	assert.Equal(t, token.INT, literal.Kind)
}

func TestFunctionDeclParams(t *testing.T) {
	file := `
package main

fn hello(i: Int, ii: Int) { 1 }
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ast.FuncLit{}, fst.Value)

	funcLit := fst.Value.(*ast.FuncLit)
	assert.Len(t, funcLit.Params, 2)
	assert.Equal(t, "i", funcLit.Params[0].Name)
	assert.Equal(t, "ii", funcLit.Params[1].Name)

	// Check that the parameter types are correct
	assert.IsType(t, &ast.TypeName{}, funcLit.Params[0].TypeAnn)
	assert.Equal(t, "Int", funcLit.Params[0].TypeAnn.(*ast.TypeName).Name)
	assert.IsType(t, &ast.TypeName{}, funcLit.Params[1].TypeAnn)
	assert.Equal(t, "Int", funcLit.Params[1].TypeAnn.(*ast.TypeName).Name)

	// Check that the function type annotation is correct
	assert.IsType(t, &ast.FuncType{}, fst.TypeAnn)
	funcType := fst.TypeAnn.(*ast.FuncType)
	assert.Len(t, funcType.ParamTypes, 2)
	assert.IsType(t, &ast.TypeName{}, funcType.ParamTypes[0])
	assert.Equal(t, "Int", funcType.ParamTypes[0].(*ast.TypeName).Name)
	assert.IsType(t, &ast.TypeName{}, funcType.ParamTypes[1])
	assert.Equal(t, "Int", funcType.ParamTypes[1].(*ast.TypeName).Name)
	assert.Nil(t, funcType.ReturnType)

	// Check that the function body is correct
	assert.IsType(t, &ast.BlockExpr{}, funcLit.Body)
	literal := funcLit.Body.(*ast.BlockExpr).Expressions[0].(*ast.Literal)
	assert.Equal(t, "1", literal.Value)
	assert.Equal(t, token.INT, literal.Kind)
}

func TestBinaryExpr(t *testing.T) {
	file := `
package main

val result = 1 + 2
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "result", fst.Name)
	assert.IsType(t, &ast.BinaryExpr{}, fst.Value)

	binExpr := fst.Value.(*ast.BinaryExpr)
	assert.Equal(t, token.ADD, binExpr.Operator)

	assert.IsType(t, &ast.Literal{}, binExpr.Left)
	assert.Equal(t, "1", binExpr.Left.(*ast.Literal).Value)
	assert.Equal(t, token.INT, binExpr.Left.(*ast.Literal).Kind)

	assert.IsType(t, &ast.Literal{}, binExpr.Right)
	assert.Equal(t, "2", binExpr.Right.(*ast.Literal).Value)
	assert.Equal(t, token.INT, binExpr.Right.(*ast.Literal).Kind)
}

func TestListLiteral(t *testing.T) {
	file := `
package main

val list = [1, 2, 3]
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "list", fst.Name)
	assert.IsType(t, &ast.ListLit{}, fst.Value)

	listLit := fst.Value.(*ast.ListLit)
	assert.Len(t, listLit.Elements, 3)

	assert.IsType(t, &ast.Literal{}, listLit.Elements[0])
	assert.Equal(t, "1", listLit.Elements[0].(*ast.Literal).Value)
	assert.Equal(t, token.INT, listLit.Elements[0].(*ast.Literal).Kind)

	assert.IsType(t, &ast.Literal{}, listLit.Elements[1])
	assert.Equal(t, "2", listLit.Elements[1].(*ast.Literal).Value)
	assert.Equal(t, token.INT, listLit.Elements[1].(*ast.Literal).Kind)

	assert.IsType(t, &ast.Literal{}, listLit.Elements[2])
	assert.Equal(t, "3", listLit.Elements[2].(*ast.Literal).Value)
	assert.Equal(t, token.INT, listLit.Elements[2].(*ast.Literal).Kind)
}

func TestBlockWithMultipleStatements(t *testing.T) {
	file := `
package main

fn hello() { 
	val x = 1
	val y = 2
	x + y
}
`
	src, _ := testParse(t, file)

	assert.Len(t, src.Declarations, 1)
	fst := src.Declarations[0]
	assert.Equal(t, "hello", fst.Name)
	assert.IsType(t, &ast.FuncLit{}, fst.Value)

	funcLit := fst.Value.(*ast.FuncLit)

	// Check that the function body is a BlockExpr
	assert.IsType(t, &ast.BlockExpr{}, funcLit.Body)
	blockExpr := funcLit.Body.(*ast.BlockExpr)

	// Check that the BlockExpr has the correct number of expressions
	assert.Len(t, blockExpr.Expressions, 3)

	// Check that the first expression is a VarDecl
	assert.IsType(t, &ast.VarDecl{}, blockExpr.Expressions[0])
	varDecl1 := blockExpr.Expressions[0].(*ast.VarDecl)
	assert.Equal(t, "x", varDecl1.Name)
	assert.IsType(t, &ast.Literal{}, varDecl1.Value)
	assert.Equal(t, "1", varDecl1.Value.(*ast.Literal).Value)

	// Check that the second expression is a VarDecl
	assert.IsType(t, &ast.VarDecl{}, blockExpr.Expressions[1])
	varDecl2 := blockExpr.Expressions[1].(*ast.VarDecl)
	assert.Equal(t, "y", varDecl2.Name)
	assert.IsType(t, &ast.Literal{}, varDecl2.Value)
	assert.Equal(t, "2", varDecl2.Value.(*ast.Literal).Value)

	// Check that the third expression is a BinaryExpr
	assert.IsType(t, &ast.BinaryExpr{}, blockExpr.Expressions[2])
	binExpr := blockExpr.Expressions[2].(*ast.BinaryExpr)
	assert.Equal(t, token.ADD, binExpr.Operator)
	assert.IsType(t, &ast.Identifier{}, binExpr.Left)
	assert.Equal(t, "x", binExpr.Left.(*ast.Identifier).Name)
	assert.IsType(t, &ast.Identifier{}, binExpr.Right)
	assert.Equal(t, "y", binExpr.Right.(*ast.Identifier).Name)

	// Verify that BlockExpr implements both Expr and Stmt interfaces
	var expr ast.Expr = blockExpr
	var stmt ast.Stmt = blockExpr
	assert.NotNil(t, expr)
	assert.NotNil(t, stmt)
}
