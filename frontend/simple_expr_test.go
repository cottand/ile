package frontend_test

import (
	"fmt"
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/util"
	"github.com/stretchr/testify/assert"
	"runtime/debug"
	"strings"
	"testing"
)

func testType(t *testing.T, exprStr string, expected ast.Type) {
	t.Run(fmt.Sprintf("(%s):%s", exprStr, expected.ShowIn(ast.DumbShowCtx, 0)), func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
				stack := strings.Split(string(debug.Stack()), "\n")
				t.Errorf("panic: %v\nlikely at %s\n full stack trace follows:\n%s\n", err, stack[10], string(debug.Stack()))
				t.FailNow()
			}
		}()

		fileString := `
package main

val exprTest = ` + exprStr + "\n"
		file, ilerrs, err := frontend.ParseToAST(fileString)
		assert.NoError(t, err)
		assert.Empty(t, ilerrs.Errors(), "expected no errors, but got:\n%s", ilerrs.Errors())
		expr := file.Declarations[0].E
		ctx := types.NewEmptyTypeCtx()
		vars := make(map[types.TypeVarID]types.SimpleType)

		_ = ctx.TypeExpr(expr, vars)
		if len(ctx.Failures) != 0 {
			t.Fatalf("Failures found: %s\n", "\n    "+util.JoinString(ctx.Failures, "\n    "))
		}
		if len(ctx.Errors) != 0 {
			t.Errorf("Errors found:\n")
			for _, err := range ctx.Errors {
				t.Errorf("  %s\n", ilerr.FormatWithCode(err))
			}
			t.FailNow()
		}
		asAst := ctx.TypeOf(expr)
		finalTypeStr := asAst.ShowIn(ast.DumbShowCtx, 0)
		assert.Equal(t, expected.ShowIn(ast.DumbShowCtx, 0), finalTypeStr, "unexpected type for `%s`: %s (a %T) (expected %s)", expr.ExprName(), finalTypeStr, asAst, expected.ShowIn(ast.DumbShowCtx, 0))
		fmt.Printf("INFERRED %s (with hash %x): %s\n", exprStr, expr.Hash(), finalTypeStr)
	})
}

func TestSimpleInts(t *testing.T) {
	testType(t, "1", ast.IntLiteral("1", ast.Range{}))
	testType(t, "1+2", ast.IntType)
	testType(t, "1+2+3", ast.IntType)
}

