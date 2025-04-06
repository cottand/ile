package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"github.com/stretchr/testify/assert"
	"go/token"
	"runtime/debug"
	"testing"
)

func testType(t *testing.T, expr ast.Expr, expected simpleType) {
	t.Run(fmt.Sprintf("(%s):%s", expr.ExprName(), expected.String()), func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
				t.Errorf("panic: %v\n", err)
				debug.PrintStack()
				t.FailNow()
			}
		}()
		ctx := NewEmptyTypeCtx()
		vars := make(map[typeVariableID]simpleType)

		typeScheme := ctx.TypeLetBody(expr, vars)
		instance := typeScheme.instantiate(0)
		if len(ctx.failures) != 0 {
			t.Fatalf("failures found: %s\n", "\n    "+util.JoinString(ctx.failures, "\n    "))
		}
		if len(ctx.errors) != 0 {
			t.Fatalf("errors found: %v\n", ctx.errors)
		}
		assert.True(t, expected.Equivalent(instance), "unexpected type for `%s`: %s (expected %s)", expr.ExprName(), instance, expected)
	})
}

func TestInferSingleInt(t *testing.T) {
	expr := ast.IntLiteral("1", nil)

	testType(t, expr, classTag{
		id:             ast.IntLiteral("1", nil),
		parents:        util.NewSetOf(ast.IntBuiltinType, ast.NumberBuiltinType),
		withProvenance: withProvenance{},
	})
	testType(t, ast.IntLiteral("2", nil), classTag{
		id:      ast.IntLiteral("2", nil),
		parents: util.NewSetOf(ast.IntBuiltinType, ast.NumberBuiltinType),
	})
}

func TestInferIntOps(t *testing.T) {
	expr := &ast.Call{
		Func: ast.BinOp(token.ADD, nil),
		Args: []ast.Expr{ast.IntLiteral("1", nil), ast.IntLiteral("2", nil)},
	}

	testType(t, expr, classTag{
		id:      ast.IntLiteral("3", nil),
		parents: util.NewSetOf(ast.IntBuiltinType, ast.NumberBuiltinType),
	})
}
