package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"github.com/stretchr/testify/assert"
	"runtime/debug"
	"strings"
	"testing"
)

func testType(t *testing.T, expr ast.Expr, expected SimpleType) {
	t.Run(fmt.Sprintf("(%s):%s", expr.ExprName(), expected.String()), func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
				stack := strings.Split(string(debug.Stack()), "\n")
				t.Errorf("panic: %v\nlikely at %s\n full stack trace follows:\n%s\n", err, stack[10], string(debug.Stack()))
				t.FailNow()
			}
		}()
		ctx := NewEmptyTypeCtx()
		vars := make(map[typeVariableID]SimpleType)

		typeScheme := ctx.TypeLetBody(expr, vars)
		instance := typeScheme.instantiate(ctx.fresher, 0)
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
		Func: &ast.Var{
			Name: "+",
		},
		Args: []ast.Expr{ast.IntLiteral("1", ast.Range{}), ast.IntLiteral("2", ast.Range{})},
	}

	testType(t, expr, classTag{
		id:      ast.IntLiteral("3", ast.Range{}),
		parents: util.NewSetOf(ast.IntBuiltinType, ast.NumberBuiltinType),
	})
}
