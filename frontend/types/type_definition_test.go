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

type MapShowCtx map[string]*ast.TypeVar

func (m MapShowCtx) NameOf(t *ast.TypeVar) string {
	return t.Identifier
}

func testType(t *testing.T, expr ast.Expr, expected ast.Type) {
	typeMap := new(MapShowCtx)
	t.Run(fmt.Sprintf("(%s):%s", expr.ExprName(), expected.ShowIn(typeMap)), func(t *testing.T) {
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
		asAst := ctx.GetType(instance)
		finalTypeStr := asAst.ShowIn(typeMap)
		assert.Equal(t, expected.ShowIn(typeMap), finalTypeStr, "unexpected type for `%s`: %s (a %T, %s where %s) (expected %s)", expr.ExprName(), finalTypeStr, asAst, instance, boundsString(instance), expected.ShowIn(typeMap))
	})
}

func TestInferSingleInt(t *testing.T) {
	expr := ast.IntLiteral("1", ast.Range{})

	testType(t, expr, expr)
}

func TestInferIntOps(t *testing.T) {
	expr := &ast.Call{
		Func: &ast.Var{
			Name: "+",
		},
		Args: []ast.Expr{ast.IntLiteral("1", ast.Range{}), ast.IntLiteral("2", ast.Range{})},
	}

	testType(t, expr, ast.IntLiteral("3", ast.Range{}))
}
func TestInferPlusFunc(t *testing.T) {
	expr := &ast.Var{
		Name: "+",
	}

	testType(t, expr, ast.IntLiteral("3", ast.Range{}))
}
