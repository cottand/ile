package types_test

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/types"
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
	exprString := ast.ExprString(expr)
	t.Run(fmt.Sprintf("(%s):%s", exprString, expected.ShowIn(typeMap, 0)), func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
				stack := strings.Split(string(debug.Stack()), "\n")
				t.Errorf("panic: %v\nlikely at %s\n full stack trace follows:\n%s\n", err, stack[10], string(debug.Stack()))
				t.FailNow()
			}
		}()
		ctx := types.NewEmptyTypeCtx()
		vars := make(map[string]types.SimpleType)

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
		finalTypeStr := asAst.ShowIn(typeMap, 0)
		assert.Equal(t, expected.ShowIn(typeMap, 0), finalTypeStr, "unexpected type for `%s`: %s (a %T) (expected %s)", expr.ExprName(), finalTypeStr, asAst, expected.ShowIn(typeMap, 0))
		fmt.Printf("INFERRED %s (with hash %x): %s\n", exprString, expr.Hash(), finalTypeStr)
	})
}

func TestInferSingleInt(t *testing.T) {
	expr := ast.IntLiteral("1", ast.Range{})

	testType(t, expr, expr)
}

func TestInferIntOps(t *testing.T) {
	onePlusTwo := &ast.Call{
		Func: &ast.Var{
			Name: "+",
		},
		Args: []ast.Expr{ast.IntLiteral("1", ast.Range{}), ast.IntLiteral("2", ast.Range{})},
	}

	testType(t, onePlusTwo, ast.IntType)

	plusThree := &ast.Call{
		Func: &ast.Var{
			Name: "+",
		},
		Args: []ast.Expr{
			ast.IntLiteral("3", ast.Range{}),
			onePlusTwo,
		},
	}
	testType(t, plusThree, ast.IntType)
}

func TestInferPlusFunc(t *testing.T) {
	expr := &ast.Var{
		Name: "+",
	}
	testType(t, expr, &ast.FnType{
		Args: []ast.Type{
			ast.IntType,
			ast.IntType,
		},
		Return: ast.IntType,
	})
}

func TestAssign(t *testing.T) {
	expr := &ast.Assign{
		// x = 1
		// x + 1
		Var:   "x",
		Value: ast.IntLiteral("1", ast.Range{}),
		Body: &ast.Call{
			Func:  &ast.Var{Name: "+"},
			Args:  []ast.Expr{ast.IntLiteral("10", ast.Range{}), &ast.Var{Name: "x"}},
			Range: ast.Range{},
		},
	}
	testType(t, expr, ast.IntType)
}

func TestIdentityFunc(t *testing.T) {
	expr := &ast.Assign{
		// fn f(x) = x
		// f
		Var: "f",
		Value: &ast.Func{
			ArgNames: []string{"x"},
			Body:     &ast.Var{Name: "x"},
			Range:    ast.Range{},
		},
		Body:      &ast.Var{Name: "f"},
		Recursive: false,
	}

	testType(t, expr, &ast.FnType{
		Args:   []ast.Type{&ast.TypeVar{Identifier: "α2"}},
		Return: &ast.TypeVar{Identifier: "α2"},
		Range:  ast.Range{},
	})
}

func TestRecFunc(t *testing.T) {
	expr := &ast.Assign{
		// fn f(x) = f(x)
		// f
		Var: "f",
		Value: &ast.Func{
			ArgNames: []string{"x"},
			Body: &ast.Call{
				Func: &ast.Var{Name: "f"},
				Args: []ast.Expr{
					&ast.Var{Name: "x"},
				},
			},
			Range: ast.Range{},
		},
		Body:      &ast.Var{Name: "f"},
		Recursive: true,
	}
	testType(t, expr, &ast.FnType{
		Args:   []ast.Type{&ast.AnyType{}},
		Return: &ast.NothingType{},
		Range:  ast.Range{},
	})
}

func TestLetGroup(t *testing.T) {
	expr := &ast.LetGroup{
		// v = 1
		// fn f(x) = x + v
		// in: f
		Vars: []ast.LetBinding{
			{
				// v = 1
				Var:   "v",
				Value: ast.IntLiteral("1", nil),
			},
			{
				// fn f(x) = x + v
				Var: "f",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: &ast.Var{Name: "+"},
						Args: []ast.Expr{
							&ast.Var{Name: "x"},
							&ast.Var{Name: "v"},
						},
					},
				},
			},
		},
		Body: &ast.Var{Name: "f"},
	}

	testType(t, expr, &ast.FnType{Args: []ast.Type{ast.IntType}, Return: ast.IntType})
}

func TestMutuallyRecursive(t *testing.T) {
	expr := &ast.LetGroup{
		Vars: []ast.LetBinding{
			{
				// fn rec1(x) = x + rec2(x)
				Var: "rec1",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: &ast.Var{Name: "+"},
						Args: []ast.Expr{
							&ast.Call{
								Func: &ast.Var{Name: "rec2"},
								Args: []ast.Expr{&ast.Var{Name: "x"}},
							},
							&ast.Var{Name: "x"},
						},
					},
				},
			},
			{
				// fn rec2(x) = rec1(x + x)
				Var: "rec2",
				Value: &ast.Func{
					ArgNames: []string{"x"},
					Body: &ast.Call{
						Func: &ast.Var{Name: "rec1"},
						Args: []ast.Expr{
							&ast.Call{Func: &ast.Var{Name: "+"},
								Args: []ast.Expr{
									&ast.Var{Name: "x"},
									&ast.Var{Name: "x"},
								}},
						},
					},
				},
			},
		},
		Body: &ast.Call{
			Func: &ast.Var{Name: "rec2"},
			Args: []ast.Expr{ast.IntLiteral("2", ast.Range{})},
		},
	}

	testType(t, expr, ast.IntType)
}

func TestSimpleBoolFunc(t *testing.T) {
	// x = 32 > (1 + 2)
	// x
	expr := &ast.Assign{
		Var: "x",
		Value: &ast.Call{
			Func: &ast.Var{Name: ">"},
			Args: []ast.Expr{
				ast.IntLiteral("32", ast.Range{}),
				&ast.Call{
					Func: &ast.Var{Name: "+"},
					Args: []ast.Expr{
						ast.IntLiteral("1", ast.Range{}),
						ast.IntLiteral("2", ast.Range{}),
					},
				},
			},
		},
		Body: &ast.Var{Name: "x"},
	}
	testType(t, &ast.Var{Name: ">"}, &ast.FnType{Args: []ast.Type{ast.IntType, ast.IntType}, Return: ast.BoolType})
	testType(t, expr, ast.BoolType)
}
