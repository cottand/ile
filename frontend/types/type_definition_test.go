package types_test

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/util"
	"github.com/stretchr/testify/assert"
	"runtime/debug"
	"strings"
	"testing"
)

type MapShowCtx map[string]*ir.TypeVar

func (m MapShowCtx) NameOf(t *ir.TypeVar) string {
	return t.Identifier
}

func testType(t *testing.T, expr ir.Expr, expected ir.Type) {
	typeMap := new(MapShowCtx)
	exprString := ir.ExprString(expr)
	t.Run(fmt.Sprintf("(%s):%s", exprString, expected.ShowIn(typeMap, 0)), func(t *testing.T) {
		ctx := types.NewEmptyTypeCtx()
		defer func() {
			if err := recover(); err != nil {
				if len(ctx.Failures) != 0 {
					t.Errorf("following failures were present in type context: \n  %s", util.JoinErrorsWith("", ctx.Failures, "\n "))
				}
				stack := strings.Split(string(debug.Stack()), "\n")
				t.Errorf("panic: %v\nlikely at %s\n full stack trace follows:\n%s\n", err, stack[10], string(debug.Stack()))
				t.FailNow()
			}
		}()
		vars := make(map[string]types.SimpleType)

		_ = ctx.TypeExpr(expr, vars)
		if len(ctx.Failures) != 0 {
			t.Fatalf("Failures found:\n  %s\n", util.JoinErrorsWith("", ctx.Failures, "\n  "))
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
	expr := ir.IntLiteral("1", ir.Range{})

	testType(t, expr, expr)
}

func TestInferIntOps(t *testing.T) {
	onePlusTwo := &ir.Call{
		Func: &ir.Var{
			Name: "+",
		},
		Args: []ir.Expr{ir.IntLiteral("1", ir.Range{}), ir.IntLiteral("2", ir.Range{})},
	}

	testType(t, onePlusTwo, ir.IntType)

	plusThree := &ir.Call{
		Func: &ir.Var{
			Name: "+",
		},
		Args: []ir.Expr{
			ir.IntLiteral("3", ir.Range{}),
			onePlusTwo,
		},
	}
	testType(t, plusThree, ir.IntType)
}

func TestInferPlusFunc(t *testing.T) {
	expr := &ir.Var{
		Name: "+",
	}
	testType(t, expr, &ir.FnType{
		Args: []ir.Type{
			ir.IntType,
			ir.IntType,
		},
		Return: ir.IntType,
	})
}

func TestAssign(t *testing.T) {
	expr := &ir.Assign{
		// x = 1
		// x + 1
		Var:   "x",
		Value: ir.IntLiteral("1", ir.Range{}),
		Body: &ir.Call{
			Func:  &ir.Var{Name: "+"},
			Args:  []ir.Expr{ir.IntLiteral("10", ir.Range{}), &ir.Var{Name: "x"}},
			Range: ir.Range{},
		},
	}
	testType(t, expr, ir.IntType)
}

func TestIdentityFunc(t *testing.T) {
	t.Skip("stable strings for type variables not implemented - this will break until we are consistent with that")
	expr := &ir.Assign{
		// fn f(x) = x
		// f
		Var: "f",
		Value: &ir.Func{
			ArgNames: []string{"x"},
			Body:     &ir.Var{Name: "x"},
			Range:    ir.Range{},
		},
		Body:      &ir.Var{Name: "f"},
		Recursive: false,
	}

	testType(t, expr, &ir.FnType{
		Args:   []ir.Type{&ir.TypeVar{Identifier: "α10"}},
		Return: &ir.TypeVar{Identifier: "α10"},
		Range:  ir.Range{},
	})
}

func TestRecFunc(t *testing.T) {
	expr := &ir.Assign{
		// fn f(x) = f(x)
		// f
		Var: "f",
		Value: &ir.Func{
			ArgNames: []string{"x"},
			Body: &ir.Call{
				Func: &ir.Var{Name: "f"},
				Args: []ir.Expr{
					&ir.Var{Name: "x"},
				},
			},
			Range: ir.Range{},
		},
		Body:      &ir.Var{Name: "f"},
		Recursive: true,
	}
	testType(t, expr, &ir.FnType{
		Args:   []ir.Type{&ir.AnyType{}},
		Return: &ir.NothingType{},
		Range:  ir.Range{},
	})
}

func TestLetGroup(t *testing.T) {
	expr := &ir.LetGroup{
		// v = 1
		// fn f(x) = x + v
		// in: f
		Vars: []ir.LetBinding{
			{
				// v = 1
				Var:   "v",
				Value: ir.IntLiteral("1", nil),
			},
			{
				// fn f(x) = x + v
				Var: "f",
				Value: &ir.Func{
					ArgNames: []string{"x"},
					Body: &ir.Call{
						Func: &ir.Var{Name: "+"},
						Args: []ir.Expr{
							&ir.Var{Name: "x"},
							&ir.Var{Name: "v"},
						},
					},
				},
			},
		},
		Body: &ir.Var{Name: "f"},
	}

	testType(t, expr, &ir.FnType{Args: []ir.Type{ir.IntType}, Return: ir.IntType})
}

func TestMutuallyRecursive(t *testing.T) {
	expr := &ir.LetGroup{
		Vars: []ir.LetBinding{
			{
				// fn rec1(x) = x + rec2(x)
				Var: "rec1",
				Value: &ir.Func{
					ArgNames: []string{"x"},
					Body: &ir.Call{
						Func: &ir.Var{Name: "+"},
						Args: []ir.Expr{
							&ir.Call{
								Func: &ir.Var{Name: "rec2"},
								Args: []ir.Expr{&ir.Var{Name: "x"}},
							},
							&ir.Var{Name: "x"},
						},
					},
				},
			},
			{
				// fn rec2(x) = rec1(x + x)
				Var: "rec2",
				Value: &ir.Func{
					ArgNames: []string{"x"},
					Body: &ir.Call{
						Func: &ir.Var{Name: "rec1"},
						Args: []ir.Expr{
							&ir.Call{Func: &ir.Var{Name: "+"},
								Args: []ir.Expr{
									&ir.Var{Name: "x"},
									&ir.Var{Name: "x"},
								}},
						},
					},
				},
			},
		},
		Body: &ir.Call{
			Func: &ir.Var{Name: "rec2"},
			Args: []ir.Expr{ir.IntLiteral("2", ir.Range{})},
		},
	}

	testType(t, expr, ir.IntType)
}

func TestSimpleBoolFunc(t *testing.T) {
	t.Skip("simple bool functions rely on type var printing being stable, which is not yet implemented")

	// x = 32 > (1 + 2)
	// x
	expr := &ir.Assign{
		Var: "x",
		Value: &ir.Call{
			Func: &ir.Var{Name: ">"},
			Args: []ir.Expr{
				ir.IntLiteral("32", ir.Range{}),
				&ir.Call{
					Func: &ir.Var{Name: "+"},
					Args: []ir.Expr{
						ir.IntLiteral("1", ir.Range{}),
						ir.IntLiteral("2", ir.Range{}),
					},
				},
			},
		},
		Body: &ir.Var{Name: "x"},
	}
	testType(t, &ir.Var{Name: ">"}, &ir.FnType{Args: []ir.Type{ir.IntType, ir.IntType}, Return: ir.BoolType})
	testType(t, expr, ir.BoolType)
}
