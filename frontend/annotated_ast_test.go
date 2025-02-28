package frontend_test

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/construct"
	"github.com/cottand/ile/frontend/hmtypes"
	"github.com/cottand/ile/frontend/infer"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func TestAnnotatedFunc(t *testing.T) {
	op := ast.BinOp(token.ADD, ast.Range{})
	expr := &ast.Func{
		ArgNames: []string{"a"},
		Body: &ast.Call{
			Func: op,
			Args: []ast.Expr{
				&ast.Var{
					Name: "a",
				},
				&ast.Var{
					Name: "a",
				},
			},
			Range: ast.Range{},
		},
	}

	expr.SetType(construct.TArrow1(&hmtypes.Const{Name: "Int"}, &hmtypes.Const{Name: "Int"}))

	env := infer.NewTypeEnv(nil)
	ctx := infer.NewContext()
	expr2, err := ctx.Annotate(expr, env)
	assert.NoError(t, err)
	fmt.Println(hmtypes.TypeString(expr2.Type()))
}
