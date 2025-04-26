package backend

import (
	"bytes"
	"github.com/cottand/ile/frontend/ast"
	"github.com/stretchr/testify/assert"
	"github.com/traefik/yaegi/interp"
	goast "go/ast"
	"go/format"
	"go/token"
	"log/slog"
	"reflect"
	"testing"
)

func evalNode(node goast.Node) (reflect.Value, *interp.Interpreter, error) {
	sourceBuf := bytes.NewBuffer(nil)

	// Print the generated Go code
	_ = format.Node(sourceBuf, token.NewFileSet(), node)
	i := interp.New(interp.Options{})
	v, err := i.Eval(sourceBuf.String())
	if err != nil {
		slog.Warn("had errors while evaluating", "err", err.Error(), "body", sourceBuf.String())
	}
	return v, i, err
}

func evalExpr(t *testing.T, ile ast.Expr) reflect.Value {
	f := ast.File{
		PkgName: "main",
		Declarations: []ast.Declaration{
			{
				Name: "Value",
				E:    ile,
			},
		},
	}

	tp := Transpiler{}
	g, err := tp.TranspileFile(f)
	assert.NoError(t, err)

	_, i, err := evalNode(g)
	assert.NoError(t, err)
	return i.Globals()["Value"]
}

func TestVarDecl(t *testing.T) {
	f := ast.File{
		PkgName: "main",
		Declarations: []ast.Declaration{
			{
				Name: "Hello",
				E: &ast.Call{
					Func: ast.BinOp(token.ADD, ast.Range{}),
					Args: []ast.Expr{ast.IntLiteral("1", nil), ast.IntLiteral("2", nil)},
				},
			},
			{
				Name: "Bye",
				E: &ast.Call{
					Func: ast.BinOp(token.ADD, ast.Range{}),
					Args: []ast.Expr{ast.IntLiteral("1", nil), ast.IntLiteral("1", nil)},
				},
			},
		},
	}

	tp := Transpiler{}
	g, err := tp.TranspileFile(f)
	assert.NoError(t, err)
	assert.Equal(t, g.Name.Name, "main")

	_, i, err := evalNode(g)
	assert.NoError(t, err)
	assert.Contains(t, i.Globals(), "Hello")
	assert.Contains(t, i.Globals(), "Bye")
	assert.Equal(t, int64(2), i.Globals()["Bye"].Int())
}

func TestIntDecl(t *testing.T) {
	v := evalExpr(t, ast.IntLiteral("2", nil))

	assert.Equal(t, int64(2), v.Int())
}

func TestStringDecl(t *testing.T) {
	v := evalExpr(t, ast.StringLiteral("aa", nil))

	assert.Equal(t, "aa", v.String())
}

func TestFunctionDecl(t *testing.T) {
	fn := ast.Func{
		ArgNames: []string{"a"},
		Body:     ast.IntLiteral("32", nil),
		Range:    ast.Range{},
		//TAnnotation: ,
	}
	f := ast.File{
		PkgName: "main",
		Declarations: []ast.Declaration{
			{
				Name: "OneInt",
				E: &ast.Ascribe{
					Expr: &fn,
					Type_: &ast.FnType{
						Args:   []ast.Type{ast.IntType},
						Return: ast.IntType,
					},
				},
			},
		},
	}
	tp := Transpiler{}
	g, err := tp.TranspileFile(f)
	assert.NoError(t, err)
	assert.Equal(t, g.Name.Name, "main")

	_, i, err := evalNode(g)
	assert.NoError(t, err)
	i.Symbols("main")
	assert.Contains(t, i.Symbols("main")["main"], "OneInt")
	res, err := i.Eval("OneInt(1)")
	assert.NoError(t, err)
	assert.Equal(t, int64(32), res.Int())
}
