package frontend_test

import (
	"github.com/cottand/ile/frontend/ast"
	"go/token"
	"testing"
)

func TestInferSimpleFn(t *testing.T) {

	decl := ast.Declaration{
		Name: "f",
		E: &ast.Func{
			ArgNames: []string{"a"},
			Body: &ast.Literal{
				Syntax: "1",
				Kind:   token.INT,
			},
		},
	}



}
