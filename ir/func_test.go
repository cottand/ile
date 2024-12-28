package ir

import (
	"fmt"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func TestFuncDecl_TypeInfer(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{{
			Name: Ident{Name: "myInt"},
			T:    TypeLit{NameLit: "Int"},
		}},
		BodyLit: IdentifierLitExpr{NameLit: "myInt"},
		Result:  nil,
	}

	inferred, errs := ast.TypeInfer()
	assert.Empty(t, errs)

	assert.IsType(t, FuncType{}, inferred)
	asFuncType := inferred.(FuncType)
	assert.Len(t, asFuncType.Params, 1)
	assert.Equal(t, TypeLit{NameLit: "Int"}, asFuncType.Params[0])
	assert.Equal(t, TypeLit{NameLit: "Int"}, asFuncType.Result)
}

func TestFuncDecl_TypeInfer2Args(t *testing.T) {

	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myIntA"},
				T:    TypeLit{NameLit: "Int"},
			}, {
				Name: Ident{Name: "myIntB"},
				T:    TypeLit{NameLit: "Int"},
			}},
		BodyLit: BinaryOpExpr{
			Op:  PrimOp(token.ADD),
			Lhs: IdentifierLitExpr{NameLit: "myIntA"},
			Rhs: IdentifierLitExpr{NameLit: "myIntB"},
		},
		Result: nil,
	}

	inferred, errs := ast.TypeInfer()
	assert.Empty(t, errs)

	assert.IsType(t, FuncType{}, inferred)
	asFuncType := inferred.(FuncType)
	assert.Len(t, asFuncType.Params, 1)
	assert.Equal(t, TypeLit{NameLit: "Int"}, asFuncType.Result)
}

func TestFuncDecl_TypeInferFuncArg(t *testing.T) {

	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myIntA"},
				T:    TypeLit{NameLit: "Int"},
			}, {
				Name: Ident{Name: "myFn"},
				T:    NewFuncType(nil, TypeLit{NameLit: "Bool"}),
			}},
		BodyLit: IdentifierLitExpr{NameLit: "myFn"},
		Result:  nil,
	}

	inferred, errs := ast.TypeInfer()
	assert.Empty(t, errs)

	assert.IsType(t, FuncType{}, inferred)
	asFuncType := inferred.(FuncType)
	fmt.Println(asFuncType.String())
	//assert.Len(t, asFuncType.Params, 1)
	//assert.Equal(t, TypeLit{NameLit: "Int"}, asFuncType.Result)
}
