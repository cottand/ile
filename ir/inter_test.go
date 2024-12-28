package ir

import (
	hm "github.com/cottand/ile/ir/hm"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

// InferEnv maps literals to general types, like: isZero: Float -> Bool
var env = InferEnv

func TestInferBinaryOpExpr(t *testing.T) {
	ast := BinaryOpExpr{
		Op: PrimOp(token.ADD),
		Lhs: BinaryOpExpr{
			Op:  PrimOp(token.SUB),
			Lhs: BasicLitExpr{Kind: token.INT, Value: "2"},
			Rhs: BasicLitExpr{Kind: token.INT, Value: "1"},
		},
		Rhs: BasicLitExpr{Kind: token.INT, Value: "4"},
	}

	scheme, err := hm.Infer(env(), ast)
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, TypeLit{NameLit: "Int"}, resT)
	assert.True(t, isMono)
}

func TestInferBinaryBoolOpExpr(t *testing.T) {
	ast := BinaryOpExpr{
		Op: PrimOp(token.GTR),
		Lhs: BinaryOpExpr{
			Op:  PrimOp(token.SUB),
			Lhs: BasicLitExpr{Kind: token.INT, Value: "2"},
			Rhs: BasicLitExpr{Kind: token.INT, Value: "1"},
		},
		Rhs: BasicLitExpr{Kind: token.INT, Value: "4"},
	}

	scheme, err := hm.Infer(env(), ast)
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, TypeLit{NameLit: "Bool"}, resT)
	assert.True(t, isMono)
}

func TestFuncTypeSimple(t *testing.T) {
	ast := PrimOp(token.ADD)

	scheme, err := hm.Infer(env(), ast)
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "a → a → a", resT.String())
	assert.False(t, isMono)
}

func TestFuncSimpleNullary(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params:  nil,
		BodyLit: BasicLitExpr{Kind: token.INT, Value: "2"},
		Result:  nil,
	}

	scheme, err := hm.Infer(env(), ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Nil → Int", resT.String())
	assert.True(t, isMono)
}

func TestFuncTypeOneArg(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{{
			Name: Ident{Name: "myInt"},
			T:    TypeLit{NameLit: "Int"},
		}},
		BodyLit: BinaryOpExpr{
			Op:  PrimOp(token.SUB),
			Lhs: BasicLitExpr{Kind: token.INT, Value: "2"},
			Rhs: IdentifierLitExpr{NameLit: "myInt"},
		},
		Result: nil,
	}

	scheme, err := hm.Infer(env(), ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Int → Int", resT.String())
	assert.True(t, isMono)
}

func TestFuncTypeOneArgTyped(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{{
			Name: Ident{Name: "myInt"},
			T:    TypeLit{NameLit: "Int"},
		}},
		BodyLit: IdentifierLitExpr{NameLit: "myInt"},
		Result:  nil,
	}

	env := env().MergeWith(ast.AdditionalEnv())
	scheme, err := hm.Infer(env, ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Int → Int", resT.String())
	assert.True(t, isMono)
}

func TestFuncTypeOneArgRetTyped(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myInt"},
				T:    TypeLit{NameLit: "Int"},
			},
		},
		BodyLit: BinaryOpExpr{
			Op:  PrimOp(token.ADD),
			Lhs: IdentifierLitExpr{NameLit: "myInt"},
			Rhs: IdentifierLitExpr{NameLit: "myInt"},
		},
	}

	scheme, err := hm.Infer(env(), ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Int → Int", resT.String())
	assert.True(t, isMono)
}

func TestFuncTypeTwoArgRetTyped(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myInt"},
				T:    TypeLit{NameLit: "Int"},
			},
			{
				Name: Ident{Name: "myInt2"},
				T:    TypeLit{NameLit: "Int"},
			},
		},
		BodyLit: BinaryOpExpr{
			Op:  PrimOp(token.ADD),
			Lhs: IdentifierLitExpr{NameLit: "myInt"},
			Rhs: IdentifierLitExpr{NameLit: "myInt2"},
		},
	}

	scheme, err := hm.Infer(env(), ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Int → Int → Int", resT.String())
	assert.True(t, isMono)
}

func TestTryVanillaUnaryOp(t *testing.T) {
	arg := IdentifierLitExpr{
		NameLit: "x",
	}
	ast :=
		letrec{
			name: "fn",
			def: lambdaBodyArg{
				paramName: Ident{Name: "x"},
				body: unaryApplication{
					f: unaryApplication{
						f:   PrimOp(token.ADD),
						arg: arg,
					},
					arg: arg,
				},
				paramScheme: hm.NewScheme(hm.TypeVarSet{}, TypeLit{NameLit: "Int"}),
			},
			in: IdentifierLitExpr{NameLit: "fn"},
		}
	scheme, err := hm.Infer(env(), ast)
	assert.NoError(t, err)
	ty, _ := scheme.Type()
	assert.Equal(t, ty.String(), "Int → Int")
}

func TestFuncTypeTwoArgRetTyped2(t *testing.T) {
	ast := FuncDecl{
		NameLit: "OneInt",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myInt"},
				T:    TypeLit{NameLit: "Int"},
			},
			{
				Name: Ident{Name: "myBool"},
				T:    TypeLit{NameLit: "Bool"},
			},
		},
		BodyLit: BinaryOpExpr{
			Op:  PrimOp(token.ADD),
			Lhs: IdentifierLitExpr{NameLit: "myInt"},
			Rhs: IdentifierLitExpr{NameLit: "myInt"},
		},
	}

	scheme, err := hm.Infer(env(), ast.ToTypeExpression(IdentifierLitExpr{NameLit: "OneInt"}))
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, "Int → Bool → Int", resT.String())
	assert.True(t, isMono)
}

func TestFnParamInfer(t *testing.T) {
	ast := FuncDecl{
		NameLit: "MakeFn",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myIntA"},
				T:    TypeLit{NameLit: "Int"},
			},
			{
				Name: Ident{Name: "myFn"},
				T:    NewFuncType(nil, TypeLit{NameLit: "Bool"}),
			},
		},
		BodyLit: IdentifierLitExpr{NameLit: "myIntA"},
		Result:  nil,
	}

	expression := ast.ToTypeExpression(IdentifierLitExpr{NameLit: "MakeFn"})
	scheme, err := hm.Infer(env(), expression)
	assert.NoError(t, err)
	ty, _ := scheme.Type()
	assert.Equal(t, "Int → (Nil → Bool) → Int", ty.String())
}

func TestFnParamInferFirstParam(t *testing.T) {
	ast := FuncDecl{
		NameLit: "MakeFn",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myFn"},
				T:    NewFuncType(nil, TypeLit{NameLit: "Bool"}),
			},
			{
				Name: Ident{Name: "myIntA"},
				T:    TypeLit{NameLit: "Int"},
			},
		},
		BodyLit: IdentifierLitExpr{NameLit: "myIntA"},
		Result:  nil,
	}

	expression := ast.ToTypeExpression(IdentifierLitExpr{NameLit: "MakeFn"})
	scheme, err := hm.Infer(env(), expression)
	assert.NoError(t, err)
	ty, _ := scheme.Type()
	assert.Equal(t, "(Nil → Bool) → Int → Int", ty.String())
}

func TestFnParamInferRetType(t *testing.T) {
	ast := FuncDecl{
		NameLit: "MakeFn",
		Params: []ParamDecl{
			{
				Name: Ident{Name: "myFn"},
				T:    NewFuncType(nil, TypeLit{NameLit: "Bool"}),
			},
			{
				Name: Ident{Name: "myIntA"},
				T:    TypeLit{NameLit: "Int"},
			},
		},
		BodyLit: IdentifierLitExpr{NameLit: "myFn"},
		Result:  nil,
	}

	expression := ast.ToTypeExpression(IdentifierLitExpr{NameLit: "MakeFn"})
	scheme, err := hm.Infer(env(), expression)
	assert.NoError(t, err)
	ty, _ := scheme.Type()
	assert.Equal(t, "(Nil → Bool) → Int → Nil → Bool", ty.String())
}
