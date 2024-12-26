package ir

import (
	hm "github.com/cottand/ile/ir/hm"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

// env maps literals to general types, like: isZero: Float -> Bool
var env = hm.SimpleEnv{
	"PrimOp:+": hm.NewScheme(hm.TypeVarSet{'a'}, hm.NewFnType(hm.TypeVariable('a'), hm.TypeVariable('a'), hm.TypeVariable('a'))),
	"PrimOp:-": hm.NewScheme(hm.TypeVarSet{'a'}, hm.NewFnType(hm.TypeVariable('a'), hm.TypeVariable('a'), hm.TypeVariable('a'))),
	"PrimOp:>": hm.NewScheme(hm.TypeVarSet{'a'}, hm.NewFnType(hm.TypeVariable('a'), hm.TypeVariable('a'), TypeLit{NameLit: "Bool"})),
}

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

	scheme, err := hm.Infer(env, ast)
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

	scheme, err := hm.Infer(env, ast)
	if err != nil {
		t.Fatal(err)
	}
	resT, isMono := scheme.Type()
	assert.Equal(t, TypeLit{NameLit: "Bool"}, resT)
	assert.True(t, isMono)
}
