package ir

import (
	"fmt"
	"github.com/cottand/ile/ir/hm"
	"reflect"
	"strings"
)

// InferEnv maps literals to general types, like: isZero: Float -> Bool
var InferEnv = func() hm.SimpleEnv {
	// only lits are looked up here!
	return hm.SimpleEnv{}
}

type TypeInferrable[N Node] interface {
	TypeInfer() (N, []*CompileError)
}

type ExprInferrable[E Expr] interface {
	TypeInfer() (Type, []*CompileError)
}

func (f FuncDecl) TypeInfer() (Type, []*CompileError) {
	scheme, err := hm.Infer(InferEnv(), f.ToTypeExpression(IdentifierLitExpr{
		NameLit: f.NameLit,
	}))
	if err != nil {
		return nil, []*CompileError{{}}
	}
	t, _ := scheme.Type()
	return convertInferred(t, len(f.Params)), nil
}

func (f ValDecl) TypeInfer() (Type, []*CompileError) {
	scheme, err := hm.Infer(InferEnv(), f.E)
	if err != nil {
		if strings.Contains(err.Error(), "Unification Fail:") && strings.Contains(err.Error(), "cannot be unified") {
			trimmed := strings.TrimPrefix(err.Error(), "Unification Fail: ")
			trimmed = strings.TrimSuffix(trimmed, " cannot be unified")
			return nil, []*CompileError{{
				Message: fmt.Sprintf("type mismatch: " + trimmed),
			}}
		}
		return nil, []*CompileError{{Message: err.Error()}}
	}
	t, _ := scheme.Type()
	return convertInferred(t, -1), nil
}

func convertFuncType(t hm.FunctionType, nParams int, paramsFoundSoFar []Type) (params []Type) {
	ts := t.Types()
	t0 := ts[0]
	t1 := ts[1]

	// then our first arg is Nil
	if nParams == 0 {
		return []Type{TypeLit{NameLit: "Nil"}, convertInferred(t1, -1)}
	}
	if nParams == 1 {
		paramsFoundSoFar = append(paramsFoundSoFar, convertInferred(t0, -1), convertInferred(t1, -1))
		return paramsFoundSoFar
	}
	// we have more than 1 param, so the return type will be
	// of type (oneParam) -> DeclaredT
	// So we need to uncurry
	ft, ok := t1.(*hm.FunctionType)
	if !ok {
		panic("unexpected no func type, got " + reflect.TypeOf(t1).String())
	}
	paramsFoundSoFar = append(paramsFoundSoFar, convertInferred(t0, -1))
	return convertFuncType(*ft, nParams-1, paramsFoundSoFar)
}

func convertInferred(t hm.Type, funcTypeArgs int) Type {
	switch t := t.(type) {
	case *hm.FunctionType:
		params := convertFuncType(*t, funcTypeArgs, nil)
		return FuncType{
			Params: params[:1],
			Result: params[len(params)-1],
		}
	case TypeLit:
		return t
		// function generic!
	case hm.TypeVariable:
		return TypeLit{NameLit: "GENERIC_" + t.Name()}
	default:
		panic(fmt.Sprintf("unexpected implementor of Type: %v", reflect.TypeOf(t)))
	}
}
