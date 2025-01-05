package ir

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/ir/hm"
	hmtypes "github.com/cottand/ile/ir/hm/types"
	"go/token"
)

type PrimOp token.Token

func (p PrimOp) Token() token.Token { return token.Token(p) }

func (p PrimOp) IsLambda() bool { return true }

func (p PrimOp) IsLit() bool         { return true }
func (p PrimOp) Body() hm.Expression { return p }
func (p PrimOp) Name() string {
	return "PrimOp:" + token.Token(p).String()
}

func (p PrimOp) Infer(_ hm.Env, f hm.Fresher) (hm.Type, error) {
	switch p.Token() {
	case token.NOT:
		return hm.NewFnType(TypeLit{NameLit: "Bool"}, TypeLit{NameLit: "Bool"}), nil

	case token.AND, token.OR:
		return hm.NewFnType(TypeLit{NameLit: "Bool"}, TypeLit{NameLit: "Bool"}, TypeLit{NameLit: "Bool"}), nil

	case token.GTR, token.LEQ, token.GEQ, token.LSS, token.EQL, token.NEQ:
		typeVar := f.Fresh()
		return hm.NewFnType(typeVar, typeVar, TypeLit{NameLit: "Bool"}), nil

	default:
		typeVar := f.Fresh()
		return hm.NewFnType(typeVar, typeVar, typeVar), nil
	}
}

// ----------------------------

// FuncDecl represents a function definition at the top of the source file
// while it is not a valid Expr, it is a valid hm.Expression as it can
// be used for type inference
type FuncDecl struct {
	Range
	NameLit string
	Params  []ParamDecl
	BodyLit Expr
	// consider multiple returns
	Result Type
}

func (param ParamDecl) scheme() (hm.TypeVarSet, hm.Type) {
	var typeSet hm.TypeVarSet
	var paramT hm.Type
	if param.T != nil {
		paramT = param.T
	} else {
		typeSet = append(typeSet, hm.TypeVariable('a'))
		paramT = hm.TypeVariable('a')
	}
	return typeSet, paramT
}

func (f FuncDecl) expandArgsAsLambda(params []ParamDecl) hm.Expression {
	var typeSet hm.TypeVarSet
	var fields []hmtypes.Field
	if len(params) == 0 {
		fields = append(fields, hmtypes.Field{
			Name: "ident:Nil",
			Type: TypeLit{NameLit: "Nil"},
		})
		//return lambdaBodyArg{
		//	paramName:   Ident{Name: "Nil"},
		//	body:        f.BodyLit,
		//	paramScheme: hm.NewScheme(hm.TypeVarSet{}, TypeLit{NameLit: "Nil"}),
		//}
	} else {
		for i, param := range params {
			var paramT hm.Type
			if param.T != nil {
				paramT = param.T
			} else {
				typeSet = append(typeSet, hm.TypeVariable(i))
				paramT = hm.TypeVariable(i)
			}
			fields = append(fields, hmtypes.Field{
				Name: "ident:" + param.Name.Name,
				Type: paramT,
			})
		}
	}

	paramsScheme := hm.NewScheme(typeSet, hmtypes.NewRecordType("", fields...))

	return tupleLambda{
		fName:        Ident{Name: f.NameLit},
		body:         f.BodyLit,
		paramsScheme: paramsScheme,
	}

	//return lambdaBodyArg{
	//	paramName:   Ident{},
	//	body:        nil,
	//	paramScheme: nil,
	//}
	//
	//if len(params) == 1 {
	//	arg := lambdaBodyArg{
	//		paramName:   params[0].Name,
	//		body:        f.BodyLit,
	//		paramScheme: params[0].scheme(),
	//	}
	//	return arg
	//}
	//
	//fstParam := params[0]
	//remainingParams := params[1:]
	//return lambdaBodyArg{
	//	paramName:   fstParam.Name,
	//	body:        f.expandArgsAsLambda(remainingParams),
	//	paramScheme: fstParam.scheme(),
	//}
}

func (f FuncDecl) ToTypeExpression(usedIn hm.Expression) hm.LetRec {
	return letrec{
		name: f.NameLit,
		def:  f.expandArgsAsLambda(f.Params),
		in:   usedIn,
	}
}

func (f FuncDecl) String() string {
	return fmt.Sprintf("%v %v -> %v {\n%v\n}", f.NameLit, f.Params, f.Result, f.BodyLit)
}

func (f FuncDecl) AdditionalEnv() hm.Env {
	var fnType []hm.Type
	var newTypeVars []hm.TypeVariable

	for i, param := range f.Params {
		if param.T != nil {
			fnType = append(fnType, param.T)
		} else {
			// TODO issue#1 when generics are implemented,
			//  we won'paramT make a new var for every param, we will make
			//  a var per generic parameter instead
			newTypeVar := hm.TypeVariable(i)
			// type was not declared, so we make a type variable which will get inferred
			fnType = append(fnType, newTypeVar)
			newTypeVars = append(newTypeVars, newTypeVar)
		}
	}

	if f.Result != nil {
		fnType = append(fnType, f.Result)
	} else {
		newTypeVar := hm.TypeVariable(len(f.Params) + 1)
		newTypeVars = append(newTypeVars, newTypeVar)
		fnType = append(fnType, newTypeVar)
	}

	return hm.SimpleEnv{
		"ident:" + f.NameLit: hm.NewScheme(newTypeVars, hm.NewFnType(fnType...)),
	}
}

type ParamDecl struct {
	Range
	Name Ident
	T    Type
}

// ------------------------
type letrec struct {
	name string
	def  hm.Expression
	in   hm.Expression
}

func (n letrec) Name() string              { return "ident:" + n.name }
func (n letrec) Def() hm.Expression        { return n.def }
func (n letrec) Body() hm.Expression       { return n.in }
func (n letrec) Children() []hm.Expression { return []hm.Expression{n.def, n.in} }
func (n letrec) IsRecursive() bool         { return true }

// ------------------------

type lambdaBodyArg struct {
	paramName Ident
	body      hm.Expression

	// all parameter types, including return
	// can be nil if those are unknown
	paramScheme *hm.Scheme
}

func (n lambdaBodyArg) Name() string        { return "ident:" + n.paramName.Name }
func (n lambdaBodyArg) Body() hm.Expression { return n.body }
func (n lambdaBodyArg) IsLambda() bool      { return true }

func (n lambdaBodyArg) Infer(e hm.Env, f hm.Fresher) (hm.Type, error) {
	if n.paramScheme != nil {
		// add the scheme of the parameter to the environment!
		e.Add("ident:"+n.paramName.Name, n.paramScheme)
	}
	// error will be handled ok by hm, this is not a crash
	return nil, errors.New("not found")
}

// ----------------------

type tupleLambda struct {
	fName Ident
	body  hm.Expression

	// all parameter types, including return
	// can be nil if those are unknown
	paramsScheme *hm.Scheme
}

func (n tupleLambda) Name() string        { return "func:" + n.fName.Name }
func (n tupleLambda) Body() hm.Expression { return n.body }
func (n tupleLambda) IsLambda() bool      { return true }

func (n tupleLambda) Infer(e hm.Env, f hm.Fresher) (hm.Type, error) {
	if n.paramsScheme != nil {
		// add the scheme of the parameter to the environment!
		//e.Add("ident:"+n.paramName.Name, n.paramScheme)
		//n.paramsScheme.

	}
	// error will be handled ok by hm, this is not a crash
	return nil, errors.New("not found")
}
