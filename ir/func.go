package ir

import (
	"github.com/cottand/ile/ir/hm"
	"go/token"
)

type PrimOp token.Token

func (p PrimOp) Token() token.Token  { return token.Token(p) }
func (p PrimOp) IsLambda() bool      { return true }
func (p PrimOp) IsLit() bool         { return true }
func (p PrimOp) Body() hm.Expression { return p }
func (p PrimOp) Name() string {
	return "PrimOp:" + token.Token(p).String()
}
func (p PrimOp) Type() hm.Type { return nil }

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

func (f FuncDecl) Scheme() *hm.Scheme {
	// TODO generics for free bound vars?
	var fnType []hm.Type
	var newTypeVars []hm.TypeVariable
	for i, param := range f.Params {
		if param.T != nil {
			fnType = append(fnType, param.T)
		} else {
			// TODO when generics are implemented,
			//  we won't make a new var for every param, we will make
			//  a var per generic parameter instead
			newTypeVar := hm.TypeVariable(i)
			// type was not declared, so we make a type variable which will get inferred
			fnType = append(fnType, newTypeVar)
			newTypeVars = append(newTypeVars, newTypeVar)
		}
	}

	return hm.NewScheme(newTypeVars, hm.NewFnType(fnType...))
}

func (f FuncDecl) Body() hm.Expression {
	return f
}

func (f FuncDecl) Name() string {
	//TODO implement me
	panic("implement me")
}

func (f FuncDecl) IsLambda() bool {
	//TODO implement me
	panic("implement me")
}

type ParamDecl struct {
	Range
	Name Ident
	T    Type
}
