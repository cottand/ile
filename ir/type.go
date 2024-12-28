package ir

import (
	"fmt"
	"github.com/cottand/ile/ir/hm"
)

type Type interface {
	hm.Type
	fmt.Stringer
	Node
	typeNode()
}

type TypeLit struct {
	Range
	Package string
	NameLit string
}

func (TypeLit) typeNode() {}

func (t TypeLit) CanBinaryOp(other Type) bool {
	sameType, ok := other.(TypeLit)
	return ok && sameType.NameLit == t.NameLit && sameType.Package == t.Package
}
func (t TypeLit) String() string {
	if t.Package != "" {
		return fmt.Sprintf("%s.%s", t.Package, t.NameLit)
	}
	return t.NameLit
}

// implement hm.Type

func (t TypeLit) Name() string                                            { return "TypeLit:" + t.String() }
func (t TypeLit) Apply(hm.Subs) hm.Substitutable                          { return t }
func (t TypeLit) FreeTypeVar() hm.TypeVarSet                              { return nil }
func (t TypeLit) Normalize(hm.TypeVarSet, hm.TypeVarSet) (hm.Type, error) { return t, nil }
func (t TypeLit) Types() hm.Types                                         { return nil }
func (t TypeLit) Eq(other hm.Type) bool {
	if ot, ok := other.(TypeLit); ok {
		return ot.NameLit == t.NameLit && ot.Package == t.Package
	}
	return false
}
func (t TypeLit) Format(s fmt.State, c rune) { _, _ = fmt.Fprintf(s, t.String()) }

// ---------

type FuncType struct {
	Range
	*hm.FunctionType
	Params []Type
	Result Type
}

func NewFuncType(params []Type, result Type) FuncType {
	if params == nil {
		params = []Type{TypeLit{NameLit: "Nil"}}
	}
	newSlice := make([]hm.Type, len(params)+1)
	// copy slice
	for i, p := range params {
		newSlice[i] = p
	}
	newSlice[len(params)] = result

	return FuncType{
		FunctionType: hm.NewFnType(newSlice...),
		Params:       params,
		Result:       result,
	}
}

func (FuncType) typeNode() {}
func (f FuncType) String() string {
	return fmt.Sprintf("(%s)", f.FunctionType.String())
}
