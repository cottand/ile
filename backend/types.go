package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	goast "go/ast"
	"go/token"
	"strconv"
)

func (tp *Transpiler) transpileType(t ir.Type) (goast.Expr, error) {
	if t == nil {
		return nil, nil
	}
	// some shortcuts
	switch t.Hash() {
	case ir.BoolType.Hash(), ir.BoolTypeUnaliased.Hash():
		return goast.NewIdent("bool"), nil
		//case (&ir.NothingType{}).Hash():
		//	return nil, nil
	}
	switch e := t.(type) {
	case *ir.FnType:
		if e == nil {
			panic("nil type for arrow function")
		}
		var params = make([]*goast.Field, len(e.Args))
		var errs error
		for i, arg := range e.Args {
			tParam, err := tp.transpileType(arg)
			errs = errors.Join(errs, err)
			params[i] = &goast.Field{Type: tParam}
		}
		retType, err := tp.transpileType(e.Return)
		errs = errors.Join(errs, err)
		if err != nil {
			return nil, err
		}
		return &goast.FuncType{
			Params:  &goast.FieldList{List: params},
			Results: &goast.FieldList{List: []*goast.Field{{Type: retType}}},
		}, nil

	case *ir.TypeName:
		goEquivalent, ok := typeTagToGoType(e.Name)
		if ok {
			return goast.NewIdent(goEquivalent), nil
		}
		return goast.NewIdent(e.Name), nil

		// inferred to literal value
	case *ir.Literal:
		if tp.inFunctionSignature {
			goType, err := nearestGoType(e)
			return goast.NewIdent(goType), err
		}
		// if we are not in a function signature, we can just use the literal value without a type signature
		// to rely on Go inference to make literals be const (i.e., untyped types according to Go spec)
		return nil, nil

		// generic type!
	case *ir.TypeVar:
		return nil, fmt.Errorf("generics are not implemented yet, but got %v", t.ShowIn(ir.DumbShowCtx, 0))

	case *ir.AnyType:
		return goast.NewIdent("any"), nil

	case *ir.ListLiteralType:
		if len(e.ElementTypes) == 0 {
			// Empty list, use interface{} as element type
			return &goast.ArrayType{
				Len: &goast.BasicLit{
					Kind:  token.INT,
					Value: "0",
				},
				Elt: goast.NewIdent("interface{}"),
			}, nil
		}

		goType, err := tp.transpileType(e.InnerType())
		if err != nil {
			return nil, fmt.Errorf("failed to transpile inner type: %v", err)
		}
		return &goast.ArrayType{
			Len: &goast.BasicLit{
				Kind:  token.INT,
				Value: strconv.Itoa(len(e.ElementTypes)),
			},
			Elt: goType,
		}, nil

	case *ir.ListType:
		// List type with elements of a specific type (slice in Go)
		elemTypeExpr, err := tp.transpileType(e.ElementType)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile element type: %v", err)
		}

		return &goast.ArrayType{
			Len: nil, // nil length means slice
			Elt: elemTypeExpr,
		}, nil

	case *ir.UnionType:
		// try to find a common supertype for literals - otherwise, use any
		common, err := tp.tryFindCommonType(e.Left, e.Right)
		if err != nil {
			return nil, fmt.Errorf("failed to find common supertype: %v", err)
		}
		if lit, ok := common.(*ir.Literal); ok {
			str, err := nearestGoType(lit)
			return goast.NewIdent(str), err
		}
		return tp.transpileType(common)

	default:
		return nil, fmt.Errorf("transpileType: unexpected ast.Type type: %v: %T", ir.TypeString(e), e)
	}
}

func (tp *Transpiler) tryFindCommonType(left, right ir.Type) (ir.Type, error) {
	switch left := left.(type) {
	case *ir.Literal:
		if right, ok := right.(*ir.Literal); ok {
			if right.Kind == left.Kind {
				// will be approximated with nearestGoType
				return left, nil
			}
		}
	case *ir.UnionType:
		commonInUnion, err := tp.tryFindCommonType(left.Left, left.Right)
		if err != nil {
			return nil, err
		}
		return tp.tryFindCommonType(commonInUnion, right)
	}

	tp.Warn("could not find common supertype for types", "left", left, "right", right)
	return &ir.AnyType{}, nil
}
