package backend

import (
	"errors"
	"fmt"
	goast "go/ast"
	"go/token"
	"strconv"

	"github.com/cottand/ile/frontend/ir"
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
			if e.Variadic && i == len(e.Args)-1 {
				tParam = variadicElemType(tParam)
				params[i] = &goast.Field{Type: &goast.Ellipsis{Elt: tParam}}
			} else {
				params[i] = &goast.Field{Type: tParam}
			}
		}
		if isUnitType(e.Return) {
			return &goast.FuncType{
				Params: &goast.FieldList{List: params},
			}, errs
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
		goType, err := nearestGoType(e)
		return goast.NewIdent(goType), err
		// if we are not in a function signature, a record lit, nor an expression body, we must be on a top level declaration.
		// so we can just use the literal value without a type signature
		// to rely on Go inference to make literals be const (i.e., untyped types according to Go spec)
		// TODO re-evaluate this 'default to untyped' approach in favour of keeping track if we are on a top-level
		//  decl, because otherwise this will break every time we eval a type in a new place (eg generics)
		panic("TODO deal with tracking const during transpiling")
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

	case *ir.AppliedType:
		return tp.transpileAppliedType(e)

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

	case *ir.RecordType:
		fields := make([]*goast.Field, 0, len(e.Fields))
		for _, field := range e.Fields {
			fieldType, err := tp.transpileType(field.Type.Out)
			if err != nil {
				fieldType = goast.NewIdent("any")
				tp.Error("failed to transpile field type", "field", field, "err", err)
			}
			fields = append(fields, &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(field.Name.Name)},
				Type:  fieldType,
			})
		}
		return &goast.StructType{
			Fields: &goast.FieldList{List: fields},
		}, nil

	case *ir.NothingType:
		return nil, nil

	default:
		return nil, fmt.Errorf("transpileType: unexpected ir.Type type: %v: %T", ir.TypeString(e), e)
	}
}

// variadicElemType unwraps a slice type ([]T) to get the element type T
// for use in variadic parameter declarations (...T).
// If the type is not a slice, it returns the type as-is.
func variadicElemType(t goast.Expr) goast.Expr {
	if arr, ok := t.(*goast.ArrayType); ok && arr.Len == nil {
		return arr.Elt
	}
	return t
}

func (tp *Transpiler) transpileAppliedType(e *ir.AppliedType) (goast.Expr, error) {
	if e.Base.Name == ir.ListTypeName && len(e.Args) == 1 {
		// List type with elements of a specific type (slice in Go)
		elemTypeExpr, err := tp.transpileType(e.Args[0])
		if err != nil {
			return nil, fmt.Errorf("failed to transpile element type: %v", err)
		}

		return &goast.ArrayType{
			Len: nil, // nil length means slice
			Elt: elemTypeExpr,
		}, nil
	}
	return nil, fmt.Errorf("applied type not supported yet: %v", e.ShowIn(ir.DumbShowCtx, 0))
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
