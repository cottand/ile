package ir

import (
	"fmt"
	"go/token"
	"log/slog"
	"reflect"
)


// Universe keeps track of identName -> Type
// and can mutate it freely
type Universe struct {
	idents map[string]Type
}

func (u *Universe) Has(i Ident) bool {
	_, ok := u.idents[i.Name]
	return ok
}

func (u *Universe) TypeOf(i Ident) (Type, bool) {
	t, ok := u.idents[i.Name]
	return t, ok
}

// TypeInferrable is capable of producing a new node of itself
// with additional type annotations
// It should return itself if no inference could be performed.
//
// When N is not the leaf node of the AST, TypeInfer should be called
// on children too
type TypeInferrable[N Node] interface {
	TypeInfer(u Universe) (N, []*CompileError)
}

type ExprInferrable[E Expr] interface {
	TypeInfer(u Universe) (Type, []*CompileError)
}

func (f File) TypeInfer(u Universe) (res File, errs []*CompileError) {
	newVals := make([]ValDecl, 0)

	for _, v := range f.Values {
		inferredVal, err := v.TypeInfer(u)
		if err != nil {
			errs = append(errs, err...)
			continue
		}
		newVals = append(newVals, inferredVal)
	}
	f.Values = newVals
	return f, errs
}

func (v ValDecl) TypeInfer(u Universe) (ValDecl, []*CompileError) {
	if v.T != nil {
		return v, nil
	}
	var errs []*CompileError
	v.T, errs = TypeInfer(u, v.E)
	return v, errs
}

func TypeInfer(u Universe, expr Expr) (t Type, errs []*CompileError) {
	switch t := expr.(type) {
	case BasicLitExpr:
		eT, err := t.TypeInfer()
		if err != nil {
			errs = append(errs, err)
		}
		return eT, errs

	case IdentifierLitExpr:
		if u.Has(t.Ident) {

		}
		// TODO validate that it must be in u

	case BinaryOpExpr:
		lhs, err := TypeInfer(u, t.Lhs)
		rhs, err2 := TypeInfer(u, t.Rhs)
		if err != nil {
			errs = append(errs, err...)
		}
		if err2 != nil {
			errs = append(errs, err2...)
		}
		if lhs == nil || rhs == nil {
			return lhs, errs
		}

		//if !lhs.CanBinaryOp(rhs) {
		//	return lhs, []*CompileError{{
		//		At:      t.Range,
		//		Message: fmt.Sprintf("invalid operation: type mismatch for (%v) and (%v)", lhs, rhs),
		//	}}
		//}
		if t.IsBoolOp() {
			return TypeLit{NameLit: "Bool"}, nil
		}
		return lhs, nil
	default:
		slog.Warn("ignoring inference attempt on unknown type", "type", reflect.TypeOf(expr))
	}
	return nil, []*CompileError{{
		Message: "failed to infer on unknown types",
		//At:      // TODO
	}}
}

func (e BasicLitExpr) TypeInfer() (Type, *CompileError) {
	switch e.Kind {
	case token.INT:
		return TypeLit{NameLit: "Int"}, nil
	case token.STRING:
		return TypeLit{NameLit: "String"}, nil
	default:

	}
	return nil, &CompileError{
		Message: fmt.Sprintf("unknown type for basic lit %v", e.Kind),
		At:      e.Range,
	}
}
