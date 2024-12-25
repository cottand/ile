package ir

import (
	"fmt"
	"go/token"
	"log/slog"
	"reflect"
)

// TypeInferrable is capable of producing a new node of itself
// with additional type annotations
// It should return itself if no inference could be performed.
//
// When N is not the leaf node of the AST, TypeInfer should be called
// on children too
type TypeInferrable[N Node] interface {
	TypeInfer() (N, *TypeInferenceError)
}

type ExprInferrable[E Expr]interface{
	TypeInfer() (Type, *TypeInferenceError)
}

type TypeInferenceError struct {
	Message string
	At Range
}

func (f File) TypeInfer() (File, *TypeInferenceError) {
	newVals := make([]ValDecl, 0)
	for _, v := range f.Values {
		inferredVal, err := v.TypeInfer()
		if err != nil {
			return File{}, err
		}
		newVals = append(newVals, inferredVal)
	}
	f.Values = newVals
	return f, nil
}

func (v ValDecl) TypeInfer() (ValDecl, *TypeInferenceError) {
	if v.T != nil {
		return v, nil
	}
	var err *TypeInferenceError
	v.T, err = TypeInfer(v.E)
	return v, err
}

func TypeInfer(expr Expr) (Type, *TypeInferenceError) {
	switch t := expr.(type) {
	case BasicLit:
		eT, err := t.TypeInfer()
		if err != nil {
			return nil, err
		}
		return eT, nil

	case BinaryOpExpr:
		lhs, err := TypeInfer(t.Lhs)
		if err != nil {
			return nil, err
		}
		rhs, err := TypeInfer(t.Rhs)
		if lhs != nil && lhs.Equals(rhs) {
			return lhs, nil
		}
	default:
		slog.Warn("ignoring inference attempt on unknown type", "type", reflect.TypeOf(expr))
	}
	return nil, &TypeInferenceError{
		Message: "failed to infer on unknown types",
		//At:      // TODO
	}
}

func (e BasicLit) TypeInfer() (Type, *TypeInferenceError)  {
	switch e.Kind {
	case token.INT:
		return TypeLit{Name: "Int"}, nil
	default:

	}
	return nil, &TypeInferenceError{
		Message: fmt.Sprintf("unknown type for basic lit %v", e.Kind),
		At: e.Range,
	}
}