package ir

import "go/token"

// TypeInferrable is capable of producing a new node of itself
// with additional type annotations
// It should return itself if no inference could be performed.
//
// When N is not the leaf node of the AST, TypeInfer should be called
// on children too
type TypeInferrable[N Node] interface {
	TypeInfer() N
}

func (f File) TypeInfer() File {
	newVals := make([]ValDecl, 0)
	for _, v := range f.Values {
		newVals = append(newVals, v.TypeInfer())
	}
	f.Values = newVals
	return f
}

func (v ValDecl) TypeInfer() ValDecl {
	if v.T != nil {
		return v
	}
	switch t := v.E.(type) {
	case BasicLit:
		switch t.Kind {
		case token.INT:
			v.T = TypeLit{Name: "Int"}
		default:
		}
	}
	return v
}
