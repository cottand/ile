package failed

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/types"
)

type ErrCode int

const (
	None           ErrCode = iota
	TypeUnifyConst ErrCode = iota
	Parse
	NoDiscardInWhen
)

type IleError interface {
	Error() string
	Code() ErrCode
	ast.Positioner
}

type Unclassified struct {
	From error
	ast.Positioner
}

func (e Unclassified) Error() string {
	return fmt.Sprintf("unclassified error: %v", e.From)
}
func (e Unclassified) Code() ErrCode { return None }

type ToUnifyConst struct {
	ast.Positioner
	First  types.Type
	Second types.Type
}

func (e ToUnifyConst) Error() string {
	return fmt.Sprintf("(E%03d) type mismatch: expected type '%v', but found a different type '%v'", e.Code(), e.First.TypeName(), e.Second.TypeName())
}
func (e ToUnifyConst) Code() ErrCode { return TypeUnifyConst }

type ToParse struct {
	ast.Positioner
	ParserMessage string
	Hint          string
}

func (e ToParse) Error() string {
	return fmt.Sprintf("(E%03d) %s", e.Code(), e.ParserMessage)
}
func (e ToParse) Code() ErrCode { return Parse }

type ToDiscardInWhen struct {
	ast.Positioner
}

func (e ToDiscardInWhen) Code() ErrCode { return NoDiscardInWhen }
func (e ToDiscardInWhen) Error() string {
	return fmt.Sprintf("(E%03d) last case of when is missing final discard pattern '_'", e.Code())
}
