package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/types"
	"strings"
)

// TypeAnnotation specifies what the program specifies in the source AST, and is
// a schema that is compared to inference.
//
// It is not to be confused with a types.Type (produced via inference), although it
// is also capable of producing a types.Type from TypeDeclarable which is used during inference
type TypeAnnotation interface {
	TypeDeclarable
	Positioner

	// TypeString as printed in the source
	TypeString() string
}

var (
	_ TypeAnnotation = TConst{}
)

type TConst struct {
	Const types.Const
	Range
}

func (t TConst) DeclaredType() TypeAnnotationFn {
	return func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
		return &t.Const, nil
	}
}

func (t TConst) TypeString() string { return t.Const.Name }

type TArrow struct {
	Args   []TypeAnnotation
	Return TypeAnnotation
	Range
}

func (t TArrow) DeclaredType() TypeAnnotationFn {
	return func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
		var args []types.Type

		for i, arg := range t.Args {
			var err error
			args[i], err = arg.DeclaredType()(env, level, using)
			if err != nil {
				return nil, err
			}
		}
		ret, err := t.Return.DeclaredType()(env, level, using)
		if err != nil {
			return nil, err
		}
		return &types.Arrow{
			Args:   args,
			Return: ret,
			//Flags:  0,
		}, nil

	}
}

func (t TArrow) TypeString() string {
	var args []string
	for i, arg := range t.Args {
		args[i] = arg.TypeString()
	}
	joined := strings.Join(args, ",")
	return fmt.Sprintf("(%s) -> %s", joined, t.Return.TypeString())
}
