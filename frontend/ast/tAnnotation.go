package ast

import (
	"fmt"
	"github.com/cottand/ile/frontend/types"
	"strings"
)

// TypeAnnotation specifies what the program specifies in the source AST, and is
// a schema that is compared against inference results.
//
// It is not to be confused with a types.Type (produced via inference), although it
// is also capable of producing a types.Type from TAnnotated which is used during inference
type TypeAnnotation interface {
	Positioner

	// TypeString as printed in the source
	TypeString() string

	// ConstructType produces a types.Type for use in inference
	ConstructType(env types.TypeEnv, level uint, using []types.Type) (types.Type, error)
}

var (
	_ TypeAnnotation = TConst{}
	_ TypeAnnotation = TArrow{}
)

type TConst struct {
	Name    string
	Package string
	Range
}

func (t TConst) ConstructType(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
	return &types.Const{Name: t.Name}, nil
}

func (t TConst) TypeString() string { return t.Name }

type TArrow struct {
	Args   []TypeAnnotation
	Return TypeAnnotation
	Range
}

func (t TArrow) ConstructType(env types.TypeEnv, level uint, using []types.Type) (types.Type, error) {
	var err error
	var args = make([]types.Type, len(t.Args))

	for i, arg := range t.Args {
		if arg == nil {
			args[i] = env.NewVar(level)
			continue
		}
		args[i], err = arg.ConstructType(env, level, using)
		if err != nil {
			return nil, err
		}
		if args[i] == nil {
			args[i] = env.NewVar(level)
		}
		// no type was constructed, so we make a new var
	}
	var ret types.Type
	if t.Return != nil {
		ret, err = t.Return.ConstructType(env, level, using)
		if err != nil {
			return nil, err
		}
	} else {
		ret = env.NewVar(level)
	}
	return &types.Arrow{
		Args:   args,
		Return: ret,
		//Flags:  0,
	}, nil
}

func (t TArrow) TypeString() string {
	var args []string
	for i, arg := range t.Args {
		args[i] = arg.TypeString()
	}
	joined := strings.Join(args, ",")
	return fmt.Sprintf("(%s) -> %s", joined, t.Return.TypeString())
}
