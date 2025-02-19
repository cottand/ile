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

	// TypeString as would appear in the source
	TypeString() string

	// ConstructType produces a types.Type for use in inference
	ConstructType(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error)
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

func (t TConst) ConstructType(_ hmtypes.TypeEnv, _ uint, _ []hmtypes.Type) (hmtypes.Type, error) {
	return &hmtypes.Const{Name: t.Name}, nil
}

func (t TConst) TypeString() string { return t.Name }

type TComptimeInt struct {
	Range
}

func (t TComptimeInt) ConstructType(_ hmtypes.TypeEnv, _ uint, _ []hmtypes.Type) (hmtypes.Type, error) {
	return &hmtypes.CompTimeConst{
		Name: "<go untyped Int>",
		MaybeUnify: func(other hmtypes.Const) error {
			switch other.Name {
			case "Int":
				return nil
			default:
				return fmt.Errorf("a go untyped int cannot be used with type %s", other.Name)
			}
		},
		DefaultType: &hmtypes.Const{Name: "Int"},
	}, nil
}

func (t TComptimeInt) TypeString() string { return "<go untyped Int>" }

type TArrow struct {
	Args   []TypeAnnotation
	Return TypeAnnotation
	Range
}

func (t TArrow) ConstructType(env hmtypes.TypeEnv, level uint, using []hmtypes.Type) (hmtypes.Type, error) {
	var err error
	var args = make([]hmtypes.Type, len(t.Args))

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
	var ret hmtypes.Type
	if t.Return != nil {
		ret, err = t.Return.ConstructType(env, level, using)
		if err != nil {
			return nil, err
		}
	} else {
		ret = env.NewVar(level)
	}
	return &hmtypes.Arrow{
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
