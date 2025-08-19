package types

import (
	"github.com/cottand/ile/frontend/ir"
	"github.com/hashicorp/go-set/v3"
	"slices"
)

var builtinProv = newOriginProv(ir.Range{}, "builtin", "builtin")

// instantiated types that can be referenced directly
// so they cannot contain type variables, therefore, they are usable anywhere
var (
	// anyClassTag is called Object in the reference scala implementation
	anyClassTag = classTag{
		id:             &ir.Var{Name: ir.AnyTypeName},
		parents:        set.New[typeName](0),
		withProvenance: withProvenance{builtinProv},
	}
	intType = classTag{
		id:             &ir.Var{Name: ir.IntTypeName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	floatType = classTag{
		id:             &ir.Var{Name: ir.FloatTypeName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	stringType = classTag{
		id:             &ir.Var{Name: ir.StringTypeName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	trueType = classTag{
		id:             &ir.Var{Name: ir.TrueName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	falseType = classTag{
		id:             &ir.Var{Name: ir.FalseName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
	boolType = unionType{
		lhs:            trueType,
		rhs:            falseType,
		withProvenance: withProvenance{builtinProv},
	}
	nilType = classTag{
		id:             &ir.Var{Name: ir.NilTypeName},
		parents:        set.From([]typeName{ir.AnyTypeName}),
		withProvenance: withProvenance{builtinProv},
	}
)

type universeStruct struct {
	any, nothing       SimpleType
	string, int, float SimpleType
	true, false, bool  SimpleType
	plus               SimpleType
}

func (t *Fresher) universeEnv() map[string]typeInfo {
	u := t.universe

	comparisonBinOp := func() funcType {
		tv := t.newTypeVariable(1, emptyProv, "", nil, nil)
		return funcType{
			args:           []SimpleType{tv, tv},
			ret:            boolType,
			withProvenance: withProvenance{builtinProv},
		}
	}
	_ = func() funcType {
		addableTypes := unionOf(intType, stringType, unionOpts{})
		tv := t.newTypeVariable(1, builtinProv, "", []SimpleType{addableTypes}, nil)
		tv2 := t.newTypeVariable(1, builtinProv, "", nil, []SimpleType{addableTypes})
		return funcType{
			args: []SimpleType{tv, tv},
			ret:  tv2,
		}
	}
	numberOp := func() funcType {
		return funcType{
			args:           []SimpleType{intType, intType},
			ret:            intType,
			withProvenance: withProvenance{builtinProv},
		}
	}
	return map[string]typeInfo{
		"+":          u.plus,
		"*":          numberOp(),
		"-":          numberOp(),
		"/":          numberOp(),
		"%":          numberOp(),
		ir.TrueName:  u.true,
		ir.FalseName: u.false,
		">":          comparisonBinOp(),
		"<":          comparisonBinOp(),
		"==":         comparisonBinOp(),
		"!=":         comparisonBinOp(),
		"println":    funcType{args: []SimpleType{anyClassTag}, ret: nilType},
		"panic":      funcType{args: []SimpleType{anyClassTag}, ret: bottomType},
		// until we can overload `+`
		"++": funcType{args: []SimpleType{stringType, stringType}, ret: stringType},
	}
}

// universeInit should only be called during NewFresher
// it is a function rather than a static struct so that it can instantiate
// polymorphic types (which require first instantiating the type variables).
func (t *Fresher) universeInit() universeStruct {
	return universeStruct{
		any:     anyClassTag,
		nothing: bottomType,
		string:  stringType,
		int:     intType,
		float:   floatType,
		true:    trueType,
		false:   falseType,
		bool:    boolType,
		// overloading plus for string concatenation or floats can only be done via
		// overloading, and the only overloading MLStruct supports
		// is type class overloading, so we are leaving that for later for now
		plus: func() SimpleType {
			tv := intType
			return funcType{
				args:           []SimpleType{tv, tv},
				ret:            tv,
				withProvenance: withProvenance{builtinProv},
			}
		}(),
	}
}

var errorTypeInstance = classTag{
	id:      &ir.Var{Name: "Error"},
	parents: set.New[typeName](0),
	withProvenance: withProvenance{
		provenance: typeProvenance{
			desc: "Error",
		},
	},
}

var builtinTypes = []TypeDefinition{
	{
		defKind:     ir.KindClass,
		name:        ir.IntTypeName,
		bodyType:    topType,
		baseClasses: intType.parents,
	},
	{
		defKind:     ir.KindClass,
		name:        ir.FloatTypeName,
		bodyType:    topType,
		baseClasses: floatType.parents,
	},
	{
		defKind:     ir.KindClass,
		name:        ir.StringTypeName,
		bodyType:    topType,
		baseClasses: intType.parents,
	},
	{
		defKind:     ir.KindClass,
		name:        ir.TrueName,
		bodyType:    topType,
		baseClasses: set.From([]typeName{ir.AnyTypeName}),
	},
	{
		defKind:     ir.KindClass,
		name:        ir.FalseName,
		bodyType:    topType,
		baseClasses: set.From([]typeName{ir.AnyTypeName}),
	},
	{
		defKind:  ir.KindAlias,
		name:     ir.BoolTypeName,
		bodyType: boolType,
	},
	{
		defKind:     ir.KindClass,
		name:        ir.NilTypeName,
		bodyType:    topType,
		baseClasses: set.From([]typeName{ir.AnyTypeName}),
	},
}

func (_ *TypeCtx) isBuiltinType(name string) bool {
	return slices.ContainsFunc(builtinTypes, func(def TypeDefinition) bool {
		return def.name == name
	})
}
