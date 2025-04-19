package ast

import (
	"go/token"
	"strings"
)

type TypeDefKind uint8

const (
	_ TypeDefKind = iota
	// KindClass ('class') is a whole new type in Go and ile semantics
	KindClass
	KindAlias
	KindTrait
)

func (k TypeDefKind) String() string {
	switch k {
	case KindClass:
		return "class"
	case KindAlias:
		return "type alias"
	case KindTrait:
		return "trait"
	default:
		return "invalid"
	}
}

type TypeDefinition struct {
	Kind       TypeDefKind
	Name       TypeName
	TypeParams []TypeName
	Body       Type
	Positioner
}
type Field struct {
	// In may be nil, but not Out
	In, Out Type
	Positioner
}

// Type is not the same as a hmtypes.Type (legacy HM type system)
// it can be found in the source (provided by the user) or inferred
type Type interface {
	ShowIn(ShowCtx) string
	Positioner
}

type NullaryType interface {
	Type
	isNullaryType()
}

var (
	_ Type = (*IntersectionType)(nil)
	_ Type = (*UnionType)(nil)
	//_ Type = (*Record)(nil)
	_ Type = (*AppliedType)(nil)
	_ Type = (*TypeBounds)(nil)
	_ Type = (*ConstrainedType)(nil)
	_ Type = (*FnType)(nil)

	_ NullaryType = (*TypeVar)(nil)
	_ NullaryType = (*Literal)(nil)
	_ NullaryType = (*AnyType)(nil)
	_ NullaryType = (*NothingType)(nil)
	_ NullaryType = (*TypeName)(nil)
	_ NullaryType = (*TypeTag)(nil)
)

type IntersectionType struct {
	Left, Right Type
	Positioner
}

func (t *IntersectionType) ShowIn(ctx ShowCtx) string {
	return t.Left.ShowIn(ctx) + " & " + t.Right.ShowIn(ctx)
}

type UnionType struct {
	Left, Right Type
	Positioner
}

func (t *UnionType) ShowIn(ctx ShowCtx) string {
	return t.Left.ShowIn(ctx) + " | " + t.Right.ShowIn(ctx)
}

type AppliedType struct {
	Base TypeName
	Args []Type
	Positioner
}

func (t *AppliedType) ShowIn(ctx ShowCtx) string {
	sb := strings.Builder{}
	sb.WriteString(t.Base.ShowIn(ctx))
	sb.WriteString("<")
	for _, arg := range t.Args {
		sb.WriteString(arg.ShowIn(ctx) + ", ")
	}
	sb.WriteString(">")
	return sb.String()
}

type Record struct{}

type TypeVar struct {
	Identifier string
	// NameHint may be ""
	NameHint string
	Positioner
}

func (t *TypeVar) ShowIn(ctx ShowCtx) string {
	return ctx.NameOf(t)
}
func (*TypeVar) isNullaryType() {}

func (t *Literal) ShowIn(ctx ShowCtx) string {
	switch t.Kind {
	case token.INT, token.FLOAT:
		return t.Syntax
	case token.STRING:
		return "\"" + t.Syntax + "\""
	default:
		panic("unreachable")
	}
}
func (*Literal) isNullaryType() {}

// NothingType corresponds to Bottom in the type lattice
// no value ever is the NothingType type
type NothingType struct{ Positioner }

func (*NothingType) ShowIn(ShowCtx) string { return NothingTypeName }
func (*NothingType) isNullaryType()        {}

// AnyType corresponds to Top in the type lattice
// all values have the AnyType type
type AnyType struct {
	Positioner
}

func (*AnyType) ShowIn(ShowCtx) string { return AnyTypeName }
func (*AnyType) isNullaryType()        {}

type ShowCtx interface {
	NameOf(typeVar *TypeVar) string
}

type TypeName struct {
	Name string
	Positioner
}

func (n *TypeName) ShowIn(ShowCtx) string { return n.Name }
func (n *TypeName) isNullaryType()        {}

type TypeTag struct {
	Name string
	Positioner
}

func (n *TypeTag) ShowIn(ShowCtx) string { return "#" + n.Name }
func (n *TypeTag) isNullaryType()        {}

type TypeBounds struct {
	Lower, Upper Type
	Range
}

func (n *TypeBounds) ShowIn(ctx ShowCtx) string {
	lowerStr := n.Lower.ShowIn(ctx)
	upperStr := n.Upper.ShowIn(ctx)

	if lowerStr == upperStr {
		return lowerStr
	}
	if _, ok := n.Lower.(*NothingType); ok {
		if _, ok := n.Upper.(*AnyType); ok {
			return "?"
		}
		return ".. " + upperStr
	}
	if _, ok := n.Upper.(*AnyType); ok {
		return lowerStr + " .."
	}
	return lowerStr + " .. " + upperStr
}

type ConstrainedEntry = struct {
	Var    *TypeVar
	Bounds *TypeBounds
}
type ConstrainedType struct {
	Base  Type
	Where []ConstrainedEntry
	Range
}

func (t *ConstrainedType) ShowIn(ctx ShowCtx) string {
	entries := make([]string, 0, len(t.Where))
	for _, entry := range t.Where {

		var lhs, rhs string

		v := ctx.NameOf(entry.Var)
		if _, isNothing := entry.Bounds.Lower.(*NothingType); isNothing {
			lhs = ""
		} else {
			lhs = entry.Bounds.Lower.ShowIn(ctx) + " <: "
		}

		if _, isAny := entry.Bounds.Upper.(*AnyType); isAny {
			rhs = ""
			// the reference scala implementation chooses to use :> here,
			// but we simply invert the symbol here (personal preference)
		} else {
			rhs = " <: " + entry.Bounds.Upper.ShowIn(ctx)
		}
		entries = append(entries, lhs+v+rhs)
	}
	return t.Base.ShowIn(ctx) + " where " + strings.Join(entries, ", ")

}

type FnType struct {
	Args   []Type
	Return Type
	Range
}

func (t *FnType) ShowIn(ctx ShowCtx) string {
	argShow := make([]string, 0, len(t.Args))
	for _, arg := range t.Args {
		argShow = append(argShow, arg.ShowIn(ctx))
	}

	return "fn " + strings.Join(argShow, ", ") + " -> " + t.Return.ShowIn(ctx)
}
