package ast

import (
	"github.com/cottand/ile/util"
	"go/token"
	"strings"
)

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
	_ Type = (*Record)(nil)
	_ Type = (*AppliedType)(nil)

	_ NullaryType = (*TypeVar)(nil)
	_ NullaryType = (*Literal)(nil)
	_ NullaryType = (*Any)(nil)
	_ NullaryType = (*Nothing)(nil)
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

func (TypeVar) ShowIn(ctx ShowCtx) string {
	panic("")
}
func (TypeVar) isNullaryType() {}

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

type Nothing struct{ Positioner }

func (*Nothing) ShowIn(ShowCtx) string { return "Nothing" }
func (*Nothing) isNullaryType()        {}

type Any struct {
	Positioner
}

func (*Any) ShowIn(ShowCtx) string { return "Any" }
func (*Any) isNullaryType()        {}

type ShowCtx interface {
	NameOf(typeVar TypeVar) string
}

type TypeName struct {
	Name string
	Positioner
}

func (n *TypeName) ShowIn(ShowCtx) string { return n.Name }
func (n *TypeName) isNullaryType()        {}

type TypeTag struct {
	name string
	Positioner
}

func (n *TypeTag) ShowIn(ShowCtx) string { return "#" + n.name }
func (n *TypeTag) isNullaryType()        {}

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
