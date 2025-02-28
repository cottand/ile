package ast

import (
	"go/token"
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
	_ NullaryType = (*TypeVar)(nil)
	_ NullaryType = (*Literal)(nil)
	_ NullaryType = (*Any)(nil)
	_ NullaryType = (*Nothing)(nil)
	_ NullaryType = (*TypeName)(nil)
	_ NullaryType = (*TypeTag)(nil)
)

type TypeVar struct {
	identifier string
	// nameHint may be ""
	nameHint string
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
	name string
	Positioner
}

func (n TypeName) ShowIn(ShowCtx) string { return n.name }
func (n TypeName) isNullaryType()        {}

type TypeTag struct {
	name string
	Positioner
}

func (n TypeTag) ShowIn(ShowCtx) string { return "#" + n.name }
func (n TypeTag) isNullaryType()        {}
