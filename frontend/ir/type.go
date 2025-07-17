package ir

import (
	"encoding/binary"
	"fmt"
	"go/token"
	"go/types"
	"hash/fnv"
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

func TypeString(t Type) string {
	return t.ShowIn(DumbShowCtx, 0)

}

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
type FieldType struct {
	// In is the type this FieldType accepts - if nil, this FieldType is immutable
	In Type
	// Out is the type you get when selecting this FieldType - it is never nil as records can always be read
	Out Type
	Range
}

// Type is different from a hmtypes.Type (legacy HM type system)
// it can be found in the source (provided by the user) or inferred
type Type interface {
	ShowIn(ctx ShowCtx, outerPrecedence uint16) string
	Hash() uint64
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
	_ Type = (*ListLiteralType)(nil)
	_ Type = (*ListType)(nil)

	_ NullaryType = (*TypeVar)(nil)
	_ NullaryType = (*Literal)(nil)
	_ NullaryType = (*AnyType)(nil)
	_ NullaryType = (*NothingType)(nil)
	_ NullaryType = (*TypeName)(nil)
	_ NullaryType = (*TypeTag)(nil)
	_ NullaryType = (*GoType)(nil)
)

type IntersectionType struct {
	Left, Right Type
	Positioner
}

func (t *IntersectionType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	var thisPrecedence uint16 = 25
	return t.Left.ShowIn(ctx, thisPrecedence) + " & " + t.Right.ShowIn(ctx, thisPrecedence)
}

func (t *IntersectionType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("IntersectionType"))
	arr := make([]byte, 0)
	arr = binary.LittleEndian.AppendUint64(arr, t.Left.Hash())
	arr = binary.LittleEndian.AppendUint64(arr, t.Right.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

type UnionType struct {
	Left, Right Type
	Positioner
}

func (t *UnionType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	const thisPrecedence uint16 = 20
	return t.Left.ShowIn(ctx, thisPrecedence) + " | " + t.Right.ShowIn(ctx, thisPrecedence)
}

func (t *UnionType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("UnionType"))
	arr := make([]byte, 0)
	arr = binary.LittleEndian.AppendUint64(arr, t.Left.Hash())
	arr = binary.LittleEndian.AppendUint64(arr, t.Right.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

type AppliedType struct {
	Base TypeName
	Args []Type
	Positioner
}

func (t *AppliedType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	sb := strings.Builder{}
	sb.WriteString(t.Base.ShowIn(ctx, 0))
	sb.WriteString("<")
	for _, arg := range t.Args {
		sb.WriteString(arg.ShowIn(ctx, 0) + ", ")
	}
	sb.WriteString(">")
	return sb.String()
}

func (t *AppliedType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("AppliedType"))
	arr := make([]byte, 0)
	arr = binary.LittleEndian.AppendUint64(arr, t.Base.Hash())
	for _, arg := range t.Args {
		arr = binary.LittleEndian.AppendUint64(arr, arg.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

type Record struct{}

type TypeVar struct {
	Identifier string
	// NameHint may be ""
	NameHint string
	Range
}

func (t *TypeVar) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	return ctx.NameOf(t)
}
func (*TypeVar) isNullaryType() {}

func (t *TypeVar) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("TypeVar"))
	_, _ = h.Write([]byte(t.Identifier))
	_, _ = h.Write([]byte(t.NameHint))
	return h.Sum64()
}

func (e *Literal) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	switch e.Kind {
	case token.INT, token.FLOAT:
		return e.Syntax
	case token.STRING:
		return "\"" + e.Syntax + "\""
	default:
		panic("unreachable")
	}
}
func (*Literal) isNullaryType() {}

// NothingType corresponds to Bottom in the type lattice
// no value ever is the NothingType type
type NothingType struct{ Positioner }

func (*NothingType) ShowIn(ShowCtx, uint16) string { return NothingTypeName }
func (*NothingType) isNullaryType()                {}

func (*NothingType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("NothingType"))
	return h.Sum64()
}

// AnyType corresponds to Top in the type lattice
// all values have the AnyType type
type AnyType struct {
	Positioner
}

func (*AnyType) ShowIn(ShowCtx, uint16) string { return AnyTypeName }
func (*AnyType) isNullaryType()                {}

func (*AnyType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("AnyType"))
	return h.Sum64()
}

type ShowCtx interface {
	NameOf(typeVar *TypeVar) string
}

type dumbShowCtx struct{}

var DumbShowCtx ShowCtx = (*dumbShowCtx)(nil)

func (*dumbShowCtx) NameOf(typeVar *TypeVar) string { return typeVar.Identifier }

type TypeName struct {
	Name string
	Range

	// Provenance is a metadata hint for the origin of the type definition for debugging information
	// (optional)
	Provenance string
}

func (n *TypeName) ShowIn(ShowCtx, uint16) string { return n.Name }
func (n *TypeName) isNullaryType()                {}

func (n *TypeName) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("TypeName"))
	_, _ = h.Write([]byte(n.Name))
	return h.Sum64()
}

type TypeTag struct {
	Name string
	Positioner
}

func (n *TypeTag) ShowIn(ShowCtx, uint16) string { return "#" + n.Name }
func (n *TypeTag) isNullaryType()                {}

func (n *TypeTag) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("TypeTag" + n.Name))
	return h.Sum64()
}

type TypeBounds struct {
	Lower, Upper Type
	Range
}

func (n *TypeBounds) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	lowerStr := n.Lower.ShowIn(ctx, 0)
	upperStr := n.Upper.ShowIn(ctx, 0)

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

func (n *TypeBounds) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("TypeBounds"))
	arr := make([]byte, 0, 4)
	arr = binary.LittleEndian.AppendUint64(arr, n.Lower.Hash())
	arr = binary.LittleEndian.AppendUint64(arr, n.Upper.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
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

func (t *ConstrainedType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	entries := make([]string, 0, len(t.Where))
	for _, entry := range t.Where {

		var lhs, rhs string

		v := ctx.NameOf(entry.Var)
		if _, isNothing := entry.Bounds.Lower.(*NothingType); isNothing {
			lhs = ""
		} else {
			lhs = entry.Bounds.Lower.ShowIn(ctx, 0) + " <: "
		}

		if _, isAny := entry.Bounds.Upper.(*AnyType); isAny {
			rhs = ""
			// the reference scala implementation chooses to use :> here,
			// but we simply invert the symbol here (personal preference)
		} else {
			rhs = " <: " + entry.Bounds.Upper.ShowIn(ctx, 0)
		}
		entries = append(entries, lhs+v+rhs)
	}
	return t.Base.ShowIn(ctx, 0) + " where " + strings.Join(entries, ", ")
}

func (t *ConstrainedType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("ConstrainedType"))
	arr := make([]byte, 0)
	arr = binary.LittleEndian.AppendUint64(arr, t.Base.Hash())
	for _, entry := range t.Where {
		arr = binary.LittleEndian.AppendUint64(arr, entry.Var.Hash())
		arr = binary.LittleEndian.AppendUint64(arr, entry.Bounds.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

// FnType represents a function type like fn A, B -> C
//
// Exceptionally, Args or Return may be nil, because we allow partially specifying the type of a function in the AST
type FnType struct {
	Args   []Type
	Return Type
	Range
}

func withParensIf(when bool, str string) string {
	if when {
		return "(" + str + ")"
	}
	return str
}

func (t *FnType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	argShow := make([]string, 0, len(t.Args))
	for _, arg := range t.Args {
		str := "_"
		if arg != nil {
			str = arg.ShowIn(ctx, 20)
		}
		argShow = append(argShow, str)
	}

	args := strings.Join(argShow, ", ")
	if args != "" {
		args = args + " "
	}
	retStr := "_"
	if t.Return != nil {
		retStr = t.Return.ShowIn(ctx, 30)
	}
	return withParensIf(outerPrecedence > 30, "fn "+args+"-> "+retStr)
}

func (t *FnType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("FnType")
	for _, arg := range t.Args {
		hash := uint64(1)
		if arg != nil {
			hash = arg.Hash()
		}
		arr = binary.LittleEndian.AppendUint64(arr, hash)
	}
	hash := uint64(1)
	if hash == 0 {
		hash = t.Return.Hash()
	}
	arr = binary.LittleEndian.AppendUint64(arr, hash)
	_, _ = h.Write(arr)
	return h.Sum64()
}

type RecordField struct {
	Name Var
	Type FieldType
	Range
}
type RecordType struct {
	Fields []RecordField
	Range
}

func (e *RecordType) ShowIn(ctx ShowCtx, _ uint16) string {
	if len(e.Fields) == 0 {
		return "{}"
	}
	var sb strings.Builder
	sb.WriteString("{")
	firstField := true
	for i, nameField := range e.Fields {
		if !firstField {
			sb.WriteString(",")
		}
		firstField = false
		sb.WriteString(" " + nameField.Name.Name + ": " + nameField.Type.Out.ShowIn(ctx, 0))
		if i > 6 {
			sb.WriteString(fmt.Sprintf("... (%d more)", len(e.Fields)-i))
			break
		}
	}
	sb.WriteString(" }")

	return sb.String()
}

func (e *RecordType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("RecordType")
	for _, field := range e.Fields {
		_, _ = h.Write([]byte(field.Name.Name))
		arr = binary.LittleEndian.AppendUint64(arr, field.Type.Out.Hash())
		if field.Type.In != nil {
			arr = binary.LittleEndian.AppendUint64(arr, field.Type.In.Hash())
		}
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

// GoType is a type that represents an external type in Go.
//
// It can only surface in the IR from Go source code.
type GoType struct {
	Underlying types.Object
	Range
	// Package provenance information
	PackagePath string // Full import path of the package
	PackageName string // Short name of the package
	SourceFile  string // Source file where the type is defined (if available)
}

func (t *GoType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	if t.PackageName != "" {
		return fmt.Sprintf("%s.%s", t.PackageName, t.Underlying.Name())
	}
	return t.Underlying.String()
}
func (t *GoType) isNullaryType() {}
func (t *GoType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("GoType"))
	_, _ = h.Write([]byte(t.Underlying.String()))
	_, _ = h.Write([]byte(t.Underlying.Pkg().Path()))
	return h.Sum64()
}

// ListType represents a list type with elements of a specific type
type ListType struct {
	ElementType Type
	Positioner
}

func (t *ListType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	return "List<" + t.ElementType.ShowIn(ctx, 0) + ">"
}

func (t *ListType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("ListType"))
	arr := make([]byte, 0)
	arr = binary.LittleEndian.AppendUint64(arr, t.ElementType.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// ListLiteralType represents a tuple type with elements of specific types
// It's used for tuples of known width and subtypes (e.g., [Int, String, Int])
type ListLiteralType struct {
	ElementTypes []Type
	// InnerType lazily resolves the common supertype of all the ElementTypes
	InnerType func() Type
	Positioner
}

func (t *ListLiteralType) ShowIn(ctx ShowCtx, outerPrecedence uint16) string {
	sb := strings.Builder{}
	sb.WriteString("[")
	for i, elemType := range t.ElementTypes {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(elemType.ShowIn(ctx, 0))
	}
	sb.WriteString("]")
	return sb.String()
}

func (t *ListLiteralType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("ListLiteralType"))
	arr := make([]byte, 0)
	for _, elemType := range t.ElementTypes {
		arr = binary.LittleEndian.AppendUint64(arr, elemType.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}
