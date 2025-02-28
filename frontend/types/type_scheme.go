package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"go/token"
	"strconv"
	"strings"
)

type typeName = string

type typeScheme interface {
	uninstantiatedBody() simpleType
	instantiate(level int) simpleType
	prov() *typeProvenance
}

type withProvenance struct {
	provenance *typeProvenance
}

func (w withProvenance) prov() *typeProvenance {
	return w.provenance
}

type positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// typeProvenance tracks the origin and description of types
type typeProvenance struct {
	positioner positioner
	desc       string // Description
	originName string // Optional origin name
	isType     bool   // Whether this represents a type
}

func (tp *typeProvenance) embed() withProvenance {
	return withProvenance{
		provenance: tp,
	}

}

func (tp *typeProvenance) IsOrigin() bool {
	return tp.originName != ""
}

func (tp *typeProvenance) Wrapping(new typeProvenance) *typeProvenance {
	new.positioner = tp.positioner
	return &new
}

// simpleType is a type without universally quantified type variables
type simpleType interface {
	typeScheme
	typeInfo
	level() int
	fmt.Stringer
}

var (
	_ simpleType = (*extremeType)(nil)
	_ simpleType = (*intersectionType)(nil)
	_ simpleType = (*unionType)(nil)
	_ simpleType = (*negType)(nil)
	_ simpleType = (*funcType)(nil)
	_ simpleType = (*typeVariable)(nil)

	_ objectTag = (*classTag)(nil)

	_ arrayBase = (*tupleType)(nil)
	_ arrayBase = (*namedTupleType)(nil)
	_ arrayBase = (*arrayType)(nil)
)

// arrayBase is implemented by types which wrap other types
type arrayBase interface {
	simpleType
	inner() simpleType
}

type typeInfo interface {
}

type extremeType struct {
	// polarity = true means bottom, = false means extremeType
	polarity bool
	withProvenance
}

var bottomType = extremeType{polarity: true}
var topType = extremeType{polarity: true}

func (extremeType) level() int                         { return 0 }
func (t extremeType) uninstantiatedBody() simpleType   { return t }
func (t extremeType) instantiate(level int) simpleType { return t }
func (t extremeType) String() string {
	if t.polarity {
		return "bottom"
	} else {
		return "top"
	}
}

type unionType struct {
	lhs, rhs simpleType
	withProvenance
}

func (t unionType) uninstantiatedBody() simpleType   { return t }
func (t unionType) instantiate(level int) simpleType { return t }
func (t unionType) level() int                       { return max(t.lhs.level(), t.rhs.level()) }
func (t unionType) String() string {
	return "(" + t.String() + "|" + t.rhs.String() + ")"
}

type intersectionType struct {
	lhs, rhs simpleType
	withProvenance
}

func (t intersectionType) uninstantiatedBody() simpleType   { return t }
func (t intersectionType) instantiate(level int) simpleType { return t }
func (t intersectionType) level() int                       { return max(t.lhs.level(), t.rhs.level()) }
func (t intersectionType) String() string {
	return "(" + t.String() + "&" + t.rhs.String() + ")"
}

type negType struct {
	withProvenance
	negated simpleType
}

func (t negType) uninstantiatedBody() simpleType   { return t }
func (t negType) instantiate(level int) simpleType { return t }
func (t negType) level() int                       { return t.negated.level() }
func (t negType) String() string                   { return "~(" + t.negated.String() + ")" }

type typeEnv struct {
	env map[string]typeInfo
}

func newTypeEnv(parent *typeEnv) *typeEnv {
	return &typeEnv{}
}

type typeRef struct {
	defName  typeName
	typeArgs []simpleType
	withProvenance
}

func (t typeRef) uninstantiatedBody() simpleType { return t }
func (t typeRef) instantiate(_ int) simpleType   { return t }
func (t typeRef) String() string {
	displayName := t.defName
	if len(t.typeArgs) == 0 {
		return displayName
	}
	return fmt.Sprintf("%s[%s]", displayName, util.JoinString(t.typeArgs, ","))
}
func (t typeRef) level() int {
	maxSoFar := 0
	for _, t := range t.typeArgs {
		if t.level() > maxSoFar {
			maxSoFar = t.level()
		}
	}
	return maxSoFar
}

func (t typeRef) expand(ctx *TypeCtx) simpleType {
	return t.expandWith(ctx, true)
}
func (t typeRef) expandWith(ctx *TypeCtx, withParamTags bool) simpleType {
	panic("implement me")
}

// Fresher keeps track of new variable IDs
// it us mutable and not suitable for concurrent use
type Fresher struct {
	freshCount uint
}

func (t *Fresher) newTypeVariable(
	level int,
	prov typeProvenance,
	nameHint string,
	lowerBounds,
	upperBounds []simpleType,
) typeVariable {
	defer func() {
		t.freshCount++
	}()
	return typeVariable{
		id:             t.freshCount,
		level_:         level,
		lowerBounds:    lowerBounds,
		upperBounds:    upperBounds,
		nameHint:       nameHint,
		withProvenance: prov.embed(),
	}
}

type typeVariableID = uint

// typeVariable living at a certain polymorphism level, with mutable bounds.
// Invariant: Types appearing in the bounds never have a level higher than this variable's `level`
//
// Construct with Fresher.newTypeVariable
type typeVariable struct {
	id                       typeVariableID
	level_                   int
	lowerBounds, upperBounds []simpleType
	// may be "" when not set
	nameHint string
	withProvenance
}

func (t typeVariable) uninstantiatedBody() simpleType { return t }
func (t typeVariable) instantiate(_ int) simpleType   { return t }
func (t typeVariable) String() string {
	name := t.nameHint
	if name == "" {
		name = "α"
	}
	return name + strconv.FormatUint(uint64(t.id), 10) + strings.Repeat("'", t.level_)
}

func (t typeVariable) level() int {
	return t.level_
}

type objectTag interface {
	simpleType
	Compare(other objectTag) int
}
type classTag struct {
	id      ast.AtomicExpr
	parents util.MSet[typeName]
	withProvenance
}

func (t classTag) level() int                     { return 0 }
func (t classTag) uninstantiatedBody() simpleType { return t }
func (t classTag) instantiate(_ int) simpleType   { return t }
func (t classTag) String() string {
	return fmt.Sprintf("#%s<%s>", t.id.CanonicalSyntax(), strings.Join(t.parents.AsSlice(), ","))
}
func (t classTag) Compare(other objectTag) int {
	panic("implement me")
}

// typeRange represents an unknown type between bounds `lb` and `ub`,
// where `lb` is in negative position and `ub` is in positive position.
// For this type to be meaningful, it is necessary that `lb` be a subtype of `ub` by construction.
// Currently, there are essentially two ways these types are introduced:
//
// - During type extrusion, when extruding a variable occurring in neutral position:
// here, we create a type bound to recover the polarity needed to do the extrusion.
// This is correct because the negative extrusion of a type should be a subtype of its positive extrusion.
//
// - In user-entered wildcard types `?`, which are interpreted as `TypeRange(Bot, Top)`.
//
// These are also treated as definition-level existentials,
// in that when checking an inferred type against a signature, which is done in the `subsume` method,
// each wildcard or type range is replaced by a fresh type variable bounded between `lb` and `ub`
// (this happens in rigidification).
type typeRange struct {
	lowerBound, upperBound simpleType
	withProvenance
}

func (t typeRange) uninstantiatedBody() simpleType { return t }
func (t typeRange) instantiate(int) simpleType     { return t }
func (t typeRange) String() string                 { return t.lowerBound.String() + ".." + t.upperBound.String() }
func (t typeRange) level() int                     { return 0 }

func (ctx *TypeCtx) makeTypeRange(lowerBound, upperBound simpleType, provenance *typeProvenance) simpleType {
	if ctx.TypesEquivalent(lowerBound, upperBound) {
		return lowerBound
	}
	if lowerBound, ok := lowerBound.(typeRange); ok {
		return ctx.makeTypeRange(lowerBound.lowerBound, upperBound, provenance)
	}
	if upperBound, ok := upperBound.(typeRange); ok {
		return ctx.makeTypeRange(lowerBound, upperBound.upperBound, provenance)
	}
	return typeRange{
		lowerBound:     lowerBound,
		upperBound:     upperBound,
		withProvenance: withProvenance{provenance},
	}
}

type funcType struct {
	args, ret simpleType
	withProvenance
}

func (t funcType) uninstantiatedBody() simpleType { return t }
func (t funcType) instantiate(int) simpleType     { return t }
func (t funcType) level() int                     { return max(t.args.level(), t.ret.level()) }
func (t funcType) String() string {
	var argsStr string
	// if there is a single tuple type, and it has no var, that's our type
	if asTuple, ok := t.args.(tupleType); ok && len(asTuple.fields) == 1 {
		argsStr = asTuple.fields[0].String()
	} else {
		argsStr = t.args.String()
	}
	return fmt.Sprintf("(%s -> %s)", argsStr, t.ret.String())
}

// tupleType is for known-width structures with specific types (say, [Int, String, Int])
//
// in the mlstruct scala implementation, this is simply a TupleType
type tupleType struct {
	fields []simpleType
	withProvenance
}

func (t tupleType) uninstantiatedBody() simpleType { return t }
func (t tupleType) instantiate(int) simpleType     { return t }
func (t tupleType) level() int {
	l := 0
	for _, field := range t.fields {
		l = max(l, field.level())
	}
	return l
}

// inner makes a union out of all subtypes
func (t tupleType) inner() simpleType {
	var acc simpleType = bottomType
	for _, field := range t.fields {
		acc = unionOf(acc, field, unionOpts{})
	}
	return acc
}
func (t tupleType) String() string {
	return "(" + util.JoinString(t.fields, ", ") + ")"
}
func (t tupleType) toArray() simpleType {
	return arrayType{
		innerT:         t.inner(),
		withProvenance: t.withProvenance,
	}
}

// namedTupleType is a tupleType where fields also have names.
// It is useful for function parameters for example
//
// in the mlstruct scala implementation, this is simply a TupleType
type namedTupleType struct {
	fields []util.Pair[ast.Var, simpleType]
	withProvenance
}

func (t namedTupleType) uninstantiatedBody() simpleType { return t }
func (t namedTupleType) instantiate(int) simpleType     { return t }
func (t namedTupleType) level() int {
	l := 0
	for _, field := range t.fields {
		l = max(l, field.Snd.level())
	}
	return l
}

// inner makes a union out of all subtypes
func (t namedTupleType) inner() simpleType {
	var acc simpleType = bottomType
	for _, field := range t.fields {
		acc = unionOf(acc, field.Snd, unionOpts{})
	}
	return acc
}
func (t namedTupleType) String() string {
	var fieldStrs []string = make([]string, len(t.fields))
	for i, field := range t.fields {
		fieldStrs[i] = field.Fst.Name + ": " + field.Snd.String() + ","
	}
	return "(" + strings.Join(fieldStrs, " ") + ")"
}

// arrayType is like tupleType, except we don't know how many elements there are,
// so instead of enumerating them they all get the same innerT type
type arrayType struct {
	innerT simpleType
	withProvenance
}

func (t arrayType) uninstantiatedBody() simpleType { return t }
func (t arrayType) instantiate(int) simpleType     { return t }
func (t arrayType) level() int                     { return t.innerT.level() }
func (t arrayType) String() string                 { return "Array<" + t.innerT.String() + ">" }
func (t arrayType) inner() simpleType              { return t.innerT }
