package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"go/token"
	"iter"
	"slices"
	"strconv"
	"strings"
)

type typeName = string

type TypeScheme interface {
	uninstantiatedBody() SimpleType
	instantiate(level level) SimpleType
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

func errorType() SimpleType {
	return classTag{
		id:      &ast.Var{Name: "Error"},
		parents: util.MSet[typeName]{},
	}
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

// SimpleType is a type without universally quantified type variables
type SimpleType interface {
	TypeScheme
	typeInfo
	util.Equivalenceable[SimpleType]
	fmt.Stringer
	level() level
	children(includeBounds bool) iter.Seq[SimpleType]
}

var (
	_ TypeScheme = (*PolymorphicType)(nil)

	_ SimpleType = (*extremeType)(nil)
	_ SimpleType = (*intersectionType)(nil)
	_ SimpleType = (*unionType)(nil)
	_ SimpleType = (*negType)(nil)
	_ SimpleType = (*funcType)(nil)
	_ SimpleType = (*typeVariable)(nil)

	_ objectTag = (*classTag)(nil)
	_ objectTag = (*traitTag)(nil)

	_ arrayBase = (*tupleType)(nil)
	_ arrayBase = (*namedTupleType)(nil)
	_ arrayBase = (*arrayType)(nil)

	_ SimpleType = (*recordType)(nil)

	_ SimpleType = (*PolymorphicType)(nil)
)

// wrappingProvType encapsulates another SimpleType but contains different provenance info
type wrappingProvType struct {
	SimpleType
	proxyProvenance *typeProvenance
}

func (t wrappingProvType) underlying() SimpleType { return t.SimpleType }
func (t wrappingProvType) String() string         { return "[" + t.SimpleType.String() + "]" }
func (t wrappingProvType) prov() *typeProvenance  { return t.proxyProvenance }

// arrayBase is implemented by types which wrap other types
type arrayBase interface {
	SimpleType
	inner() SimpleType
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
var emptySeqSimpleType iter.Seq[SimpleType] = func(_ func(SimpleType) bool) { return }

func (extremeType) level() level                         { return 0 }
func (t extremeType) uninstantiatedBody() SimpleType     { return t }
func (t extremeType) instantiate(level level) SimpleType { return t }
func (t extremeType) children(bool) iter.Seq[SimpleType] { return emptySeqSimpleType }

// equivalent is true when two types are equal except for ProvType, which is equivalent
// to the underlying type, which is necessary for recursive types to associate type provenances to
// their recursive uses without making the constraint solver diverge
func (t extremeType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(extremeType)
	return ok && t.polarity == otherT.polarity
}
func (t extremeType) String() string {
	if t.polarity {
		return "bottom"
	} else {
		return "top"
	}
}

type unionType struct {
	lhs, rhs SimpleType
	withProvenance
}

func (t unionType) uninstantiatedBody() SimpleType     { return t }
func (t unionType) instantiate(level level) SimpleType { return t }
func (t unionType) level() level                       { return max(t.lhs.level(), t.rhs.level()) }
func (t unionType) String() string {
	return "(" + t.String() + "|" + t.rhs.String() + ")"
}
func (t unionType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(unionType)
	return ok && t.lhs.Equivalent(otherT.lhs) && t.rhs.Equivalent(otherT.rhs)
}
func (t unionType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lhs) {
			yield(t.rhs)
		}
	}
}

type intersectionType struct {
	lhs, rhs SimpleType
	withProvenance
}

func (t intersectionType) uninstantiatedBody() SimpleType     { return t }
func (t intersectionType) instantiate(level level) SimpleType { return t }
func (t intersectionType) level() level                       { return max(t.lhs.level(), t.rhs.level()) }
func (t intersectionType) String() string {
	return "(" + t.String() + "&" + t.rhs.String() + ")"
}
func (t intersectionType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(intersectionType)
	return ok && t.lhs.Equivalent(otherT.lhs) && t.rhs.Equivalent(otherT.rhs)
}
func (t intersectionType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lhs) {
			yield(t.rhs)
		}
	}
}

type negType struct {
	withProvenance
	negated SimpleType
}

func (t negType) uninstantiatedBody() SimpleType     { return t }
func (t negType) instantiate(level level) SimpleType { return t }
func (t negType) level() level                       { return t.negated.level() }
func (t negType) String() string                     { return "~(" + t.negated.String() + ")" }
func (t negType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(negType)
	return ok && t.negated.Equivalent(otherT.negated)
}
func (t negType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) { yield(t.negated) }
}

type typeEnv struct {
	env map[string]typeInfo
}

func newTypeEnv(parent *typeEnv) *typeEnv {
	return &typeEnv{}
}

type typeRef struct {
	defName  typeName
	typeArgs []SimpleType
	withProvenance
}

func (t typeRef) uninstantiatedBody() SimpleType     { return t }
func (t typeRef) instantiate(level level) SimpleType { return t }
func (t typeRef) String() string {
	displayName := t.defName
	if len(t.typeArgs) == 0 {
		return displayName
	}
	return fmt.Sprintf("%s[%s]", displayName, util.JoinString(t.typeArgs, ","))
}
func (t typeRef) level() level {
	maxSoFar := level(0)
	for _, t := range t.typeArgs {
		if t.level() > maxSoFar {
			maxSoFar = t.level()
		}
	}
	return maxSoFar
}

func (ctx *TypeCtx) expand(t typeRef) SimpleType {
	return ctx.expandWith(t, true)
}
func (t typeRef) Equivalent(other SimpleType) bool {
	otherT, ok := other.(typeRef)
	return ok && t.defName == otherT.defName && util.SlicesEquivalent(t.typeArgs, otherT.typeArgs)
}
func (ctx *TypeCtx) expandWith(t typeRef, withParamTags bool) SimpleType {
	panic("implement me")
}
func (t typeRef) children(bool) iter.Seq[SimpleType] {
	return slices.Values(t.typeArgs)
}

// Fresher keeps track of new variable IDs
// it us mutable and not suitable for concurrent use
type Fresher struct {
	freshCount uint
}

func (t *Fresher) newTypeVariable(
	level level,
	prov *typeProvenance,
	nameHint string,
	lowerBounds,
	upperBounds []SimpleType,
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

func newOriginProv(pos ast.Positioner, description string, name string) *typeProvenance {
	return &typeProvenance{
		positioner: pos,
		desc:       description,
		originName: name,
		isType:     true,
	}
}

type typeVariableID = uint

// typeVariable living stack a certain polymorphism level, with mutable bounds.
// Invariant: Types appearing in the bounds never have a level higher than this variable's `level`
//
// Construct with Fresher.newTypeVariable
type typeVariable struct {
	id                       typeVariableID
	level_                   level
	lowerBounds, upperBounds []SimpleType
	// may be "" when not set
	nameHint string
	withProvenance
}

func (t typeVariable) uninstantiatedBody() SimpleType     { return t }
func (t typeVariable) instantiate(level level) SimpleType { return t }
func (t typeVariable) String() string {
	name := t.nameHint
	if name == "" {
		name = "α"
	}
	return name + strconv.FormatUint(uint64(t.id), 10) + strings.Repeat("'", int(t.level_))
}

func (t typeVariable) level() level {
	return t.level_
}

// Equivalent only compares id for typeVariable
func (t typeVariable) Equivalent(other SimpleType) bool {
	otherT, ok := other.(typeVariable)
	return ok && t.id == otherT.id
}
func (t typeVariable) children(includeBounds bool) iter.Seq[SimpleType] {
	if !includeBounds {
		return emptySeqSimpleType
	}
	return util.ConcatIter[SimpleType](slices.Values(t.lowerBounds), slices.Values(t.upperBounds))
}

type objectTag interface {
	SimpleType
	Compare(other objectTag) int
}
type classTag struct {
	id      ast.AtomicExpr
	parents util.MSet[typeName]
	withProvenance
}

func (t classTag) level() level                       { return 0 }
func (t classTag) uninstantiatedBody() SimpleType     { return t }
func (t classTag) instantiate(level level) SimpleType { return t }
func (t classTag) String() string {
	return fmt.Sprintf("#%s<%s>", t.id.CanonicalSyntax(), strings.Join(t.parents.AsSlice(), ","))
}
func (t classTag) Compare(other objectTag) int {
	panic("implement me")
}
func (t classTag) children(bool) iter.Seq[SimpleType] { return emptySeqSimpleType }

// TODO unclear whether equivalent requires more than the ID to be equal for classTag
func (t classTag) Equivalent(other SimpleType) bool {
	otherT, ok := other.(classTag)
	return ok && t.id.Equivalent(otherT.id) && t.parents.Equals(otherT.parents)
}

type traitTag struct {
	id ast.AtomicExpr
	withProvenance
}

func (t traitTag) level() level                       { return 0 }
func (t traitTag) uninstantiatedBody() SimpleType     { return t }
func (t traitTag) instantiate(level level) SimpleType { return t }
func (t traitTag) String() string {
	return fmt.Sprintf("#%s", t.id.CanonicalSyntax())
}
func (t traitTag) Compare(other objectTag) int {
	panic("implement me")
}

func (t traitTag) Equivalent(other SimpleType) bool {
	//TODO implement me
	panic("implement me")
}

func (t traitTag) children(includeBounds bool) iter.Seq[SimpleType] { return emptySeqSimpleType }

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
	lowerBound, upperBound SimpleType
	withProvenance
}

func (t typeRange) uninstantiatedBody() SimpleType { return t }
func (t typeRange) instantiate(level) SimpleType   { return t }
func (t typeRange) String() string                 { return t.lowerBound.String() + ".." + t.upperBound.String() }
func (t typeRange) level() level                   { return 0 }
func (t typeRange) Equivalent(other SimpleType) bool {
	otherT, ok := other.(typeRange)
	return ok && otherT.upperBound.Equivalent(otherT.upperBound) && otherT.lowerBound.Equivalent(otherT.lowerBound)
}
func (t typeRange) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lowerBound) {
			yield(t.upperBound)
		}
	}
}
func (ctx *TypeCtx) makeTypeRange(lowerBound, upperBound SimpleType, provenance *typeProvenance) SimpleType {
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
	args []SimpleType
	ret  SimpleType
	withProvenance
}

func (t funcType) uninstantiatedBody() SimpleType { return t }
func (t funcType) instantiate(level) SimpleType   { return t }
func (t funcType) level() level {
	maxArgLevel := level(0)
	for _, arg := range t.args {
		maxArgLevel = max(maxArgLevel, arg.level())
	}
	return max(maxArgLevel, t.ret.level())
}
func (t funcType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(funcType)
	return ok && t.ret.Equivalent(otherT.ret) && util.SlicesEquivalent(t.args, otherT.args)
}
func (t funcType) String() string {
	var argsStr string = util.JoinString(t.args, ", ")
	return fmt.Sprintf("(fn %s -> %s)", argsStr, t.ret.String())
}
func (t funcType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		for _, arg := range t.args {
			if !yield(arg) {
				return
			}
		}
		yield(t.ret)
	}
}

// tupleType is for known-width structures with specific types (say, [Int, String, Int])
//
// in the mlstruct scala implementaution, this is simply a TupleType
type tupleType struct {
	fields []SimpleType
	withProvenance
}

func (t tupleType) uninstantiatedBody() SimpleType { return t }
func (t tupleType) instantiate(level) SimpleType   { return t }
func (t tupleType) level() level {
	l := level(0)
	for _, field := range t.fields {
		l = max(l, field.level())
	}
	return l
}
func (t tupleType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(tupleType)
	return ok && util.SlicesEquivalent(t.fields, otherT.fields)
}

// inner makes a union out of all subtypes
func (t tupleType) inner() SimpleType {
	var acc SimpleType = bottomType
	for _, field := range t.fields {
		acc = unionOf(acc, field, unionOpts{})
	}
	return acc
}
func (t tupleType) String() string {
	return "(" + util.JoinString(t.fields, ", ") + ")"
}
func (t tupleType) toArray() SimpleType {
	return arrayType{
		innerT:         t.inner(),
		withProvenance: t.withProvenance,
	}
}

func (t tupleType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		for _, field := range t.fields {
			if !yield(field) {
				return
			}
		}
	}
}

// namedTupleType is a tupleType where fields also have names.
// It is useful for function parameters for example
//
// in the mlstruct scala implementation, this is simply a TupleType
type namedTupleType struct {
	fields []util.Pair[ast.Var, SimpleType]
	withProvenance
}

func (t namedTupleType) uninstantiatedBody() SimpleType { return t }
func (t namedTupleType) instantiate(level) SimpleType   { return t }
func (t namedTupleType) level() level {
	l := level(0)
	for _, field := range t.fields {
		l = max(l, field.Snd.level())
	}
	return l
}

// inner makes a union out of all subtypes
func (t namedTupleType) inner() SimpleType {
	var acc SimpleType = bottomType
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
func (t namedTupleType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(namedTupleType)
	return ok && slices.EqualFunc(t.fields, otherT.fields, func(left util.Pair[ast.Var, SimpleType], right util.Pair[ast.Var, SimpleType]) bool {
		return left.Fst.Name == right.Fst.Name && left.Snd.Equivalent(right.Snd)
	})
}
func (t namedTupleType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		for _, field := range t.fields {
			if !yield(field.Snd) {
				return
			}
		}
	}
}

type recordType struct {
	fields []util.Pair[ast.Var, fieldType]
	withProvenance
}

func (t recordType) uninstantiatedBody() SimpleType { return t }
func (t recordType) instantiate(level) SimpleType   { return t }
func (t recordType) level() level {
	l := level(0)
	for _, field := range t.fields {
		l = max(l, field.Snd.level())
	}
	return l
}
func (t recordType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(recordType)
	return ok && slices.EqualFunc(t.fields, otherT.fields, func(left util.Pair[ast.Var, fieldType], right util.Pair[ast.Var, fieldType]) bool {
		return left.Fst.Name == right.Fst.Name && left.Snd.equivalent(right.Snd)
	})
}
func (t recordType) inner() SimpleType {
	var acc SimpleType = bottomType
	for _, field := range t.fields {
		acc = unionOf(acc, field.Snd.upperBound, unionOpts{})
	}
	return acc
}
func (t recordType) String() string {
	var fieldStrs = make([]string, len(t.fields))
	for i, field := range t.fields {
		fieldStrs[i] = field.Fst.Name + ": " + field.Snd.String() + ","
	}
	return "{" + strings.Join(fieldStrs, " ") + "}"
}
func (t recordType) children(bool) iter.Seq[SimpleType] {
	panic("TODO what is the child of a record type?")
}

// fieldType represents either a normal record field OR a type parameter in a class,
// which are encoded using fields.
// The lower bound `lb` will only be non-bottom when the field is used as a type parameter.
type fieldType struct {
	lowerBound, upperBound SimpleType
	withProvenance
}

func (t fieldType) equivalent(other fieldType) bool {
	return t.lowerBound.Equivalent(other.lowerBound) && t.upperBound.Equivalent(other.upperBound)
}
func (t fieldType) String() string {
	return t.lowerBound.String() + ".." + t.upperBound.String()
}
func (t fieldType) level() level {
	return max(t.lowerBound.level(), t.upperBound.level())
}
func (t fieldType) inner() SimpleType {
	return t.upperBound
}
func (t fieldType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lowerBound) {
			return
		}
		if !yield(t.upperBound) {
			return
		}
	}
}

// arrayType is like tupleType, except we don't know how many elements there are,
// so instead of enumerating them they all get the same innerT type
type arrayType struct {
	innerT SimpleType
	withProvenance
}

func (t arrayType) uninstantiatedBody() SimpleType { return t }
func (t arrayType) instantiate(level) SimpleType   { return t }
func (t arrayType) level() level                   { return t.innerT.level() }
func (t arrayType) String() string                 { return "Array<" + t.innerT.String() + ">" }
func (t arrayType) inner() SimpleType              { return t.innerT }
func (t arrayType) Equivalent(other SimpleType) bool {
	otherT, ok := other.(arrayType)
	return ok && t.innerT.Equivalent(otherT.innerT)
}
func (t arrayType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		yield(t.innerT)
	}
}

type PolymorphicType struct {
	Body   SimpleType
	_level level
	withProvenance
}

func (p PolymorphicType) Equivalent(other SimpleType) bool {
	//TODO implement me
	panic("implement me")
}

func (p PolymorphicType) String() string {
	//TODO implement me
	panic("implement me")
}

func (p PolymorphicType) level() level { return p._level }

func (p PolymorphicType) children(includeBounds bool) iter.Seq[SimpleType] {
	//TODO implement me
	panic("implement me")
}

func (p PolymorphicType) prov() *typeProvenance {
	return p.Body.prov()
}
func (p PolymorphicType) End() token.Pos {
	return p.Body.prov().positioner.End()
}
func (p PolymorphicType) Pos() token.Pos {
	return p.Body.prov().positioner.Pos()
}
func (p PolymorphicType) instantiate(level level) SimpleType {
	return level.freshenAbove(p._level, p.Body)
}
func (p PolymorphicType) uninstantiatedBody() SimpleType { return p.Body }
func (p PolymorphicType) rigidify(level level) SimpleType {
	return level.freshenAboveWithRigidify(p._level, p.Body, true)
}
