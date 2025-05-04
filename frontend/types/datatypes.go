package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"go/token"
	"hash/fnv"
	"iter"
	"slices"
	"strconv"
	"strings"
)

type typeName = string

type TypeScheme interface {
	uninstantiatedBody() SimpleType
	instantiate(fresher *Fresher, level level) SimpleType
	prov() typeProvenance
}

type withProvenance struct {
	provenance typeProvenance
}

// Equal can be used to compare SimpleType instances for equality.
// We implement it here rather than in individual types because each type has
// its own interpretation of equality.
//
// HISTORICAL NOTE -- For example, wrappingProvType wants to be equal
// to its underlying type and vice versa. This would be impossible to implement via a Equals() *method* without
// having every other type check that the 'other' is a wrappingProvType and then
// comparing the underlying type.
func Equal[H, HH set.Hasher[uint64]](this H, other HH) bool {
	return this.Hash() == other.Hash()
}

func (w withProvenance) prov() typeProvenance {
	return w.provenance
}

type positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// typeProvenance tracks the origin and description of types
type typeProvenance struct {
	Range      ast.Range
	desc       string // Description
	originName string // Optional origin name
	isType     bool   // Whether this represents a type
}

var emptyProv = typeProvenance{}

func errorType() SimpleType {
	return errorTypeInstance
}

func (tp *typeProvenance) embed() withProvenance {
	return withProvenance{
		provenance: *tp,
	}

}

func (tp *typeProvenance) IsOrigin() bool {
	return tp.originName != ""
}

func (tp *typeProvenance) Wrapping(new typeProvenance) *typeProvenance {
	new.Range = tp.Range
	return &new
}
func (tp *typeProvenance) Pos() token.Pos {
	if tp == nil {
		return token.NoPos
	}
	return tp.Range.Pos()
}
func (tp *typeProvenance) End() token.Pos {
	if tp == nil {
		return token.NoPos
	}
	return tp.Range.End()
}

// SimpleType is a type without universally quantified type variables
type SimpleType interface {
	TypeScheme
	fmt.Stringer
	Hash() uint64
	level() level
	children(includeBounds bool) iter.Seq[SimpleType]
}

type BasicType interface {
}

var (
	_ TypeScheme = (*PolymorphicType)(nil)

	_ SimpleType = (*extremeType)(nil)
	_ SimpleType = (*intersectionType)(nil)
	_ SimpleType = (*unionType)(nil)
	_ SimpleType = (*negType)(nil)
	_ SimpleType = (*funcType)(nil)
	_ SimpleType = (*typeVariable)(nil)
	_ SimpleType = (*typeRef)(nil)
	_ SimpleType = (*wrappingProvType)(nil)

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
	proxyProvenance typeProvenance
}

func (t wrappingProvType) underlying() SimpleType { return t.SimpleType }
func (t wrappingProvType) String() string         { return "prov(" + t.SimpleType.String() + ")" }
func (t wrappingProvType) prov() typeProvenance   { return t.proxyProvenance }

// arrayBase is implemented by types which wrap other types
type arrayBase interface {
	SimpleType
	inner() SimpleType
}

// typeInfo is what we store in TypeCtx.env to store info about the current scope
// in the reference scala implementation, it can be a TypeScheme or an
// AbstractConstructor. We are not implementing the latter until we implement traits.
type typeInfo = TypeScheme

type extremeType struct {
	// polarity = true means bottom, = false means extremeType
	polarity bool
	withProvenance
}

var bottomType = extremeType{polarity: true}
var topType = extremeType{polarity: false}
var emptySeqSimpleType iter.Seq[SimpleType] = func(_ func(SimpleType) bool) { return }

func (extremeType) level() level                                           { return 0 }
func (t extremeType) uninstantiatedBody() SimpleType                       { return t }
func (t extremeType) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t extremeType) children(bool) iter.Seq[SimpleType]                   { return emptySeqSimpleType }
func (t extremeType) isTop() bool {
	return !t.polarity
}

func (t extremeType) String() string {
	if t.polarity {
		return "bottom"
	} else {
		return "top"
	}
}

// Hash generates a hash for wrappingProvType using its underlying SimpleType
func (t wrappingProvType) Hash() uint64 {
	return t.SimpleType.Hash()
}

// Hash generates a hash for extremeType using its polarity
func (t extremeType) Hash() uint64 {
	if t.polarity {
		return 16777619 // FNV-1a prime for true/bottom
	}
	return 1099511628211 // FNV-1a prime for false/top
}

// Hash generates a hash for unionType using its lhs and rhs
func (t unionType) Hash() uint64 {
	lhsHash := t.lhs.Hash()
	rhsHash := t.rhs.Hash()
	return lhsHash*31 + rhsHash*37
}

// Hash generates a hash for intersectionType using its lhs and rhs
func (t intersectionType) Hash() uint64 {
	lhsHash := t.lhs.Hash()
	rhsHash := t.rhs.Hash()
	return lhsHash*41 + rhsHash*43
}

// Hash generates a hash for negType using its negated type
func (t negType) Hash() uint64 {
	return t.negated.Hash() * 53
}

// unionType is a composedType with positive polarity in the scala reference
type unionType struct {
	lhs, rhs SimpleType
	withProvenance
}

func (t unionType) uninstantiatedBody() SimpleType                       { return t }
func (t unionType) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t unionType) level() level                                         { return max(t.lhs.level(), t.rhs.level()) }
func (t unionType) String() string {
	return "(" + t.lhs.String() + "|" + t.rhs.String() + ")"
}
func (t unionType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lhs) {
			yield(t.rhs)
		}
	}
}

// intersectionType is a composedType with negative polarity in the scala reference
type intersectionType struct {
	lhs, rhs SimpleType
	withProvenance
}

func (t intersectionType) uninstantiatedBody() SimpleType                       { return t }
func (t intersectionType) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t intersectionType) level() level                                         { return max(t.lhs.level(), t.rhs.level()) }
func (t intersectionType) String() string {
	return "(" + t.lhs.String() + "&" + t.rhs.String() + ")"
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

func (t negType) uninstantiatedBody() SimpleType                       { return t }
func (t negType) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t negType) level() level                                         { return t.negated.level() }
func (t negType) String() string                                       { return "~(" + t.negated.String() + ")" }
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

func (t typeRef) uninstantiatedBody() SimpleType                       { return t }
func (t typeRef) instantiate(fresher *Fresher, level level) SimpleType { return t }
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
func (ctx *TypeCtx) expandWith(t typeRef, withParamTags bool) SimpleType {
	panic("implement me")
}
func (t typeRef) children(bool) iter.Seq[SimpleType] {
	return slices.Values(t.typeArgs)
}
func (t typeRef) Hash() uint64 {
	const prime1 uint64 = 14695981039346656037
	var hash = prime1
	h := fnv.New64a()
	_, _ = h.Write([]byte(t.defName))
	for _, arg := range t.typeArgs {
		hash = hash*31 + arg.Hash()
	}
	return h.Sum64() ^ hash
}

// corresponds to mapTArgs in the scala reference
func (t typeRef) forEachTypeArg(ctx *TypeCtx, pol polarity, fn func(polarity, SimpleType)) {
	def, ok := ctx.typeDefs[t.defName]
	if !ok {
		panic("type reference to unknown type! Not handled here")
	}

	if len(def.typeVarVariances) == 0 {
		for _, arg := range t.typeArgs {
			fn(invariant, arg)
			return
		}
	}

	if len(def.typeVarVariances) != len(t.typeArgs) {
		panic("mismatched types - expected as many variances as args")
	}

	for i, arg := range t.typeArgs {
		tv := def.typeParamArgs[i]
		vari, ok := def.typeVarVariances[tv.Snd.id]
		if !ok {
			vari = varianceInvariant
		}
		if vari == varianceBivaraint {
			fn(invariant, typeRange{
				lowerBound: bottomType,
				upperBound: topType,
			})
			return
		}
		var paramVariance polarity
		if vari.covariant {
			paramVariance = pol
		} else if vari.contravariant {
			paramVariance = pol.inverse()
		} else {
			paramVariance = invariant
		}
		fn(paramVariance, arg)
	}

}

func newOriginProv(pos ast.Positioner, description string, name string) typeProvenance {
	return typeProvenance{
		Range:      ast.RangeOf(pos),
		desc:       description,
		originName: name,
		isType:     true,
	}
}

type TypeVarID = uint64

// typeVariable living stack a certain polymorphism level, with mutable bounds.
// Invariant: Types appearing in the bounds never have a level higher than this variable's `level`
//
// Construct with Fresher.newTypeVariable
type typeVariable struct {
	id                       TypeVarID
	level_                   level
	lowerBounds, upperBounds []SimpleType
	// may be "" when not set
	nameHint string
	withProvenance
}

func (t *typeVariable) uninstantiatedBody() SimpleType                       { return t }
func (t *typeVariable) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t *typeVariable) String() string {
	name := t.nameHint
	if name == "" {
		name = "α"
	}
	return name + strconv.FormatUint(uint64(t.id), 10) + strings.Repeat("'", int(t.level_))
}

func (t *typeVariable) level() level {
	return t.level_
}

func (t *typeVariable) children(includeBounds bool) iter.Seq[SimpleType] {
	if !includeBounds {
		return emptySeqSimpleType
	}
	return util.ConcatIter[SimpleType](slices.Values(t.lowerBounds), slices.Values(t.upperBounds))
}

type objectTag interface {
	SimpleType
	Compare(other objectTag) int
	Id() ast.AtomicExpr
}
type classTag struct {
	id ast.AtomicExpr
	// collection of
	parents set.Collection[typeName]
	withProvenance
}

func (t classTag) Id() ast.AtomicExpr                                   { return t.id }
func (t classTag) level() level                                         { return 0 }
func (t classTag) uninstantiatedBody() SimpleType                       { return t }
func (t classTag) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t classTag) String() string {
	return fmt.Sprintf("#%s<%s>", t.id.CanonicalSyntax(), strings.Join(t.parents.Slice(), ","))
}
func (t classTag) Compare(other objectTag) int {
	panic("implement me")
}
func (t classTag) children(bool) iter.Seq[SimpleType] { return emptySeqSimpleType }

func (t classTag) containsParentST(other ast.AtomicExpr) bool {
	asVar, isVar := other.(*ast.Var)
	return isVar && t.parents.Contains(asVar.Name)
}

type traitTag struct {
	id ast.AtomicExpr
	withProvenance
}

func (t traitTag) Id() ast.AtomicExpr                                   { return t.id }
func (t traitTag) level() level                                         { return 0 }
func (t traitTag) uninstantiatedBody() SimpleType                       { return t }
func (t traitTag) instantiate(fresher *Fresher, level level) SimpleType { return t }
func (t traitTag) String() string {
	return fmt.Sprintf("#%s", t.id.CanonicalSyntax())
}
func (t traitTag) Compare(other objectTag) int {
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

func (t typeRange) uninstantiatedBody() SimpleType         { return t }
func (t typeRange) instantiate(*Fresher, level) SimpleType { return t }
func (t typeRange) String() string                         { return t.lowerBound.String() + ".." + t.upperBound.String() }
func (t typeRange) level() level                           { return 0 }

func (t typeRange) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		if !yield(t.lowerBound) {
			yield(t.upperBound)
		}
	}
}
func (ctx *TypeCtx) makeTypeRange(lowerBound, upperBound SimpleType, provenance typeProvenance) SimpleType {
	if Equal(lowerBound, upperBound) {
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
func (t typeRange) Hash() uint64 {
	return 31*t.upperBound.Hash() + 91*t.lowerBound.Hash()
}

type funcType struct {
	args []SimpleType
	ret  SimpleType
	withProvenance
}

func (t funcType) uninstantiatedBody() SimpleType         { return t }
func (t funcType) instantiate(*Fresher, level) SimpleType { return t }
func (t funcType) level() level {
	maxArgLevel := level(0)
	for _, arg := range t.args {
		maxArgLevel = max(maxArgLevel, arg.level())
	}
	return max(maxArgLevel, t.ret.level())
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

func (t tupleType) uninstantiatedBody() SimpleType         { return t }
func (t tupleType) instantiate(*Fresher, level) SimpleType { return t }
func (t tupleType) level() level {
	l := level(0)
	for _, field := range t.fields {
		l = max(l, field.level())
	}
	return l
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

func (t namedTupleType) uninstantiatedBody() SimpleType         { return t }
func (t namedTupleType) instantiate(*Fresher, level) SimpleType { return t }
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

func (t namedTupleType) children(bool) iter.Seq[SimpleType] {
	return func(yield func(SimpleType) bool) {
		for _, field := range t.fields {
			if !yield(field.Snd) {
				return
			}
		}
	}
}

var emptyRecord = recordType{}

type recordType struct {
	fields []util.Pair[ast.Var, fieldType]
	withProvenance
}

func (t recordType) uninstantiatedBody() SimpleType         { return t }
func (t recordType) instantiate(*Fresher, level) SimpleType { return t }
func (t recordType) level() level {
	l := level(0)
	for _, field := range t.fields {
		l = max(l, field.Snd.level())
	}
	return l
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

func (t arrayType) uninstantiatedBody() SimpleType         { return t }
func (t arrayType) instantiate(*Fresher, level) SimpleType { return t }
func (t arrayType) level() level                           { return t.innerT.level() }
func (t arrayType) String() string                         { return "Array<" + t.innerT.String() + ">" }
func (t arrayType) inner() SimpleType                      { return t.innerT }

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

func (p PolymorphicType) String() string {
	//TODO implement me
	panic("implement me")
}

func (p PolymorphicType) level() level { return p._level }

func (p PolymorphicType) children(includeBounds bool) iter.Seq[SimpleType] {
	//TODO implement me
	panic("implement me")
}

func (p PolymorphicType) prov() typeProvenance {
	return p.Body.prov()
}
func (p PolymorphicType) End() token.Pos {
	return p.Body.prov().Range.End()
}
func (p PolymorphicType) Pos() token.Pos {
	return p.Body.prov().Range.Pos()
}
func (p PolymorphicType) instantiate(fresher *Fresher, level level) SimpleType {
	return fresher.freshenAbove(level, p._level, p.Body)
}
func (p PolymorphicType) uninstantiatedBody() SimpleType { return p.Body }
func (p PolymorphicType) rigidify(fresher *Fresher, level level) SimpleType {
	return fresher.freshenAboveWithRigidify(level, p._level, p.Body, true)
}

func (p PolymorphicType) Hash() uint64 {
	// Use prime numbers for mixing
	const prime1 uint64 = 16777619
	const prime2 uint64 = 2166136261

	// Hash the body and level, excluding provenance
	bodyHash := p.Body.Hash()
	levelHash := uint64(p._level)

	// Combine hashes using FNV-like mixing
	hash := prime2
	hash = hash*prime1 ^ bodyHash
	hash = hash*prime1 ^ levelHash

	return hash
}

func (t arrayType) Hash() uint64 {
	return 2166136261*16777619 ^ t.innerT.Hash()
}
func (t funcType) Hash() uint64 {
	var hash uint64 = 2166136261
	for _, arg := range t.args {
		hash = hash*16777619 ^ arg.Hash()
	}
	hash = hash*16777619 ^ t.ret.Hash()
	return hash
}

func (t *typeVariable) Hash() uint64 {
	const prime1 uint64 = 31
	const prime2 uint64 = 7919

	// it is unclear to me if two type vars with the same ID but different
	// bounds are possible or can be compared, for now using the ID as hash
	//hash := prime2
	//hash = hash*prime1 ^ uint64(t.id)
	//hash = hash*prime1 ^ uint64(t.level_)
	//
	//for _, lb := range t.lowerBounds {
	//	hash = hash*prime1 ^ lb.Hash()
	//}
	//for _, ub := range t.upperBounds {
	//	hash = hash*prime1 ^ ub.Hash()
	//}

	return prime1 * prime2 * t.id
}

func (t classTag) Hash() uint64 {
	const prime1 uint64 = 1299709
	hasher := fnv.New64a()
	_, _ = hasher.Write([]byte(t.id.CanonicalSyntax()))
	return prime1 ^ hasher.Sum64()
}

func (t traitTag) Hash() uint64 {
	const prime1 uint64 = 104729
	hasher := fnv.New64a()
	_, _ = hasher.Write([]byte(t.id.CanonicalSyntax()))
	return prime1 ^ hasher.Sum64()
}

func (t tupleType) Hash() uint64 {
	const prime1 uint64 = 433
	const prime2 uint64 = 9973

	hash := prime2
	for _, elem := range t.fields {
		hash = hash*prime1 ^ elem.Hash()
	}

	return hash
}

func (t namedTupleType) Hash() uint64 {
	const prime1 uint64 = 10007
	const prime2 uint64 = 104729
	hasher := fnv.New64a()

	hash := prime2
	for _, elem := range t.fields {
		hash = hash*prime1 ^ elem.Snd.Hash()
		_, _ = hasher.Write([]byte(elem.Fst.Name))
	}

	return hash * hasher.Sum64()
}

func (t recordType) Hash() uint64 {
	const prime1 uint64 = 15487469
	const prime2 uint64 = 32452843

	hasher := fnv.New64a()
	hash := prime2
	for _, field := range t.fields {
		hash = hash*prime1 ^ field.Snd.lowerBound.Hash()
		hash = hash*prime1 ^ field.Snd.upperBound.Hash()
		_, _ = hasher.Write([]byte(field.Fst.Name))
	}

	return hash * hasher.Sum64()
}
