package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"iter"
	"log/slog"
	"maps"
	"runtime/debug"
	"slices"
	"strings"
)

type typeError struct {
	message string
	// Positioner may be nil
	ir.Positioner
	stack []byte
}

func (err typeError) String() string {
	stack := strings.Split(string(err.stack), "\n")[6]
	return fmt.Sprintf("( %s ): %s", strings.TrimSpace(stack), err.message)
}
func (err typeError) Error() string {
	return err.String()
}

// TypeCtx holds mutable state during the inference process, as well as settings
type TypeCtx struct {
	parent   *TypeCtx // can be nil
	env      map[string]typeInfo
	typeDefs map[string]TypeDefinition
	// methodEnv TODO
	level     level
	inPattern bool

	// variancesStore is called variancesStore in the scala reference
	// should be accessed via variancesForTypeDef
	variancesStore map[TypeVarID]varianceInfo

	//typeDefs  map[types.Type]typeDef

	// here to avoid passing a position on every function call.
	// not present in scala implementation
	currentPos ir.Positioner

	// logger should have the current expression as an attribute (when there is one)
	logger *slog.Logger

	*TypeState
}

type nodesToSimpletypeCache map[exprCacheEntry]nodeCacheEntry
type nodeCacheEntry struct {
	t  SimpleType
	at level
}

type exprCacheEntry struct {
	r        ir.Range
	exprHash uint64
}

func (ctx *TypeCtx) putCache(expr ir.Expr, typ SimpleType) {
	ctx.cache[exprCacheEntry{
		exprHash: expr.Hash(),
		r:        ir.RangeOf(expr),
	}] = nodeCacheEntry{t: typ, at: ctx.level}
}

func (n nodesToSimpletypeCache) getCached(expr ir.Expr) (nodeCacheEntry, bool) {
	st, ok := n[exprCacheEntry{
		exprHash: expr.Hash(),
		r:        ir.RangeOf(expr),
	}]
	return st, ok
}

type expandedTypeCache = map[exprCacheEntry]ir.Type

// TypeState is part of TypeCtx and is shared across all copies of it during a single inference.
// It is not concurrency safe and is not present on the scala implementation
type TypeState struct {
	// fresher keeps track of new type variables
	// it is not present in the scala implementation,
	// there the same functionality is implemented by keeping global state
	fresher *Fresher

	// Failures are irrecoverable unexpected scenarios
	// that a normal program should never hit
	Failures []error
	// Errors are language problems that a malformed program could cause
	Errors []ilerr.IleError

	// cache keeps hashes of ast elements to already-simplified ast.Type
	cache             nodesToSimpletypeCache
	expandedTypeCache expandedTypeCache

	dontRecordProvenance bool
}

// NewEmptyTypeCtx should be the entry point to get a TypeCtx, but not how you
// produce a TypeCtx from another one. For that use copy
//
// TypeState is shared across nested levels of TypeCtx
func NewEmptyTypeCtx() *TypeCtx {
	defs := make(map[string]TypeDefinition, len(builtinTypes))
	for _, def := range builtinTypes {
		defs[def.name] = def
	}
	fresher := NewFresher()
	return &TypeCtx{
		parent:    nil,
		env:       fresher.universeEnv(),
		level:     0,
		inPattern: false,
		typeDefs:  defs,
		TypeState: &TypeState{
			fresher:           fresher,
			cache:             make(map[exprCacheEntry]nodeCacheEntry, 1),
			expandedTypeCache: make(map[exprCacheEntry]ir.Type, 1),
		},
		logger: slog.New(ir.IleAstHandler(slog.Default().Handler())).With("section", "inference"),
	}
}

// WithBindings returns a new TypeCtx with the given bindings added to the current context,
// which has the receiver TypeCtx as its parent
func (ctx *TypeCtx) WithBindings(bindings map[string]ir.Type) *TypeCtx {
	newCtx := ctx.nest()
	for name, type_ := range bindings {
		st, typeVars := newCtx.typeIrType(type_, nil, false, nil)
		if len(typeVars) > 0 {
			newCtx.addFailure("type variables inside external types not supported yet", nil)
		}
		newCtx.env[name] = st
	}
	return newCtx
}

func (ctx *TypeCtx) nest() *TypeCtx {
	copied := ctx.copy()
	copied.parent = ctx
	copied.env = make(map[string]typeInfo, len(ctx.env))
	return copied
}
func (ctx *TypeCtx) copy() *TypeCtx {
	copied := *ctx
	return &copied
}

// ctxCache stores a map of pairs of types' hashes to whether they are subtypes
type ctxCache map[uint64]bool

func (c ctxCache) get(l, r SimpleType) (sub bool, ok bool) {
	sub, ok = c[l.Hash()*r.Hash()]
	return sub, ok
}
func (c ctxCache) put(l, r SimpleType, sub bool) {
	c[l.Hash()*r.Hash()] = sub
}
func (c ctxCache) contains(l, t SimpleType) bool {
	_, ok := c[l.Hash()*t.Hash()]
	return ok
}

// assuming evaluates body assuming all entries in cache are true
func (c ctxCache) assuming(body func(ctxCache) bool) bool {
	newCache := make(ctxCache, len(c))
	for k, _ := range c {
		newCache[k] = true
	}
	return body(newCache)
}

// assuming lets you use a cache assuming all entries are true
func (c ctxCache) assume() ctxCache {
	newCache := make(ctxCache, len(c))
	for k, _ := range c {
		newCache[k] = true
	}
	return newCache
}

func (ctx *TypeCtx) get(name string) (t typeInfo, ok bool) {
	t, ok = ctx.env[name]
	if ok {
		return t, true
	}
	if ctx.parent != nil {
		t, ok = ctx.parent.get(name)
	}
	return t, ok
}

// TypesEquivalent carries the notation >:< in the scala implementation
func (ctx *TypeCtx) TypesEquivalent(this, that SimpleType) bool {
	return Equal(this, that) || ctx.isSubtype(this, that, nil) && ctx.isSubtype(that, this, nil)
}

func (ctx *TypeCtx) isSubtypeField(this, that fieldType, cache ctxCache) bool {
	return ctx.isSubtype(that.lowerBound, this.lowerBound, cache) && ctx.isSubtype(this.upperBound, that.upperBound, cache)
}

// isSubtype carries the notation <:< in the scala implementation
func (ctx *TypeCtx) isSubtype(this, that SimpleType, cache ctxCache) bool {
	this, that = unwrapProvenance(this), unwrapProvenance(that)
	if Equal(this, that) {
		return true
	}
	if cache == nil {
		cache = make(ctxCache)
	}
	// class tags
	{
		this, okThis := this.(classTag)
		that, okThat := that.(classTag)
		if okThis && okThat {
			return this.id == that.id || this.containsParentST(that.id)
		}
		if okThis || okThat {
			return false
		}
	}
	// functypes
	{
		this, okThis := this.(funcType)
		that, okThat := that.(funcType)
		if okThis && okThat {
			cache := cache.assume()
			if len(this.args) != len(that.args) {
				return false
			}
			for i, arg := range that.args {
				// TODO args are inverted here in ref impl - is it a bug?
				if !ctx.isSubtype(arg, this.args[i], cache) {
					return false
				}
			}
			return ctx.isSubtype(this.ret, that.ret, cache)
		}
		if okThis || okThat {
			return false
		}
	}
	// intersections, unions
	{
		if thisUnion, okThis := this.(unionType); okThis {
			//( A | B <:< C) => (A <:< C and B <:< C)
			return ctx.isSubtype(thisUnion.lhs, that, cache) && ctx.isSubtype(thisUnion.rhs, that, cache)
		}
		if thatUnion, okThat := that.(unionType); okThat {
			// (A <:< B | C) => (A <:< B and A <:< C)
			return ctx.isSubtype(this, thatUnion.lhs, cache) && ctx.isSubtype(this, thatUnion.rhs, cache)
		}
		if thisIntersection, okThis := this.(intersectionType); okThis {
			// (A & B <:< C) => (A <:< C or B <:< C)
			return ctx.isSubtype(thisIntersection.lhs, that, cache) || ctx.isSubtype(thisIntersection.rhs, that, cache)
		}
		if thatIntersection, okThat := that.(intersectionType); okThat {
			// (A <:< B & C) => (A <:< B or A <:< C)
			return ctx.isSubtype(this, thatIntersection.lhs, cache) || ctx.isSubtype(this, thatIntersection.rhs, cache)
		}
	}
	// records
	{
		thisRecord, okThis := this.(recordType)
		if okThis && len(thisRecord.fields) == 0 {
			return ctx.isSubtype(topType, that, cache)
		}
		thatRecord, okThat := that.(recordType)
		if okThat && len(thatRecord.fields) == 0 {
			return ctx.isSubtype(this, topType, cache)
		}
		// this subtype of that if for each field in that, there is a matching subtype field in this
		if okThis && okThat {
			cache := cache.assume()
			for _, thatField := range thatRecord.fields {
				thisHasSameField := slices.ContainsFunc(thisRecord.fields, func(f recordField) bool {
					return f.name.Name == thatField.name.Name && ctx.isSubtypeField(f.type_, thatField.type_, cache)
				})
				if !thisHasSameField {
					return false
				}
			}
			return true
		}
	}
	// type variables
	{
		thisTV, okThis := this.(*typeVariable)
		thatTV, okThat := that.(*typeVariable)
		if okThis || okThat {
			sub, ok := cache.get(this, that)
			if ok {
				return sub
			}
		}
		if okThis {
			cache.put(this, that, false)
			tmp := slices.ContainsFunc(thisTV.upperBounds, func(t SimpleType) bool {
				return ctx.isSubtype(t, that, cache)
			})
			if tmp {
				cache.put(this, that, true)
			}
			return tmp
		}
		if okThat {
			cache.put(this, that, false)
			tmp := slices.ContainsFunc(thatTV.lowerBounds, func(t SimpleType) bool {
				return ctx.isSubtype(this, t, cache)
			})
			if tmp {
				cache.put(this, that, true)
			}
			return tmp
		}
	}
	// extremes
	{
		if Equal(that, topType) || Equal(this, bottomType) {
			return true
		}
		if Equal(that, bottomType) || Equal(this, topType) {
			return false
		}
	}
	// negation types
	{
		thatNeg, okThat := that.(negType)
		if okThat {
			// A <:< ~B => A & B <:< Bot
			return ctx.isSubtype(intersectionOf(this, thatNeg.negated, unionOpts{}), bottomType, cache)
		}
		thisNeg, okThis := this.(negType)
		if okThis {
			// ~A <:< B => Top <:< A | B
			return ctx.isSubtype(topType, unionOf(thisNeg.negated, that, unionOpts{}), cache)
		}
	}
	// type refs
	{
		thisRef, okThis := this.(typeRef)
		if okThis {
			typeDef, ok := ctx.typeDefs[thisRef.defName]
			if ok && typeDef.defKind == ir.KindClass {
				tag, ok := ctx.classTagFrom(thisRef)
				return ok && ctx.isSubtype(tag, that, cache)
			}
			if ok && typeDef.defKind == ir.KindAlias {
				// this case is not handled in the reference implementation,
				// but intuition tells me there can be a type definition where
				// a ref points to a simple alias? Maybe this is caught in a different path?
				// Just logging for now to assess how frequently it really happens
				//ctx.logger.Warn("isSubtype seems to be undefined for aliases", "typeDef", typeDef.name, "thisRef", thisRef.defName)
			}
			return false
		}
		_, okThat := that.(typeRef)
		if okThat {
			return false
		}
	}
	ctx.addFailure(fmt.Sprintf("isSubtype not implemented for: %s: %T and %s: %T", this, this, that, that), this.prov())
	return false
}

func (ctx *TypeCtx) ProcessTypeDefs(newDefs []ir.TypeDefinition) *TypeCtx {
	clonedEnv := maps.Clone(ctx.env)
	allDefs := ctx.typeDefs
	defsInfo := make(map[typeName]util.Pair[ir.TypeDefKind, int], len(newDefs))
	for _, def := range newDefs {
		defsInfo[def.Name.Name] = util.NewPair(def.Kind, len(def.TypeParams))
	}

	preprocessedDefs := make([]TypeDefinition, 0, len(defsInfo))
	for _, td0 := range newDefs {
		// reference implementation performs a capitalization check here which we skip,
		// as for us that has a public/private meaning
		name := td0.Name.Name
		if isNameReserved(name) {
			ctx.addError(ilerr.New(ilerr.NewRestrictedIdentName{
				Positioner: ctx.currentPos,
				Name:       name,
			}))
		}
		if existing, ok := allDefs[name]; ok {
			ctx.addError(ilerr.New(ilerr.NewNameRedeclaration{
				Positioner: ctx.currentPos,
				Name:       name,
				Other:      existing.from,
			}))
		}
		// check for duplicate type args
		seen := make(map[typeName]ir.Positioner)
		for _, arg := range td0.TypeParams {
			if existing, ok := seen[arg.Name]; ok {
				ctx.addError(ilerr.NewNameRedeclaration{
					Positioner: ctx.currentPos,
					Name:       arg.Name,
					Other:      existing,
				})
			}
			seen[arg.Name] = arg.Range
		}

		// here, there reference implementation uses a lazy zip - we just for loops
		typeParamsArgsMap := make(map[string]SimpleType, len(td0.TypeParams))
		for _, arg := range td0.TypeParams {
			fresh := ctx.fresher.newTypeVariable(ctx.level+1, newOriginProv(arg.Range, td0.Kind.String()+" type parameter", arg.Name), arg.Name, nil, nil)
			// invariant: all types in argTypes should be of type variable
			typeParamsArgsMap[arg.Name] = fresh
		}
		bodyType, typeVars := ctx.typeIrType(td0.Body, typeParamsArgsMap, false, defsInfo)
		baseClasses := baseClassesOfDef(td0)
		var td1 TypeDefinition
		typeParamsArgsAsSlice := make([]util.Pair[typeName, *typeVariable], 0, len(typeParamsArgsMap))
		for argName, argType := range typeParamsArgsMap {
			argType, ok := argType.(*typeVariable)
			if !ok {
				panic("all types in argTypes should be of type variable")
			}
			td1.typeParamArgs = append(typeParamsArgsAsSlice, util.Pair[string, *typeVariable]{
				Fst: argName,
				Snd: argType,
			})
		}
		if (td0.Kind == ir.KindClass || td0.Kind == ir.KindTrait) && baseClasses.Size() == 0 {
			td1 = TypeDefinition{
				defKind:       td0.Kind,
				name:          td0.Name.Name,
				typeParamArgs: typeParamsArgsAsSlice,
				typeVars:      typeVars,
				bodyType: intersectionType{lhs: anyClassTag, rhs: bodyType, withProvenance: withProvenance{typeProvenance{
					desc:       "intersection type",
					originName: "",
					isType:     true,
				}}},
				// this is Object in the reference implementation
				baseClasses: set.From([]typeName{ir.AnyTypeName}),
			}
		} else {
			td1 = TypeDefinition{
				defKind:       td0.Kind,
				name:          td0.Name.Name,
				typeParamArgs: typeParamsArgsAsSlice,
				typeVars:      typeVars,
				bodyType:      bodyType,
				baseClasses:   baseClasses,
				from:          td0.Positioner,
			}
		}
		allDefs[name] = td1
		preprocessedDefs = append(preprocessedDefs, td1)
	}
	ctxCopy := &(*ctx)
	ctxCopy.env = clonedEnv
	ctxCopy.typeDefs = allDefs

	oldDefs := ctx.typeDefs

	return ctxCopy.typeTypeDefs(preprocessedDefs, oldDefs)
}

// def baseClassesOf(tyd: mlscript.TypeDef): Set[TypeName] =
// if (tyd.kind === Als) Set.empty else baseClassesOf(tyd.body)
func baseClassesOfDef(definition ir.TypeDefinition) set.Collection[typeName] {
	if definition.Kind == ir.KindAlias {
		return set.New[string](0)
	}
	return baseClassesOfType(definition.Body)

}

func baseClassesOfType(typ ir.Type) set.Collection[typeName] {
	switch typ := typ.(type) {
	case *ir.IntersectionType:
		leftClasses := baseClassesOfType(typ.Left)
		rightClasses := baseClassesOfType(typ.Right)
		rightClasses.InsertSet(leftClasses)
		return rightClasses
	case *ir.TypeName:
		return set.From([]typeName{typ.Name})
	case *ir.AppliedType:
		return baseClassesOfType(&(typ.Base))
		// including  *ast.Record, *ast.UnionType:
	default:
		return set.New[string](0)
	}
}

// ComputeVariances Finds the variances of all type variables in the given type definitions with the given
// context using a fixed point computation. The algorithm starts with each type variable
// as bivariant by default and each type definition position as covariant and
// then keeps updating the position variance based on the types it encounters.
//
// It uses the results to update variance info in the type definitions
func (ctx *TypeCtx) ComputeVariances([]TypeDefinition) {
	panic("TODO implement me")
}

func (ctx *TypeState) addFailure(message string, pos ir.Positioner) {
	logger.Error("failure during inference", "message", message)
	ctx.Failures = append(ctx.Failures, typeError{message: message, Positioner: pos, stack: debug.Stack()})
}

func (ctx *TypeState) addError(ileError ilerr.IleError) {
	logger.Warn("error during inference", "message", ileError.Error(), "at", ilerr.FormatWithCode(ileError))
	ctx.Errors = append(ctx.Errors, ileError)
}

// TypeOf must be called after running inference
func (ctx *TypeCtx) TypeOf(expr ir.Expr) (ret ir.Type) {
	logger := ctx.logger.With("section", "typeof", "expr", expr)
	defer func() {
		if len(ctx.Failures) == 0 {
			logger.Info("resolved type post-inference", "type", ret)
		}
	}()
	// check if simplified before
	expanded, ok := ctx.expandedTypeCache[exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}]
	if ok {
		return expanded
	}

	// check if traversed before
	typeScheme, ok := ctx.cache.getCached(expr)
	if !ok {
		logger.Error("TypeState: tried to access type for unknown AST node expression")
		return &ir.NothingType{Positioner: expr}
	}
	instantiated := typeScheme.t.instantiate(ctx.fresher, typeScheme.at)
	typ := ctx.GetAstTypeFor(instantiated)
	// save for later so we do not need to go through simplification and expansion again
	ctx.expandedTypeCache[exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}] = typ
	return typ
}

func (ctx *TypeCtx) nextLevel() *TypeCtx {
	copied := *ctx
	copied.level++
	return &copied
}

func (ctx *TypeCtx) newTypeVariable(prov typeProvenance, nameHint string, lowerBounds, upperBounds []SimpleType) *typeVariable {
	return ctx.fresher.newTypeVariable(ctx.level, prov, nameHint, lowerBounds, upperBounds)
}

// variancesForTypeDef corresponds to getTypeDefinitionVariances in the scala reference implementation
func (ctx *TypeCtx) variancesForTypeDef(defName string, id TypeVarID) varianceInfo {
	_, ok := ctx.typeDefs[defName]
	if !ok {
		return varianceInvariant
	}
	variance, ok := ctx.variancesStore[id]
	if !ok {
		return varianceInvariant
	}
	return variance
}

// getTypeDefinitionVariances retrieves variance information for type parameters.
func (ctx *TypeCtx) getTypeDefinitionVariances(name typeName) ([]Variance, bool) {
	logger.Warn("getTypeDefinitionVariances not implemented", "type", name)
	// Placeholder: Assume invariant for now
	def, ok := ctx.typeDefs[name] // Assuming tyDefs stores this info
	if !ok {
		return nil, false
	}
	variances := make([]Variance, len(def.typeParamArgs))
	for i := range variances {
		variances[i] = Invariant // Default to invariant
		// TODO: Read actual variance from TypeDef
	}
	return variances, true
}

func (ctx *TypeCtx) baseClassesOf(names ...string) iter.Seq[string] {
	return func(yield func(string) bool) {
		for _, name := range names {
			def, ok := ctx.typeDefs[name]
			if !ok {
				continue
			}
			for base := range def.baseClasses.Items() {
				if !yield(base) {
					return
				}
			}
		}
	}
}

func (ctx *TypeCtx) classTagFrom(name typeRef) (c classTag, ok bool) {
	def, ok := ctx.typeDefs[name.defName]
	if !ok || def.defKind != ir.KindClass {
		return c, false
	}
	return classTag{
		id:      &ir.Var{Name: name.defName},
		parents: util.SetFromSeq(ctx.baseClassesOf(name.defName), 2),
	}, true
}
