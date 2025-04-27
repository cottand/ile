package types

import (
	"fmt"
	"github.com/benbjohnson/immutable"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/util"
	"maps"
	"reflect"
	"runtime/debug"
	"slices"
	"strings"
)

type typeError struct {
	message string
	// Positioner may be nil
	ast.Positioner
	stack []byte
}

func (err typeError) String() string {
	stack := strings.Split(string(err.stack), "\n")[6]
	return fmt.Sprintf("( %s ): %s", strings.TrimSpace(stack), err.message)
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
	currentPos ast.Positioner

	*TypeState
}

type nodesToSimpletypeCache map[uint64]nodeCacheEntry
type nodeCacheEntry struct {
	t  SimpleType
	at level
}

func (ctx *TypeCtx) putCache(expr ast.Expr, typ SimpleType) {
	ctx.cache[expr.Hash()] = nodeCacheEntry{t: typ, at: ctx.level}
}

func (n nodesToSimpletypeCache) getCached(expr ast.Expr) (nodeCacheEntry, bool) {
	st, ok := n[expr.Hash()]
	return st, ok
}

type expandedTypeCache = map[uint64]ast.Type

// TypeState is part of TypeCtx and is shared across all copies of it during a single inference.
// It is not concurrency safe and is not present on the scala implementation
type TypeState struct {
	// fresher keeps track of new type variables
	// it is not present in the scala implementation,
	// there the same functionality is implemented by keeping global state
	fresher *Fresher

	// Failures are irrecoverable unexpected scenarios
	// that a normal program should never hit
	Failures []typeError
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
	return &TypeCtx{
		parent:    nil,
		env:       universe(),
		level:     0,
		inPattern: false,
		TypeState: &TypeState{
			fresher:           NewFresher(),
			cache:             make(map[uint64]nodeCacheEntry, 1),
			expandedTypeCache: make(map[uint64]ast.Type, 1),
		},
	}
}

func (ctx *TypeCtx) nest() *TypeCtx {
	copied := *ctx
	copied.parent = ctx
	copied.env = make(map[string]typeInfo, len(ctx.env))
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
func assuming[R any](cache ctxCache, body func(ctxCache) R) R {
	newCache := make(ctxCache, len(cache))
	for k, _ := range cache {
		newCache[k] = true
	}
	return body(newCache)
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
	return this.Equivalent(that) || ctx.SolveSubtype(this, that, nil) && ctx.SolveSubtype(that, this, nil)
}

// SolveSubtype carries the notation <:< in the scala implementation
func (ctx *TypeCtx) SolveSubtype(this, that SimpleType, cache ctxCache) bool {
	if this.Equivalent(that) {
		return true
	}
	if cache == nil {
		cache = make(ctxCache)
	}
	// functypes
	{
		this, okThis := this.(funcType)
		that, okThat := that.(funcType)
		if okThis && okThat {
			return assuming(cache, func(cache ctxCache) bool {
				if len(this.args) != len(that.args) {
					return false
				}
				for i, arg := range that.args {
					// TODO args are inverted here in ref impl - is it a bug?
					if !ctx.SolveSubtype(arg, this.args[i], cache) {
						return false
					}
				}
				return ctx.SolveSubtype(this.ret, that.ret, cache)
			})
		}
		if okThis || okThat {
			return false
		}
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
				return ctx.SolveSubtype(t, that, cache)
			})
			if tmp {
				cache.put(this, that, true)
			}
			return tmp
		}
		if okThat {
			cache.put(this, that, false)
			tmp := slices.ContainsFunc(thatTV.lowerBounds, func(t SimpleType) bool {
				return ctx.SolveSubtype(this, t, cache)
			})
			if tmp {
				cache.put(this, that, true)
			}
			return tmp
		}
	}
	panic("implement me for: " + reflect.TypeOf(this).String() + " and " + reflect.TypeOf(that).String())
}

func (ctx *TypeCtx) ProcessTypeDefs(newDefs []ast.TypeDefinition) *TypeCtx {
	clonedEnv := maps.Clone(ctx.env)
	allDefs := ctx.typeDefs
	defsInfo := make(map[typeName]util.Pair[ast.TypeDefKind, int], len(newDefs))
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
		seen := make(map[typeName]ast.Positioner)
		for _, arg := range td0.TypeParams {
			if existing, ok := seen[arg.Name]; ok {
				ctx.addError(ilerr.NewNameRedeclaration{
					Positioner: ctx.currentPos,
					Name:       arg.Name,
					Other:      existing,
				})
			}
			seen[arg.Name] = arg.Positioner
		}

		// here, there reference implementation uses a lazy zip - we just for loops
		typeParamsArgsMap := make(map[string]SimpleType, len(td0.TypeParams))
		for _, arg := range td0.TypeParams {
			fresh := ctx.fresher.newTypeVariable(ctx.level+1, newOriginProv(arg.Positioner, td0.Kind.String()+" type parameter", arg.Name), arg.Name, nil, nil)
			// invariant: all types in argTypes should be of type variable
			typeParamsArgsMap[arg.Name] = fresh
		}
		bodyType, typeVars := ctx.typeType2(td0.Body, false, typeParamsArgsMap, defsInfo)
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
		if (td0.Kind == ast.KindClass || td0.Kind == ast.KindTrait) && baseClasses.Len() == 0 {
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
				baseClasses: emptySetTypeName.Add(ast.AnyTypeName),
			}
		} else {
			td1 = TypeDefinition{
				defKind:       td0.Kind,
				name:          td0.Name.Name,
				typeParamArgs: typeParamsArgsAsSlice,
				typeVars:      typeVars,
				bodyType:      bodyType,
				baseClasses:   baseClasses.Immutable(immutable.NewHasher("")),
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
func baseClassesOfDef(definition ast.TypeDefinition) util.MSet[typeName] {
	if definition.Kind == ast.KindAlias {
		return util.NewEmptySet[string]()
	}
	return baseClassesOfType(definition.Body)

}

func baseClassesOfType(typ ast.Type) util.MSet[typeName] {
	switch typ := typ.(type) {
	case *ast.IntersectionType:
		leftClasses := baseClassesOfType(typ.Left)
		rightClasses := baseClassesOfType(typ.Right)
		for elem := range leftClasses.All() {
			rightClasses.Add(elem)
		}
		return rightClasses
	case *ast.TypeName:
		return util.NewSetOf(typ.Name)
	case *ast.AppliedType:
		return baseClassesOfType(&(typ.Base))
		// including  *ast.Record, *ast.UnionType:
	default:
		return util.NewEmptySet[string]()
	}
}

// wtf are these names
func (ctx *TypeCtx) typeType2(
	typ ast.Type,
	simplify bool,
	vars map[string]SimpleType,
	newDefsInfo map[string]util.Pair[ast.TypeDefKind, int],
) (SimpleType, []typeVariable) {
	panic("TODO implement me")
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

func (ctx *TypeState) addFailure(message string, pos ast.Positioner) {
	ctx.Failures = append(ctx.Failures, typeError{message: message, Positioner: pos, stack: debug.Stack()})
}

func (ctx *TypeState) addError(ileError ilerr.IleError) {
	ctx.Errors = append(ctx.Errors, ileError)
}

// TypeOf must be called after running inference
func (ctx *TypeCtx) TypeOf(expr ast.Expr) ast.Type {
	// check if simplified before
	expanded, ok := ctx.expandedTypeCache[expr.Hash()]
	if ok {
		return expanded
	}

	// check if traversed before
	typeScheme, ok := ctx.cache.getCached(expr)
	if !ok {
		logger.Warn("TypeState: tried to access type for unknown AST node", "expr", expr.ExprName())
		return &ast.NothingType{Positioner: expr}
	}
	instantiated := typeScheme.t.instantiate(ctx.fresher, typeScheme.at)
	typ := ctx.GetAstTypeFor(instantiated)
	// save in map
	ctx.expandedTypeCache[expr.Hash()] = typ
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
	fmt.Printf("WARN: getTypeDefinitionVariances not implemented for %s\n", name)
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
