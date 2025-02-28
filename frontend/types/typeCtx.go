package types

import (
	"github.com/cottand/ile/util"
	"reflect"
)

// universe are the built-in bindings
func universe() map[string]typeInfo {
	m := make(map[string]typeInfo)
	return m
}

type TypeCtx struct {
	parent   *TypeCtx // can be nil
	env      map[string]typeInfo
	typeDefs map[string]typeDefinition
	// methodEnv TODO
	level     int
	inPattern bool

	//typeDefs  map[types.Type]typeDef
}

func NewEmptyTypeCtx() *TypeCtx {
	return &TypeCtx{
		parent:    nil,
		env:       universe(),
		level:     0,
		inPattern: false,
	}
}

type ctxCache = map[util.Pair[simpleType, simpleType]]bool

// assuming evaluates body assuming all entries in cache are true
func assuming[R any](cache ctxCache, body func(ctxCache) R) R {
	newCache := make(ctxCache, len(cache))
	for k, _ := range cache {
		newCache[k] = true
	}
	return body(newCache)
}

// TypesEquivalent carries the notation >:< in the scala implementation
func (ctx *TypeCtx) TypesEquivalent(this, that simpleType) bool {
	return this == that || ctx.SolveSubtype(this, that, nil) && ctx.SolveSubtype(that, this, nil)
}

// SolveSubtype carries the notation <:< in the scala implementation
func (ctx *TypeCtx) SolveSubtype(this, that simpleType, cache ctxCache) bool {
	if this == that {
		return true
	}
	if cache == nil {
		cache = make(ctxCache)
	}

	switch this.(type) {
	default:
		panic("implement me for" + reflect.TypeOf(this).String())

	}
}
