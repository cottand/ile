package types

import "github.com/cottand/ile/frontend/ir"

// LowerTypes eagerly resolves ir.Type for every sub-expression in root,
// populating loweredTypes so that TypeOf becomes a simple map lookup.
func (ctx *TypeCtx) LowerTypes(root ir.Expr) {
	ctx.loweredTypes = make(map[exprCacheEntry]ir.Type)
	ir.Walk(root, func(expr ir.Expr) {
		key := exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}
		cached, ok := ctx.cache.getCached(expr)
		if !ok {
			return
		}
		instantiated := cached.t.instantiate(ctx.fresher, cached.at)
		typ := ctx.GetAstTypeFor(instantiated, cached.pol)
		ctx.loweredTypes[key] = typ
	})
}
