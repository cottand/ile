package types

import "github.com/cottand/ile/frontend/ir"

// LowerTypes eagerly resolves ir.Type for every sub-expression in root,
// populating loweredTypes so that TypeOf becomes a simple map lookup.
//
// It uses two passes: the first simplifies all types normally, and the second
// re-expands sub-expressions inside generic function bodies using expandSimpleType
// with stopAtTyVars=true. This preserves type variables that would otherwise be
// simplified to Nothing when processed in isolation (since the simplifier loses
// co-occurrence context from the enclosing function type).
func (ctx *TypeCtx) LowerTypes(root ir.Expr) {
	ctx.loweredTypes = make(map[exprCacheEntry]ir.Type)

	var genericFuncBodies []ir.Expr
	ir.Walk(root, func(expr ir.Expr) {
		key := exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}
		cached, ok := ctx.cache.getCached(expr)
		if !ok {
			return
		}
		instantiated := cached.t.instantiate(ctx.fresher, cached.at)
		typ := ctx.GetAstTypeFor(instantiated, cached.pol)
		ctx.loweredTypes[key] = typ

		if fn, ok := expr.(*ir.Func); ok {
			if irTypeHasTypeVars(typ) {
				genericFuncBodies = append(genericFuncBodies, fn.Body)
			}
		}
	})

	for _, body := range genericFuncBodies {
		ir.Walk(body, func(expr ir.Expr) {
			if _, isFunc := expr.(*ir.Func); isFunc {
				return
			}
			key := exprCacheEntry{r: ir.RangeOf(expr), exprHash: expr.Hash()}
			cached, ok := ctx.cache.getCached(expr)
			if !ok {
				return
			}
			typ := ctx.expandSimpleType(cached.t, true)
			ctx.loweredTypes[key] = typ
		})
	}
}

func irTypeHasTypeVars(t ir.Type) bool {
	switch ty := t.(type) {
	case *ir.TypeVar:
		return true
	case *ir.FnType:
		for _, arg := range ty.Args {
			if arg != nil && irTypeHasTypeVars(arg) {
				return true
			}
		}
		return ty.Return != nil && irTypeHasTypeVars(ty.Return)
	case *ir.UnionType:
		return irTypeHasTypeVars(ty.Left) || irTypeHasTypeVars(ty.Right)
	case *ir.IntersectionType:
		return irTypeHasTypeVars(ty.Left) || irTypeHasTypeVars(ty.Right)
	case *ir.ConstrainedType:
		return irTypeHasTypeVars(ty.Base)
	case *ir.AppliedType:
		for _, arg := range ty.Args {
			if irTypeHasTypeVars(arg) {
				return true
			}
		}
		return false
	case *ir.ListType:
		return irTypeHasTypeVars(ty.ElementType)
	case *ir.ListLiteralType:
		for _, elem := range ty.ElementTypes {
			if irTypeHasTypeVars(elem) {
				return true
			}
		}
		return false
	case *ir.NegType:
		return irTypeHasTypeVars(ty.Underlying)
	case *ir.TypeBounds:
		return irTypeHasTypeVars(ty.Lower) || irTypeHasTypeVars(ty.Upper)
	default:
		return false
	}
}
