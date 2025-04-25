package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"reflect"
)

var logger = log.DefaultLogger.With("section", "inference")

func (ctx *TypeCtx) TypeLetBody(body ast.Expr, vars map[TypeVarID]SimpleType) PolymorphicType {
	res := ctx.nextLevel().TypeExpr(body, vars)
	return PolymorphicType{
		_level: ctx.level,
		Body:   res,
	}
}

func (ctx *TypeCtx) TypeLetRecBody(name string, body ast.Expr) PolymorphicType {
	panic("TODO implement me")
}

// TypeExpr infers the type of a ast.Expr
//
// it is called typeTerm in the reference scala implementation
func (ctx *TypeCtx) TypeExpr(expr ast.Expr, vars map[TypeVarID]SimpleType) (ret SimpleType) {
	if cached, ok := ctx.cache.getCached(expr); ok && cached.at == ctx.level {
		return cached.t
	}

	logger.Debug("typing expression", "expr", expr.ExprName())
	defer func() {
		logger.Debug("done typing expression", "expr", expr.ExprName(), "result", ret)

		ctx.putCache(expr, ret)
	}()
	ctx.currentPos = expr
	prov := withProvenance{provenance: typeProvenance{Range: ast.RangeOf(expr)}}
	switch expr := expr.(type) {
	case *ast.Var:
		known, ok := ctx.get(expr.Name)
		if !ok {
			ctx.addError(ilerr.New(ilerr.NewUndefinedVariable{
				Positioner: expr,
				Name:       expr.Name,
			}))
			known = errorType()
		}
		instance := known.instantiate(ctx.fresher, ctx.level)
		return makeProxy(instance, prov.prov())
	case *ast.Literal:
		return classTag{
			id:             expr,
			parents:        expr.BaseTypes(),
			withProvenance: prov,
		}
		// the reference implementation has two matches here: one without Var (param names) and another with
		// we will focus on the former for now
	case *ast.Call:
		fType := ctx.TypeExpr(expr.Func, vars)
		argTypes := make([]SimpleType, 0, len(expr.Args))
		for _, arg := range expr.Args {
			argTypes = append(argTypes, ctx.TypeExpr(arg, vars))
		}
		res := ctx.newTypeVariable(prov.provenance, "", nil, nil)
		return ctx.typeExprConstrain(fType, funcType{
			args:           argTypes,
			ret:            res,
			withProvenance: prov,
		}, res, prov.provenance)
	case *ast.ListLiteral:
		tupleT := tupleType{
			fields:         make([]SimpleType, 0, len(expr.Args)),
			withProvenance: withProvenance{provenance: newOriginProv(expr, "list literal", "")},
		}
		for _, tupleElement := range expr.Args {
			typed := ctx.TypeExpr(tupleElement, vars)
			tupleT.fields = append(tupleT.fields, typed)
		}
		return tupleT
	default:
		panic("not implemented: unexpected expression type for " + reflect.TypeOf(expr).String())
	}
}

func (ctx *TypeCtx) typeExprConstrain(lhs, rhs, res SimpleType, prov typeProvenance) SimpleType {
	errCount := 0
	ctx.constrain(lhs, rhs, prov, func(err ilerr.IleError) (terminateEarly bool) {
		errCount++
		if errCount == 1 {
			// so that we can get error types leak into the result
			ctx.addError(err)
			ctx.constrain(errorType(), res, prov, func(err ilerr.IleError) (terminateEarly bool) { return false })
			return false
		} else if errCount < 3 {
			// silence further Errors
			return false
		} else {
			// stop constraining stack this point in order to avoid explosive badly behaved error cases
			return true
		}
	})
	return res
}

func (ctx *TypeCtx) GetLowerBoundFunctionType(t SimpleType) []funcType {
	switch t := unwrapProvenance(t).(type) {
	case PolymorphicType:
		//if ctx.typeIsAliasOf(t) {
		// see reference impl for getLowerBoundFunctionType
		panic("TODO implement type refs unapply")
	case funcType:
		return []funcType{t}
	case *typeVariable:
		var funcTypes = make([]funcType, 0)
		for _, lowerBound := range t.lowerBounds {
			for _, lBoundFuncType := range ctx.GetLowerBoundFunctionType(lowerBound) {
				funcTypes = append(funcTypes, lBoundFuncType)
			}
		}
		return funcTypes
	case intersectionType:
		var funcTypes = make([]funcType, 0)
		funcTypes = append(funcTypes, ctx.GetLowerBoundFunctionType(t.lhs)...)
		funcTypes = append(funcTypes, ctx.GetLowerBoundFunctionType(t.rhs)...)
		return funcTypes
	default:
		return make([]funcType, 0)
	}
}
