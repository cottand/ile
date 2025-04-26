package types

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"reflect"
)

var logger = log.DefaultLogger.With("section", "inference")

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
		// for types not implemented, note ret might be nil at this stage
		logger.Debug("done typing expression", "expr", expr.ExprName(), "result", ret)

		ctx.putCache(expr, ret)
	}()

	constrainOnErr := func(err ilerr.IleError) (terminateEarly bool) {
		if err != nil {
			ctx.addError(err)
		}
		return false
	}

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
	case *ast.Assign:
		var valueResult SimpleType
		if !expr.Recursive {
			valueResult = ctx.nextLevel().TypeExpr(expr.Value, vars)
		} else {
			nameVar := ctx.fresher.newTypeVariable(ctx.level+1, typeProvenance{}, expr.Var, nil, nil)
			ctx := ctx.nest()
			ctx.env[expr.Var] = nameVar
			bodyType := ctx.nextLevel().TypeExpr(expr.Value, vars)
			ctx.constrain(bodyType, nameVar, newOriginProv(expr.Value, "binding of "+expr.Describe(), ""), constrainOnErr)
			valueResult = nameVar
		}
		typeScheme := PolymorphicType{
			_level: ctx.level,
			Body:   valueResult,
		}
		ctx := ctx.nest()
		ctx.env[expr.Var] = typeScheme
		return ctx.TypeExpr(expr.Body, vars)
	case *ast.LetGroup:
		bindingVars := make([]*typeVariable, len(expr.Vars))
		for i, binding := range expr.Vars {
			bindingVars[i] = ctx.fresher.newTypeVariable(ctx.level+1, typeProvenance{}, binding.Var, nil, nil)
			ctx.env[binding.Var] = bindingVars[i]
		}
		for i, binding := range expr.Vars {
			typed := ctx.TypeExpr(binding.Value, vars)
			bindingProv := newOriginProv(binding.Value, "binding of "+binding.Value.Describe(), "")
			ctx.constrain(typed, bindingVars[i], bindingProv, constrainOnErr)
		}

		nested := ctx.nest()
		for i, binding := range expr.Vars {
			typeScheme := PolymorphicType{
				_level: ctx.level,
				Body:   bindingVars[i],
			}
			nested.env[binding.Var] = typeScheme
		}
		return nested.TypeExpr(expr.Body, vars)

	case *ast.Unused:
		_ = ctx.nextLevel().TypeExpr(expr.Value, vars)
		return ctx.TypeExpr(expr.Body, vars)

	case *ast.Func:
		nested := ctx.nest()
		argTypes := make([]SimpleType, 0, len(expr.ArgNames))
		for _, arg := range expr.ArgNames {
			//val res = new TypeVariable(lvl, Nil, Nil, N, Option.when(dbg)(nme))(prov)
			//v.uid = S(nextUid)
			//ctx += nme -> VarSymbol(res, v)
			//res
			// scala reference allows using a pattern as a function argument, but we only support normal variables
			argType := ctx.newTypeVariable(newOriginProv(expr, "function parameter", ""), "", nil, nil)
			nested.env[arg] = argType
			argTypes = append(argTypes, argType)
		}
		bodyType := nested.TypeExpr(expr.Body, vars)
		return funcType{
			args:           argTypes,
			ret:            bodyType,
			withProvenance: withProvenance{newOriginProv(expr, expr.Describe(), "")},
		}
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
