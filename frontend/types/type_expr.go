package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/internal/log"
	"reflect"
)

var logger = ast.ExprLogger(log.DefaultLogger).With("section", "inference")

func (ctx *TypeCtx) TypeLetRecBody(name string, body ast.Expr) PolymorphicType {
	panic("TODO implement me")
}

// TypeExpr infers the type of a ast.Expr
//
// it is called typeTerm in the reference scala implementation
func (ctx *TypeCtx) TypeExpr(expr ast.Expr, vars map[typeName]SimpleType) (ret SimpleType) {
	logger := logger.With("expr.name", expr.ExprName(), "expr.string", expr, "expr.hash", fmt.Sprintf("%x", expr.Hash()))
	if cached, ok := ctx.cache.getCached(expr); ok && cached.at == ctx.level {
		logger.Debug("typeExpr: using cached type")
		return cached.t
	}

	logger.Debug("typeExpr: typing expression")
	defer func() {
		// for types not implemented, note ret might be nil at this stage
		logger.Debug("typeExpr: done typing expression", "result", ret, "bounds", boundsString(ret))

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
	case *ast.WhenMatch:
		return ctx.typeWhenMatch(expr, vars)
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
	case *ast.Ascribe:
		// empty Ascribe is meaningless in the AST, and the compiler should ideally never generate it,
		// but we can guard against it here easily because it is a no-op
		if expr.Type_ == nil {
			return ctx.TypeExpr(expr.Expr, vars)
		}
		bodyType := ctx.TypeExpr(expr.Expr, vars)
		processedType, _ := ctx.typeAstType(expr.Type_, vars, false, nil)
		ctx.constrain(bodyType, processedType, prov.provenance, constrainOnErr)
		return processedType

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

func (ctx *TypeCtx) typeWhenMatch(expr *ast.WhenMatch, vars map[typeName]SimpleType) (ret SimpleType) {
	subjectType := ctx.TypeExpr(expr.Value, vars)
	if len(expr.Cases) == 0 {
		ctx.addError(ilerr.New(ilerr.NewEmptyWhen{Positioner: expr}))
		return errorType()
	}
	subjectVarName := ""
	if asVar, ok := expr.Value.(*ast.Var); ok {
		subjectVarName = asVar.Name
	}
	var finalType SimpleType = bottomType
	var reqType SimpleType = bottomType
	for _, branch := range expr.Cases {
		branchT, resultT, tVar := ctx.typeWhenMatchBranch(subjectVarName, branch, vars)
		finalType = unionOf(finalType, resultT, unionOpts{prov: typeProvenance{Range: ast.RangeOf(branch.Value), desc: "when match branch"}})
		reqType = unionOf(intersectionOf(branchT, tVar, unionOpts{}), intersectionOf(reqType, negateType(branchT, emptyProv), unionOpts{}), unionOpts{})
	}
	return ctx.typeExprConstrain(subjectType, reqType, finalType, typeProvenance{Range: ast.RangeOf(expr), desc: "when match subject type"})
}

func (ctx *TypeCtx) typeWhenMatchBranch(subject string, branch ast.WhenCase, vars map[typeName]SimpleType) (branchT SimpleType, resultT SimpleType, tVar SimpleType) {
	tVar = topType
	pattern, ok := branch.Pattern.(*ast.MatchTypePattern)
	if !ok {
		ctx.addFailure(fmt.Sprintf("non-type pattern not implemented: %T", branch.Pattern), branch.Pattern)
		return errorType(), errorType(), tVar
	}
	tVars := make([]typeVariable, 0)
	if isWildcard := Equal(pattern.Type_, &ast.TypeName{Name: "_"}); isWildcard {
		branchT = topType
	} else {
		ctx.inPattern = true
		branchT, tVars = ctx.typeAstType(pattern.Type_, vars, false, nil)
		ctx.inPattern = false
		if len(tVars) != 0 {
			ctx.addFailure(fmt.Sprintf("type pattern not supported yet: %T", branch.Pattern), branch.Pattern)
			return errorType(), errorType(), tVar
		}
	}
	branchT = makeProxy(branchT, typeProvenance{desc: "pattern", Range: ast.RangeOf(branch.Pattern)})
	newCtx := ctx.nest()
	// if there is a subject, do 'smart casting' by creating a new type variable for it, and constraining the branch type to it
	// so that it is present on the branch scope with the refined type of the pattern
	if subject != "" {
		tVar = newCtx.newTypeVariable(typeProvenance{Range: ast.RangeOf(branch.Pattern), desc: "refined pattern match subject"}, subject, nil, nil)
		newCtx.env[subject] = tVar
	}
	resultT = newCtx.TypeExpr(branch.Value, vars)
	return branchT, resultT, tVar
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
