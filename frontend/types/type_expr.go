package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"log/slog"
	"reflect"
)

var logger = slog.New(ir.IleIRSlogHandler(slog.Default().Handler())).With("section", "inference")

type exprTyper struct {
	*TypeCtx
	*slog.Logger
}

func (ctx *exprTyper) typeExpr(expr ir.Expr, vars map[typeName]SimpleType) (ret SimpleType) {
	ctx.Logger = ctx.TypeCtx.logger.With("expr.name", expr.ExprName(), "expr", expr)

	if cached, ok := ctx.cache.getCached(expr); ok && cached.at == ctx.level {
		// also check for the range so different expressions at the same level don't collide
		//  TODO ideally we store not just level info, but also a hash of scope of the variable,
		//    so that two variables at the same level in different scopes do not hit this case
		//    (but two variables in the same scope at the same level do get a cache hit)
		ctx.logger.Debug("typeExpr: using cached type", "cached", cached.t)
		return cached.t
	}

	ctx.Debug("typeExpr: typing expression")
	defer func() {
		// for types not implemented, note ret might be nil at this stage
		ctx.Debug("typeExpr: done typing expression", "result", ret, "bounds", boundsString(ret))

		ctx.putCache(expr, ret)
	}()

	constrainOnErr := func(err ilerr.IleError) (terminateEarly bool) {
		if err != nil {
			ctx.addError(err)
		}
		return false
	}

	ctx.currentPos = expr
	prov := withProvenance{provenance: typeProvenance{Range: ir.RangeOf(expr)}}
	switch expr := expr.(type) {
	case *ir.Var:
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
	case *ir.WhenMatch:
		return ctx.typeWhenMatch(expr, vars)
	case *ir.Literal:
		return classTag{
			id:      expr,
			parents: expr.BaseTypes(),
			withProvenance: typeProvenance{
				Range: prov.provenance.Range,
				desc:  "literal value",
			}.embed(),
		}
		// the reference implementation has two matches here: one without Var (param names) and another with
		// we will focus on the former for now
	case *ir.Call:
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
	case *ir.ListLiteral:
		tupleT := tupleType{
			fields:         make([]SimpleType, 0, len(expr.Args)),
			withProvenance: withProvenance{provenance: newOriginProv(expr, "list literal", "")},
		}
		for _, tupleElement := range expr.Args {
			typed := ctx.TypeExpr(tupleElement, vars)
			tupleT.fields = append(tupleT.fields, typed)
		}
		return tupleT
	case *ir.Assign:
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
	case *ir.LetGroup:
		bindingVars := make([]*typeVariable, len(expr.Vars))
		// first, add all the vars to the env
		for i, binding := range expr.Vars {
			bindingVars[i] = ctx.fresher.newTypeVariable(ctx.level+1, typeProvenance{}, binding.Var, nil, nil)
			ctx.env[binding.Var] = bindingVars[i]
		}
		// ...and after they have been added, actually try to resolve each binding's value
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

	case *ir.Unused:
		_ = ctx.nextLevel().TypeExpr(expr.Value, vars)
		return ctx.TypeExpr(expr.Body, vars)

	case *ir.Func:
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
	case *ir.Ascribe:
		// empty Ascribe is meaningless in the IR, and the compiler should ideally never generate it,
		// but we can guard against it here easily because it is a no-op
		if expr.Type_ == nil {
			return ctx.TypeExpr(expr.Expr, vars)
		}
		bodyType := ctx.TypeExpr(expr.Expr, vars)
		processedType, _ := ctx.typeIrType(expr.Type_, vars, false, nil)
		ctx.constrain(bodyType, processedType, prov.provenance, constrainOnErr)
		return processedType

	case *ir.RecordSelect:
		bodyType := ctx.TypeExpr(expr.Record, vars)
		res := ctx.newTypeVariable(prov.provenance, expr.Label, nil, nil)
		bodyType = makeProxy(bodyType, newOriginProv(expr, "receiver", ""))

		fieldProv := typeProvenance{
			Range: expr.Range,
			desc:  "field selector",
		}
		recordTyp := newRecordType(recordType{
			fields:         []recordField{{ir.Var{Name: expr.Label, Range: expr.Range}, newFieldTypeUpperBound(res, fieldProv)}},
			withProvenance: prov,
		})

		return ctx.typeExprConstrain(bodyType, recordTyp, res, prov.provenance)

	// TODO when/if we introduce record extend, do we need this case at all?
	//  a record literal is just a record extend of an empty record after all
	case *ir.RecordLit:
		labelOccurrences := make(map[string][]ir.Positioner, len(expr.Fields))
		for _, label := range expr.Fields {
			labelOccurrences[label.Label.Name] = append(labelOccurrences[label.Label.Name], label.Label)
		}

		for label, positioners := range labelOccurrences {
			if len(positioners) > 1 {
				ctx.addError(ilerr.New(ilerr.NewRepeatedRecordField{
					Positioner: expr,
					Names:      positioners,
					Name:       label,
				}))
			}
		}

		record := recordType{
			fields:         nil,
			withProvenance: prov,
		}
		for _, labelValue := range expr.Fields {
			valueType := ctx.TypeExpr(labelValue.Value, vars)
			fieldProv := typeProvenance{
				Range: ir.RangeBetween(labelValue.Label, labelValue.Value),
				desc:  "record field",
			}
			record.fields = append(record.fields, recordField{
				name:  labelValue.Label,
				type_: newFieldTypeUpperBound(valueType, fieldProv),
			})
		}
		return newRecordType(record)
	default:
		ctx.addFailure("not implemented: unexpected expression type for "+reflect.TypeOf(expr).String(), expr)
		return errorType()
	}
}

func (ctx *TypeCtx) TypeLetRecBody(name string, body ir.Expr) PolymorphicType {
	panic("TODO implement me")
}

// TypeExpr infers the type of a ast.Expr
//
// it is called typeTerm in the reference scala implementation
func (ctx *TypeCtx) TypeExpr(expr ir.Expr, vars map[typeName]SimpleType) (ret SimpleType) {
	typer := exprTyper{
		TypeCtx: ctx,
		Logger:  slog.New(ir.IleIRSlogHandler(slog.Default().Handler())).With("section", "inference"),
	}
	return typer.typeExpr(expr, vars)
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

func (ctx *exprTyper) typeWhenMatch(expr *ir.WhenMatch, vars map[typeName]SimpleType) (ret SimpleType) {
	subjectType := ctx.typeExpr(expr.Value, vars)
	if len(expr.Cases) == 0 {
		ctx.addError(ilerr.New(ilerr.NewEmptyWhen{Positioner: expr}))
		return errorType()
	}
	subjectVarName := ""
	if asVar, ok := expr.Value.(*ir.Var); ok {
		subjectVarName = asVar.Name
	}
	// used in union so we fold from bottom
	var finalType SimpleType = bottomType

	var requestedMatchedOnType SimpleType = bottomType
	for _, branch := range expr.Cases {
		branchT, resultT, tVar := ctx.typeWhenMatchBranch(subjectVarName, branch, vars)
		finalType = unionOf(finalType, resultT, unionOpts{prov: typeProvenance{Range: ir.RangeOf(branch.Value), desc: "when match branch"}})
		// FIXME: divergence from scala reference: MLStruct uses foldRight (right-to-left) processing
		// but Ile processes left-to-right. This may affect pattern negation accumulation order.
		// MLStruct formula: (a_ty & tv) | (req & a_ty.neg())
		requestedMatchedOnType = unionOf(intersectionOf(branchT, tVar, unionOpts{}), intersectionOf(requestedMatchedOnType, negateType(branchT, emptyProv), unionOpts{}), unionOpts{})
	}
	return ctx.typeExprConstrain(subjectType, requestedMatchedOnType, finalType, typeProvenance{Range: ir.RangeOf(expr), desc: "when match subject type"})
}

// typeWhenMatchBranch performs a step in typing a branch of a when pattern. The parameter `subject` should be set
// when the `when` is performed on an expression that is a variable, so that we can flow-type it inside the branches
//
// it returns:
//
// - branchT: the type of the pattern matched
// - resultT: the type of the branch value
// - tVar: a type variable with bounds corresponding to the `when` subject variable, if any
func (ctx *TypeCtx) typeWhenMatchBranch(subjectName string, branch ir.WhenCase, vars map[typeName]SimpleType) (branchT SimpleType, resultT SimpleType, tVar SimpleType) {
	tVar = topType
	pattern, ok := branch.Pattern.(*ir.MatchTypePattern)
	if !ok {
		ctx.addFailure(fmt.Sprintf("non-type pattern not implemented: %T", branch.Pattern), branch.Pattern)
		return errorType(), errorType(), tVar
	}
	tVars := make([]typeVariable, 0)
	if isWildcard := Equal(pattern.Type_, &ir.TypeName{Name: "_"}); isWildcard {
		branchT = topType
	} else {
		ctx.inPattern = true
		branchT, tVars = ctx.typeIrType(pattern.Type_, vars, false, nil)
		ctx.inPattern = false
		if len(tVars) != 0 {
			ctx.addFailure(fmt.Sprintf("type pattern not supported yet: %T", branch.Pattern), branch.Pattern)
			return errorType(), errorType(), tVar
		}
	}
	branchT = makeProxy(branchT, typeProvenance{desc: "pattern", Range: ir.RangeOf(branch.Pattern)})
	newCtx := ctx.nest()
	// if there is a subjectName, do flow typing by creating a new type variable for it, and constraining the branch type to it
	// so that it is present on the branch scope with the refined type of the pattern
	if subjectName != "" {
		tVar = newCtx.newTypeVariable(typeProvenance{Range: ir.RangeOf(branch.Pattern), desc: "refined pattern match subjectName"}, subjectName, nil, nil)
		newCtx.env[subjectName] = tVar
	}
	resultT = newCtx.TypeExpr(branch.Value, vars)
	return branchT, resultT, tVar
}

func (ctx *TypeCtx) GetLowerBoundFunctionType(t SimpleType) []funcType {
	switch t := unwrapProvenance(t).(type) {
	case PolymorphicType:
		//if simplifier.typeIsAliasOf(t) {
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
