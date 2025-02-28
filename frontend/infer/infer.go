// The MIT License (MIT)
//
// Copyright (c) 2019 West Damron
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

package infer

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/hmtypes"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/internal/astutil"
	"github.com/cottand/ile/internal/log"
	"reflect"
)

var logger = log.DefaultLogger.With("section", "inference-each")

func (ti *InferenceContext) infer(env *TypeEnv, level uint, e ast.Expr) (ret hmtypes.Type, err error) {
	current := env.common.CurrentExpr
	env.common.CurrentExpr = e
	ret, err = ti.inferCurrentExpr(env, level)
	annotated, ok := e.(ast.TAnnotated)
	if ok && annotated.GetTAnnotation() != nil {
		constructed, err := annotated.GetTAnnotation().ConstructType(env, level, nil)
		if err != nil {
			return nil, err
		}
		if constructed != nil {
			err := env.common.TryUnify(constructed, ret)
			if err != nil {
				return nil, err
			}
		}
	}
	env.common.CurrentExpr = current
	return
}

func (ti *InferenceContext) inferCurrentExpr(env *TypeEnv, level uint) (ret hmtypes.Type, err error) {
	switch e := env.common.CurrentExpr.(type) {
	case *ast.Literal:
		var using []hmtypes.Type
		// Lookup types for variables bound within the literal:
		if len(e.Using) != 0 {
			using = make([]hmtypes.Type, len(e.Using))
			for i, name := range e.Using {
				vt := env.Lookup(name)
				if vt == nil {
					return nil, ilerr.New(ilerr.NewUndefinedVariable{
						Positioner: e,
						Name:       name,
					})
				}
				using[i] = vt
			}
		}
		// Construct and instantiate the literal with the bound variable types:
		t, err := e.Construct(env, level, using)
		if err != nil {
			ti.invalid, ti.err = e, err
			return t, err
		}
		t = env.common.Instantiate(level, t)
		if ti.annotate {
			e.SetType(t)
		}
		return t, nil

	case *ast.Var:
		var t hmtypes.Type
		var scope *ast.Scope
		if ti.annotate {
			t, scope = env.scopeLookup(e.Name)
		} else {
			t = env.Lookup(e.Name)
		}
		if t == nil {
			ti.invalid, ti.err = e, ilerr.New(ilerr.NewUndefinedVariable{
				Positioner: e,
				Name:       e.Name,
			})
			return nil, ti.err
		}
		t = env.common.Instantiate(level, t)
		if ti.annotate {
			e.SetType(t)
			e.SetScope(scope)
		}
		return t, nil

	case *ast.Deref:
		t, err := ti.infer(env, level, e.Ref)
		if err != nil {
			return nil, err
		}
		tv := env.common.VarTracker.New(level)
		tv.SetWeak()
		if err := env.common.Unify(hmtypes.NewRef(tv), t); err != nil {
			ti.invalid, ti.err = e, err
			return t, err
		}
		t = hmtypes.RealType(tv)
		if ti.annotate {
			e.SetType(t)
		}
		return t, nil

	case *ast.DerefAssign:
		ref, err := ti.infer(env, level, e.Ref)
		if err != nil {
			return ref, err
		}
		tv := env.common.VarTracker.New(level)
		tv.SetWeak()
		if err := env.common.Unify(hmtypes.NewRef(tv), ref); err != nil {
			ti.invalid, ti.err = e, err
			return ref, err
		}
		val, err := ti.infer(env, level, e.Value)
		if err != nil {
			return ref, err
		}
		if err := env.common.Unify(tv, val); err != nil {
			ti.invalid, ti.err = e, err
			return ref, err
		}
		ref = hmtypes.RealType(ref)
		if ti.annotate {
			e.SetType(ref)
		}
		return ref, nil

	case *ast.Pipe:
		// Inline equivalent to inferring as nested (non-recursive) let-bindings:
		t, err := ti.infer(env, level+1, e.Source)
		if err != nil {
			return nil, err
		}
		if len(e.Sequence) == 0 {
			if ti.annotate {
				e.SetType(t)
			}
			return t, nil
		}
		stashed := env.common.Stash(env, e.As)
		env.common.EnterScope(e)
		env.common.PushVarScope(e.As)
		for _, step := range e.Sequence {
			// Reassign the placeholder:
			env.Assign(e.As, GeneralizeAtLevel(level, t))
			t, err = ti.infer(env, level, step)
		}
		if ti.annotate && err != nil {
			e.SetType(t)
		}
		// Restore the parent scope:
		env.Remove(e.As)
		env.common.Unstash(env, stashed)
		env.common.LeaveScope()
		return t, err

	case *ast.Assign:
		var t hmtypes.Type
		env.common.EnterScope(e)
		env.common.PushVarScope(e.Var)
		stashed := 0
		// Infer the binding type:
		switch binding := e.Value.(type) {
		case *ast.Func:
			// Allow self-references within function types:
			varType := env.common.VarTracker.New(level + 1)
			// Begin a new scope:
			stashed = env.common.Stash(env, e.Var)
			env.Assign(e.Var, varType)
			t, err := ti.infer(env, level+1, binding)
			if err != nil {
				goto RestoreScope
			}
			if err := env.common.Unify(varType, t); err != nil {
				ti.invalid, ti.err = e, err
				goto RestoreScope
			}
			GeneralizeAtLevel(level, varType)
		default:
			t, err := ti.infer(env, level+1, binding)
			if err != nil {
				env.common.LeaveScope()
				return nil, err
			}
			// Begin a new scope:
			stashed = env.common.Stash(env, e.Var)
			env.Assign(e.Var, GeneralizeAtLevel(level, t))
		}
		// Infer the body type:
		t, _ = ti.infer(env, level, e.Body)
	RestoreScope:
		// Restore the parent scope:
		env.Remove(e.Var)
		env.common.Unstash(env, stashed)
		env.common.PopVarScope(e.Var)
		env.common.LeaveScope()
		return t, ti.err

		// desugared to an Assign which does not use its value
	case *ast.Unused:
		var t hmtypes.Type
		// TODO this was level+1 but I IIUC this does not make a difference as long
		//   as I do not add vars to the scope
		_, err := ti.infer(env, level, e.Value)
		if err != nil {
			return nil, err
		}

		// Infer the body type:
		t, _ = ti.infer(env, level, e.Body)
		return t, ti.err

	case *ast.LetGroup:
		// Grouped let-bindings are sorted into strongly-connected components, then type-checked in dependency order:
		env.common.EnterScope(e)
		t, err := ti.inferLetGroup(env, level, e)
		env.common.LeaveScope()
		return t, err

	case *ast.Func:
		args := make([]hmtypes.Type, len(e.ArgNames))
		stashed := 0
		vars := env.common.VarTracker.NewList(level, len(e.ArgNames))
		tv, tail := vars.Head(), vars.Tail()
		// Begin a new scope:
		env.common.EnterScope(e)
		for i, name := range e.ArgNames {
			stashed += env.common.Stash(env, name)
			args[i] = tv
			env.Assign(name, tv)
			env.common.PushVarScope(name)
			tv, tail = tail.Head(), tail.Tail()
		}
		ret, err := ti.infer(env, level, e.Body)
		for _, name := range e.ArgNames {
			env.Remove(name)
			env.common.PopVarScope(name)
		}
		// Restore the parent scope:
		env.common.LeaveScope()
		env.common.Unstash(env, stashed)
		t := &hmtypes.Arrow{Args: args, Return: ret}
		if ti.annotate {
			e.SetType(t)
		}
		return t, err

	case *ast.Call:
		ft, err := ti.infer(env, level, e.Func)
		if err != nil {
			return nil, err
		}

		// If t is an unbound type-variable, instantiate a function with unbound type-variables for its
		// arguments and return value; otherwise, ensure t has the correct argument count.
		arrow, err := ti.matchFuncType(env, len(e.Args), ft)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		args, ret := arrow.Args, arrow.Return
		for i, arg := range e.Args {
			ta, err := ti.infer(env, level, arg)
			if err != nil {
				return nil, err
			}
			if err := env.common.Unify(args[i], ta); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
		}
		if ti.annotate {
			arrow, _ := ft.(*hmtypes.Arrow)
			e.SetFuncType(arrow)
			e.SetType(ret)
		}
		return ret, nil

	case *ast.RecordEmpty:
		rt := &hmtypes.Record{Row: hmtypes.RowEmptyPointer}
		if ti.annotate {
			e.SetType(rt)
		}
		return rt, nil

	case *ast.RecordSelect:
		// label, rest := fresh(), fresh()
		// unify({ <label>: label | rest }, record)
		// -> label
		label, _, err := ti.splitRecord(env, level, e.Record, e.Label)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return label, nil

	case *ast.RecordRestrict:
		// label, rest := fresh(), fresh()
		// unify({ <label>: label | rest }, record)
		// -> rest
		_, rest, err := ti.splitRecord(env, level, e.Record, e.Label)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		return rest, nil

	case *ast.RecordExtend:
		mb := hmtypes.NewTypeMapBuilder()
		for _, label := range e.Labels {
			t, err := ti.infer(env, level, label.Value)
			if err != nil {
				return nil, err
			}
			mb.Set(label.Label, hmtypes.SingletonTypeList(t))
		}
		rowType := env.common.VarTracker.New(level)
		recordType, err := ti.infer(env, level, e.Record)
		if err != nil {
			return nil, err
		}
		if err := env.common.Unify(&hmtypes.Record{Row: rowType}, recordType); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		ext := &hmtypes.RowExtend{Row: rowType, Labels: mb.Build()}
		labels, rest, err := hmtypes.FlattenRowType(ext)
		if err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		ext.Labels, ext.Row = labels, rest
		rt := &hmtypes.Record{Row: ext}
		if ti.annotate {
			e.SetType(rt)
		}
		return rt, nil

	case *ast.Variant:
		rowType := env.common.VarTracker.New(level)
		variantType := env.common.VarTracker.New(level)
		t, err := ti.infer(env, level, e.Value)
		if err != nil {
			return nil, err
		}
		if err := env.common.Unify(variantType, t); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		labels := hmtypes.SingletonTypeMap(e.Label, variantType)
		vt := &hmtypes.Variant{Row: &hmtypes.RowExtend{Row: rowType, Labels: labels}}
		return vt, nil

	case *ast.WhenMatch:
		// Inline equivalent to inferring a record-select on a record constructed from the cases,
		// where each case is represented as a labeled function from the case's variant-type to the
		// shared return type for the match expression:
		//
		// variant := [:a 'a]
		// handler := ({a : 'a -> 'result, b : 'b -> 'result})[variant.label]
		// result := handler(variant.value)
		var (
			retType, rowType hmtypes.Type
			err              error
		)
		if e.Default == nil {
			retType, rowType = env.common.VarTracker.New(level), hmtypes.RowEmptyPointer
		} else {
			rowType = env.common.VarTracker.New(level)
			// Begin a new scope:
			stashed := env.common.Stash(env, e.Default.Label)
			env.common.EnterScope(e)
			env.Assign(e.Default.Label, &hmtypes.Variant{Row: rowType})
			env.common.PushVarScope(e.Default.Label)
			retType, err = ti.infer(env, level, e.Default.Value)
			// Restore the parent scope:
			env.Remove(e.Default.Label)
			env.common.Unstash(env, stashed)
			env.common.PopVarScope(e.Default.Label)
			env.common.LeaveScope()
			if err != nil {
				return nil, err
			}
		}
		matchType, err := ti.infer(env, level, e.Value)
		if err != nil {
			return nil, err
		}
		env.common.EnterScope(e)
		casesRow, err := ti.inferCases(env, level, retType, rowType, e.Cases)
		env.common.LeaveScope()
		if err != nil {
			return nil, err
		}
		if err := env.common.Unify(matchType, casesRow); err != nil {
			ti.invalid, ti.err = e, err
			return nil, err
		}
		if ti.annotate {
			e.SetType(retType)
		}
		return retType, nil
	}

	e := env.common.CurrentExpr
	var exprName string
	if e != nil {
		exprName = "(" + e.ExprName() + ")"
	} else {
		exprName = "(nil)"
	}
	ti.invalid, ti.err = e, errors.New("Unhandled expression type "+exprName)
	return nil, ti.err
}

// label, rest := fresh(), fresh()
// unify({ <label>: label | rest }, record)
// -> (label, rest)
func (ti *InferenceContext) splitRecord(env *TypeEnv, level uint, recordExpr ast.Expr, label string) (labelType hmtypes.Type, restType hmtypes.Type, err error) {
	rowType := env.common.VarTracker.New(level)
	labelType = env.common.VarTracker.New(level)
	labels := hmtypes.SingletonTypeMap(label, labelType)
	paramType := &hmtypes.Record{Row: &hmtypes.RowExtend{Row: rowType, Labels: labels}}
	var recordType hmtypes.Type
	recordType, err = ti.infer(env, level, recordExpr)
	if err != nil {
		return nil, nil, err
	}
	if err = env.common.Unify(paramType, recordType); err != nil {
		return nil, nil, err
	}
	restType = &hmtypes.Record{Row: rowType}
	return
}

// If t is an unbound type-variable, instantiate a function with unbound type-variables for its arguments and return value;
// otherwise, ensure t has the correct argument count.
func (ti *InferenceContext) matchFuncType(env *TypeEnv, argc int, t hmtypes.Type) (*hmtypes.Arrow, error) {
	switch t := t.(type) {
	case *hmtypes.Arrow:
		if len(t.Args) != argc {
			return t, errors.New("Unexpected number of arguments for applied function")
		}
		return t, nil

	case *hmtypes.Var:
		switch {
		case t.IsLinkVar():
			return ti.matchFuncType(env, argc, t.Link())
		case t.IsUnboundVar():
			args := make([]hmtypes.Type, argc)
			vars := env.common.VarTracker.NewList(t.Level(), argc+1)
			tv, tail := vars.Head(), vars.Tail()
			for i := 0; i < argc; i++ {
				args[i] = tv
				tv, tail = tail.Head(), tail.Tail()
			}
			arrow := &hmtypes.Arrow{Args: args, Return: tv}
			t.SetLink(arrow)
			return arrow, nil
		default:
			return nil, errors.New("Type variable for applied function has not been instantiated")
		}
	}

	return nil, errors.New("Unexpected type " + t.TypeName() + " for applied function")
}

// https://github.com/tomprimozic/type-systems/blob/master/extensible_rows2/infer.ml#L287
//
// infer_cases env level return_ty rest_row_ty cases = match cases with
//
//	| [] -> rest_row_ty
//	| (label, var_name, expr) :: other_cases ->
//			let variant_ty = new_var level in
//			unify return_ty (infer (Env.extend env var_name variant_ty) level expr) ;
//			let other_cases_row = infer_cases env level return_ty rest_row_ty other_cases in
//			TRowExtend(LabelMap.singleton label [variant_ty], other_cases_row)
func (ti *InferenceContext) inferCases(env *TypeEnv, level uint, retType, rowType hmtypes.Type, cases []ast.WhenCase) (retRowType hmtypes.Type, err error) {
	// Each case extends an existing record formed from all subsequent cases.
	// Visit cases in reverse order, accumulating labels and value types into the record as row-extensions.
	vars := env.common.VarTracker.NewList(level, len(cases))
	tv, tail := vars.Head(), vars.Tail()
	for i := len(cases) - 1; i >= 0; i-- {
		c := cases[i]
		switch pattern := c.Pattern.(type) {
		case *ast.VariantPattern:
			// Infer the return expression for the case with the variable-name temporarily bound in the environment:
			variantType := tv
			// Begin a new scope:
			stashed := env.common.Stash(env, pattern.Var)
			env.Assign(pattern.Var, variantType)
			env.common.PushVarScope(pattern.Var)
			pattern.SetVariantType(variantType)
			t, err := ti.infer(env, level, c.Value)
			env.Remove(pattern.Var)
			env.common.PopVarScope(pattern.Var)
			// Restore the parent scope:
			env.common.Unstash(env, stashed)
			if err != nil {
				return nil, err
			}
			// Ensure all cases have matching return types:
			if err := env.common.Unify(retType, t); err != nil {
				return nil, err
			}
			// Extend the accumulated record:
			var extension hmtypes.RowExtend
			extension.Row, extension.Labels = rowType, hmtypes.SingletonTypeMap(pattern.Label, variantType)
			rowType = &extension
			tv, tail = tail.Head(), tail.Tail()

			// todo encode type error where the subject is a variant but the pattern looks for a literal -
			//   something that will never succeed. At the same time, we need to encode success for matching
			//   random Exprs to literals
			//   I think the solution is a new Any type!
		case *ast.ValueLiteralPattern:
			t, err := ti.infer(env, level, c.Value)
			if err != nil {
				return nil, err
			}
			// Ensure all cases have matching return types:
			if err := env.common.Unify(retType, t); err != nil {
				return nil, err
			}
			tv, tail = tail.Head(), tail.Tail()

		default:
			return nil, fmt.Errorf("unsupported pattern match type %v", reflect.TypeOf(pattern))
		}
	}
	subjectType := &hmtypes.Variant{Row: rowType}
	// Return the accumulated record which maps each variant label to its associated type(s):
	return subjectType, nil
}

// Grouped let-bindings are sorted into strongly-connected components, then type-checked in dependency order.
func (ti *InferenceContext) inferLetGroup(env *TypeEnv, level uint, e *ast.LetGroup) (ret hmtypes.Type, err error) {
	if !ti.analyzed {
		if ti.analysis == nil {
			ti.analysis = new(astutil.Analysis)
			ti.analysis.Init()
		}
		if err := ti.analysis.Analyze(ti.rootExpr); err != nil {
			ti.invalid, ti.err, ti.analysis.Invalid = ti.analysis.Invalid, err, nil
			return nil, err
		}
		ti.analyzed = true
	}
	for _, v := range e.Vars {
		env.common.PushVarScope(v.Var)
	}
	stashed, sccs := 0, ti.analysis.SCC[ti.letGroupCount]
	ti.letGroupCount++
	// Grouped let-bindings are sorted into strongly-connected components, then type-checked in dependency order:
	for _, scc := range sccs {
		// Add fresh type-variables for bindings:
		vars := env.common.VarTracker.NewList(level+1, len(scc))
		tv, tail := vars.Head(), vars.Tail()
		// Begin a new scope:
		for _, bindNum := range scc {
			v := e.Vars[bindNum]
			stashed += env.common.Stash(env, v.Var)
			env.Assign(v.Var, tv)
			tv, tail = tail.Head(), tail.Tail()
		}
		// Infer types:
		tv, tail = vars.Head(), vars.Tail()
		for _, bindNum := range scc {
			v := e.Vars[bindNum]
			var isFunc bool
			// To prevent self-references within non-function types, stash/remove the type-variable:
			if _, isFunc = v.Value.(*ast.Func); !isFunc {
				exists := false
				for i := 0; i < stashed; i++ {
					existing := env.common.EnvStash[len(env.common.EnvStash)-(1+i)]
					if existing.Name == v.Var {
						env.Assign(v.Var, existing.Type)
						exists = true
						break
					}
				}
				if !exists {
					env.Remove(v.Var)
				}
			}
			t, err := ti.infer(env, level+1, v.Value)
			if err != nil {
				return nil, err
			}
			if err := env.common.Unify(tv, t); err != nil {
				ti.invalid, ti.err = e, err
				return nil, err
			}
			// Restore the previously stashed/removed type-variable:
			if !isFunc {
				env.Assign(v.Var, tv)
			}
			tv, tail = tail.Head(), tail.Tail()
		}
		// Generalize types:
		tv, tail = vars.Head(), vars.Tail()
		for _, bindNum := range scc {
			v := e.Vars[bindNum]
			env.Assign(v.Var, GeneralizeAtLevel(level, tv))
			tv, tail = tail.Head(), tail.Tail()
		}
	}

	t, err := ti.infer(env, level, e.Body)
	// Restore the parent scope:
	for _, v := range e.Vars {
		env.Remove(v.Var)
		env.common.PopVarScope(v.Var)
	}
	env.common.Unstash(env, stashed)
	if err == nil && ti.annotate {
		sccBindings := make([][]ast.LetBinding, len(sccs))
		for i, scc := range sccs {
			cycle := make([]ast.LetBinding, len(scc))
			for j, binding := range scc {
				cycle[j] = e.Vars[binding]
			}
			sccBindings[i] = cycle
		}
		e.SetStronglyConnectedComponents(sccBindings)
	}
	return t, err
}
