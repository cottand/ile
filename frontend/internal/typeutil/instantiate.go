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

package typeutil

import (
	"github.com/cottand/ile/frontend/types"
)

func (ctx *CommonContext) Instantiate(level uint, t hmtypes.Type) hmtypes.Type {
	// Path compression:
	t = hmtypes.RealType(t)
	// Non-generic types can be shared:
	if !t.IsGeneric() {
		return t
	}
	t = ctx.visitInstantiate(level, t)
	ctx.ClearInstantiationLookup()
	return t
}

func (ctx *CommonContext) visitInstantiate(level uint, t hmtypes.Type) hmtypes.Type {
	// Path compression:
	t = hmtypes.RealType(t)

	// Non-generic types can be shared:
	if !t.IsGeneric() {
		return t
	}

	switch t := t.(type) {
	case *hmtypes.Var:
		id := t.Id()
		if tv, ok := ctx.InstLookup[id]; ok {
			return tv
		}
		next := ctx.VarTracker.New(level)
		next.Restrict(t.Level())
		if t.IsWeakVar() {
			next.SetWeak()
		}
		constraints := t.Constraints()
		constraintsCopy := make([]hmtypes.InstanceConstraint, len(constraints))
		copy(constraintsCopy, constraints)
		next.SetConstraints(constraintsCopy)
		ctx.InstLookup[t.Id()] = next
		return next

	case *hmtypes.RecursiveLink:
		rec := t.Recursive
		next := &hmtypes.Recursive{
			Source:  rec,
			Params:  make([]*hmtypes.Var, len(rec.Params)),
			Types:   make([]*hmtypes.App, 0, len(rec.Types)), // types are added during Bind
			Names:   rec.Names,
			Indexes: rec.Indexes,
			Flags:   rec.Flags,
			Bind:    rec.Bind,
		}
		copy(next.Params, rec.Params)
		for i, tv := range next.Params {
			p := ctx.visitInstantiate(level, tv)
			if tv, ok := p.(*hmtypes.Var); ok {
				next.Params[i] = tv
			} else {
				tv = ctx.VarTracker.New(level)
				tv.SetLink(p)
				next.Params[i] = tv
			}
		}
		next.Bind(next)
		next.Flags &^= hmtypes.ContainsGenericVars
		return &hmtypes.RecursiveLink{Recursive: next, Index: t.Index, Source: t}

	case *hmtypes.App:
		params := make([]hmtypes.Type, len(t.Params))
		for i, param := range t.Params {
			params[i] = ctx.visitInstantiate(level, param)
		}
		var underlying hmtypes.Type
		if t.Underlying != nil {
			underlying = ctx.visitInstantiate(level, t.Underlying)
		}
		return &hmtypes.App{Const: ctx.visitInstantiate(level, t.Const), Params: params, Underlying: underlying, Source: t}

	case *hmtypes.Arrow:
		args := make([]hmtypes.Type, len(t.Args))
		for i, arg := range t.Args {
			args[i] = ctx.visitInstantiate(level, arg)
		}
		return &hmtypes.Arrow{Args: args, Return: ctx.visitInstantiate(level, t.Return), Method: t.Method, Source: t}

	case *hmtypes.Method:
		arrow := ctx.visitInstantiate(level, t.TypeClass.Methods[t.Name]).(*hmtypes.Arrow)
		arrow.Method = t
		return arrow

	case *hmtypes.Record:
		return &hmtypes.Record{Row: ctx.visitInstantiate(level, t.Row), Source: t}

	case *hmtypes.Variant:
		return &hmtypes.Variant{Row: ctx.visitInstantiate(level, t.Row), Source: t}

	case *hmtypes.RowExtend:
		m := t.Labels
		// if the labels don't contain generic types, they don't need to be copied:
		var mb hmtypes.TypeMapBuilder
		needsRebuild := false
		builderInitialized := false
		m.Range(func(label string, ts hmtypes.TypeList) bool {
			// common case for unscoped labels:
			if ts.Len() == 1 {
				t := hmtypes.RealType(ts.Get(0))
				if t.IsGeneric() {
					if !builderInitialized {
						mb, builderInitialized, needsRebuild = m.Builder(), true, true
					}
					mb.Set(label, hmtypes.SingletonTypeList(ctx.visitInstantiate(level, t)))
				}
				return true
			}
			// only build a new type list (and update the map) if the existing list contains generic types:
			listNeedsRebuild := false
			newList := hmtypes.NewTypeList()
			ts.Range(func(i int, t hmtypes.Type) bool {
				t = hmtypes.RealType(t)
				if t.IsGeneric() {
					listNeedsRebuild = true
					newList.Append(ctx.visitInstantiate(level, t))
				} else {
					newList.Append(nil)
				}
				return true
			})
			if !listNeedsRebuild {
				return true
			}
			if !builderInitialized {
				mb, builderInitialized, needsRebuild = m.Builder(), true, true
			}
			mb.Set(label, newList)
			return true
		})
		row := t.Row
		if row == nil {
			row = hmtypes.RowEmptyPointer
		} else if _, ok := row.(*hmtypes.RowEmpty); !ok {
			row = ctx.visitInstantiate(level, t.Row)
		}
		if needsRebuild {
			m = mb.Build()
		}
		return &hmtypes.RowExtend{Row: row, Labels: m, Source: t}
	}
	panic("unexpected generic type " + t.TypeName())
}
