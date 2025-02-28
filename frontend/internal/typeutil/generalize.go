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
	"github.com/cottand/ile/frontend/hmtypes"
)

func GeneralizeOpts(level uint, t hmtypes.Type, forceGeneralize, weak bool) hmtypes.Type {
	// Path compression:
	t = hmtypes.RealType(t)
	visitTypeVars(level, t, forceGeneralize, weak)
	return t
}

func visitTypeVars(level uint, t hmtypes.Type, forceGeneralize, weak bool) (tf hmtypes.TypeFlags) {
	switch t := t.(type) {
	case *hmtypes.Unit:
		return

	case *hmtypes.Var:
		switch {
		case t.IsLinkVar():
			return visitTypeVars(level, t.Link(), forceGeneralize, weak)
		case t.IsGenericVar():
			tf |= hmtypes.ContainsGenericVars
			// Weak type-variables may not be re-generalized after instantiation:
			if weak {
				t.SetWeak()
			}
		default: // weak or unbound
			// See "Efficient Generalization with Levels" (Oleg Kiselyov) -- http://okmij.org/ftp/ML/generalization.html#levels
			//
			// If the current level is less than the type-variable's level, a let-binding where the type-variable was instantiated
			// is being generalized:
			if t.LevelNum() > level && (forceGeneralize || (!weak && !t.IsWeakVar())) {
				tf |= hmtypes.ContainsGenericVars
				t.SetGeneric()
			}
			// Weak type-variables may not be re-generalized after instantiation:
			if weak {
				t.SetWeak()
			}
		}

	case *hmtypes.RecursiveLink:
		rec := t.Recursive
		if !rec.NeedsGeneralization() { // break cycles
			if rec.IsGeneric() {
				tf |= hmtypes.ContainsGenericVars
			}
			if rec.HasRefs() {
				tf |= hmtypes.ContainsRefs
			}
			return
		}
		rec.Flags &^= hmtypes.NeedsGeneralization // break cycles
		for _, alias := range rec.Types {
			tf |= visitTypeVars(level, alias, forceGeneralize, weak)
		}
		rec.Flags |= tf
		// back-propagate type-flags through links:
		if tf&(hmtypes.ContainsGenericVars|hmtypes.ContainsRefs) != 0 {
			for _, alias := range rec.Types {
				visitTypeVars(level, alias, forceGeneralize, weak)
			}
		}

	case *hmtypes.App:
		if hmtypes.IsRefType(t) {
			tf |= hmtypes.ContainsRefs
			weak = true
		}
		for i, param := range t.Params {
			t.Params[i] = hmtypes.RealType(param)
			tf |= visitTypeVars(level, t.Params[i], forceGeneralize, weak)
		}
		t.Const = hmtypes.RealType(t.Const)
		tf |= visitTypeVars(level, t.Const, forceGeneralize, weak)
		if t.Underlying != nil {
			t.Underlying = hmtypes.RealType(t.Underlying)
			tf |= visitTypeVars(level, t.Underlying, forceGeneralize, weak)
		}
		t.Flags |= tf

	case *hmtypes.Arrow:
		for i, arg := range t.Args {
			t.Args[i] = hmtypes.RealType(arg)
			tf |= visitTypeVars(level, t.Args[i], forceGeneralize, weak)
		}
		t.Return = hmtypes.RealType(t.Return)
		tf |= visitTypeVars(level, t.Return, forceGeneralize, weak)
		t.Flags |= tf

	case *hmtypes.Record:
		t.Row = hmtypes.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf

	case *hmtypes.Variant:
		t.Row = hmtypes.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf

	case *hmtypes.RowExtend:
		t.Labels.Range(func(label string, ts hmtypes.TypeList) bool {
			ts.Range(func(i int, t hmtypes.Type) bool {
				tf |= visitTypeVars(level, hmtypes.RealType(t), forceGeneralize, weak)
				return true
			})
			return true
		})
		t.Row = hmtypes.RealType(t.Row)
		tf |= visitTypeVars(level, t.Row, forceGeneralize, weak)
		t.Flags |= tf
	}
	return
}
