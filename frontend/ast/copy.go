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

package ast

func CopyExpr(e Expr) Expr {
	switch e := e.(type) {
	case *Literal:
		return &Literal{Syntax: e.Syntax, Using: e.Using, Construct: e.Construct, inferred: e.inferred}

	case *Var:
		return &Var{Name: e.Name, inferred: e.inferred, scope: e.scope}

	case *Deref:
		return &Deref{Ref: e.Ref, inferred: e.inferred}

	case *DerefAssign:
		return &DerefAssign{Ref: e.Ref, Value: e.Value, inferred: e.inferred}

	case *Call:
		args := make([]Expr, len(e.Args))
		for i, arg := range e.Args {
			args[i] = CopyExpr(arg)
		}
		return &Call{Func: CopyExpr(e.Func), Args: args, inferred: e.inferred, inferredFunc: e.inferredFunc}

	case *Func:
		return &Func{ArgNames: e.ArgNames, Body: CopyExpr(e.Body), inferred: e.inferred}

	case *Pipe:
		seq := make([]Expr, len(e.Sequence))
		for i, step := range e.Sequence {
			seq[i] = CopyExpr(step)
		}
		return &Pipe{Source: CopyExpr(e.Source), As: e.As, Sequence: seq, inferred: e.inferred}

	case *Assign:
		return &Assign{Var: e.Var, Value: CopyExpr(e.Value), Body: CopyExpr(e.Body)}

	case *LetGroup:
		vars := make([]LetBinding, len(e.Vars))
		for i, v := range e.Vars {
			vars[i] = LetBinding{v.Var, CopyExpr(v.Value)}
		}
		return &LetGroup{Vars: vars, Body: CopyExpr(e.Body), sccs: e.sccs}

	case *RecordSelect:
		return &RecordSelect{Record: CopyExpr(e.Record), Label: e.Label, inferred: e.inferred}

	case *RecordExtend:
		labels := make([]LabelValue, len(e.Labels))
		for i, v := range e.Labels {
			labels[i] = LabelValue{v.Label, CopyExpr(v.Value)}
		}
		record := e.Record
		if record == nil {
			record = &RecordEmpty{}
		} else {
			record = CopyExpr(record)
		}
		return &RecordExtend{Record: record, Labels: labels, inferred: e.inferred}

	case *RecordRestrict:
		return &RecordRestrict{Record: CopyExpr(e.Record), Label: e.Label, inferred: e.inferred}

	case *RecordEmpty:
		return &RecordEmpty{inferred: e.inferred}

	case *Variant:
		return &Variant{Label: e.Label, Value: CopyExpr(e.Value)}

	case *Match:
		cases := make([]MatchCase, len(e.Cases))
		for i, v := range e.Cases {
			cases[i] = MatchCase{v.Label, v.Var, CopyExpr(v.Value), v.varType}
		}
		defaultCase := e.Default
		if defaultCase != nil {
			defaultCase = &MatchCase{defaultCase.Label, defaultCase.Var, CopyExpr(defaultCase.Value), defaultCase.varType}
		}
		return &Match{Value: CopyExpr(e.Value), Cases: cases, Default: defaultCase, inferred: e.inferred}

	case *ControlFlow:
		next := NewControlFlow(e.Name, e.Locals...)
		blocks := make([]Block, len(e.Blocks))
		for i, b := range e.Blocks {
			blocks[i].Sequence = make([]Expr, len(b.Sequence))
			for j, sub := range b.Sequence {
				blocks[i].Sequence[j] = CopyExpr(sub)
			}
			blocks[i].Index = b.Index
		}
		next.Blocks = blocks
		next.Entry.Sequence = make([]Expr, len(e.Entry.Sequence))
		for i, sub := range e.Entry.Sequence {
			next.Entry.Sequence[i] = CopyExpr(sub)
		}
		next.Return.Sequence = make([]Expr, len(e.Return.Sequence))
		for i, sub := range e.Return.Sequence {
			next.Return.Sequence[i] = CopyExpr(sub)
		}
		copy(next.Jumps, e.Jumps)
		return next
	}
	panic("unknown expression type: " + e.ExprName())
}
