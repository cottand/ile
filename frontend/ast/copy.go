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
	default:
		return e.Copy()
	}
}
