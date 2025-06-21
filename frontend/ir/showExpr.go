package ir

import (
	"fmt"
	"strings"
)

func ExprString(expr Expr) string {
	ctx := newShowContext()
	ctx.showExprWalker(expr, 0)
	return ctx.String()
}

type showContext struct {
	*strings.Builder
	indent    int
	indentStr string
}

func newShowContext() *showContext {
	return &showContext{
		Builder:   &strings.Builder{},
		indentStr: "  ",
		indent:    0,
	}
}

func (ctx *showContext) currentIndent() string {
	return strings.Repeat(ctx.indentStr, ctx.indent)
}

// showExprWalker prints to ctx
//
// precedences are as follows:
// 0: can be shown on its own
// 1-10: can be shown outside of arithmetic ops (like fn -> X)
// 22: -
// 23: +
// 24: /
// 25: *
func (ctx *showContext) showExprWalker(expr Expr, outerPrecedence int16) {
	if expr == nil {
		ctx.WriteString("nil")
		return
	}
	switch expr := expr.(type) {
	case AtomicExpr:
		ctx.WriteString(expr.CanonicalSyntax())
	case *Call:
		ctx.showExprWalker(expr.Func, 0)
		ctx.WriteString("(")
		for i, arg := range expr.Args {
			if i > 0 {
				ctx.WriteString(", ")
			}
			ctx.showExprWalker(arg, 0)
		}
		ctx.WriteString(")")
	case *LetGroup:
		if outerPrecedence > 0 && len(expr.Vars) > 0 {
			ctx.WriteString("(\n")
			ctx.indent++
			defer func() {
				ctx.indent--
				ctx.WriteString("\n)")
			}()
		}
		for _, binding := range expr.Vars {
			ctx.WriteString(fmt.Sprintf("val %s = ", binding.Var))
			ctx.showExprWalker(binding.Value, 1)
			ctx.WriteString("\n")
		}
		ctx.showExprWalker(expr.Body, outerPrecedence)
	case *Ascribe:
		var parenPrecedence int16 = 20
		if outerPrecedence > parenPrecedence {
			ctx.WriteString("(")
		}
		ctx.showExprWalker(expr.Expr, 10)
		if outerPrecedence > parenPrecedence {
			ctx.WriteString(")")
		}
		ctx.WriteString(": " + expr.Type_.ShowIn(DumbShowCtx, 40))

	default:
		if outerPrecedence > 0 {
			ctx.WriteString("(" + expr.ExprName() + ")")
		} else {
			ctx.WriteString(expr.ExprName())
		}
	}

}
