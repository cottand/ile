package ast

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
				ctx.WriteString("\n")
				ctx.WriteString(")")
			}()
		}
		for _, binding := range expr.Vars {
			ctx.WriteString(fmt.Sprintf("val %s = ", binding.Var))
			ctx.showExprWalker(binding.Value, 1)
			ctx.WriteString("\n")
		}
		ctx.showExprWalker(expr.Body, outerPrecedence)
	default:
		if outerPrecedence > 0 {
			ctx.WriteString("(" + expr.ExprName() + ")")
		} else {
			ctx.WriteString(expr.ExprName())
		}
	}

}
