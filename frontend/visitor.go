package frontend

import (
	"errors"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/ir"
	"github.com/cottand/ile/parser"
	"go/token"
	"strings"
)

// listener traverses the parse tree
// by using an expressionStack which keeps track of the latest
// successfully evaluated expressions
// when an expect an expression in the latest node, just pop it from
// the stack as it will already have been visited
type listener struct {
	*parser.BaseIleParserListener

	file   ir.File
	errors []ir.CompileError

	visitErrors []error

	expressionStack []ir.Expr
}

func (l *listener) popExpr() (ir.Expr, error) {
	if len(l.expressionStack) <= 0 {
		return nil, errors.New("expected to have successfully parsed an expression, but none were found")
	}
	return l.expressionStack[len(l.expressionStack)-1], nil

}

func (l *listener) Result() (ir.File, []ir.CompileError) {
	return l.file, l.errors
}

func (l *listener) VisitErrors() error {
	return errors.Join(l.visitErrors...)
}

func (l *listener) EnterPackageClause(ctx *parser.PackageClauseContext) {
	l.file.PkgName = ctx.IDENTIFIER().GetText()
}

func (l *listener) ExitVarDecl(ctx *parser.VarDeclContext) {
	// declaration is at file source
	if _, ok := ctx.GetParent().GetParent().(*parser.SourceFileContext); ok {
		expr, err := l.popExpr()
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			return
		}
		l.file.Declarations = append(l.file.Declarations, ir.ValDecl{
			Name: ctx.IDENTIFIER().GetText(),
			E:    expr,
		})
	}
}
func (l *listener) ExitExpression(ctx *parser.ExpressionContext) {
	if ctx.PrimaryExpr() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}
}

func (l *listener) ExitPrimaryExpr(ctx *parser.PrimaryExprContext) {
	if ctx.Operand() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}
}

func (l *listener) ExitOperand(ctx *parser.OperandContext) {
	if ctx.Expression() != nil {
		// then the latest expression on the stack is this one, do nothing
		return
	}
	if ctx.Literal() != nil {
		// we will deal with in ExitLiteral
		return
	}
}

func intervalTo2Pos(i antlr.Interval) (start token.Pos, end token.Pos) {
	return token.Pos(i.Start), token.Pos(i.Stop)
}

func (l *listener) ExitLiteral(ctx *parser.LiteralContext) {
	if s := ctx.String_(); s != nil {
		if strLit := s.RAW_STRING_LIT(); strLit != nil {
			start, end := intervalTo2Pos(ctx.GetSourceInterval())
			l.expressionStack = append(l.expressionStack, &ir.BasicLit{
				Kind:        token.STRING,
				Value:       strings.Trim(strLit.GetText(), "`"),
				ValuePos:    start,
				ValuePosEnd: end,
			})
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {
			start, end := intervalTo2Pos(ctx.GetSourceInterval())
			l.expressionStack = append(l.expressionStack, &ir.BasicLit{
				Kind:        token.STRING,
				Value:       strings.Trim(strLit.GetText(), "\""),
				ValuePos:    start,
				ValuePosEnd: end,
			})
			return
		}
	}

	if ctx.Integer() != nil {
		l.expressionStack = append(l.expressionStack, &ir.BasicLit{
			Kind:     token.INT,
			Value:    ctx.Integer().GetText(),
			ValuePos: token.Pos(ctx.GetStart().GetStart()),
		})
	}
}

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, ir.CompileError{
		Message: "error at: " + node.GetText(),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
	l.errors = append(l.errors, ir.CompileError{
		Message: "terminal at: " + node.GetText(),
	})
}
