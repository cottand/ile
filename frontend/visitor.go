package frontend

import (
	"errors"
	"fmt"
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

	expressionStack         []ir.Expr
	paramDeclStack          []ir.ParamDecl
	functionReturnTypeStack []ir.Type
	typeStack               []ir.Type
}

func (l *listener) popExpr() (ir.Expr, error) {
	if len(l.expressionStack) <= 0 {
		return nil, errors.New("expected to have successfully parsed an expression, but none were found")
	}
	lastIndex := len(l.expressionStack) - 1
	defer func() {
		l.expressionStack = l.expressionStack[:lastIndex]
	}()
	return l.expressionStack[len(l.expressionStack)-1], nil
}

func (l *listener) popAllParamDecl() []ir.ParamDecl {
	defer func() {
		l.paramDeclStack = make([]ir.ParamDecl, 0)
	}()
	return l.paramDeclStack
}

func (l *listener) popFunctionReturnType() (ir.Type, error) {
	if len(l.functionReturnTypeStack) <= 0 {
		return nil, errors.New("expected to have successfully parsed a functionReturnType, but none were found")
	}
	lastIndex := len(l.functionReturnTypeStack) - 1
	defer func() {
		l.functionReturnTypeStack = l.functionReturnTypeStack[:lastIndex]
	}()
	return l.functionReturnTypeStack[len(l.functionReturnTypeStack)-1], nil
}

func (l *listener) popType() (ir.Type, error) {
	if len(l.typeStack) <= 0 {
		return nil, errors.New("expected to have successfully parsed a type, but none were found")
	}
	lastIndex := len(l.typeStack) - 1
	defer func() {
		l.typeStack = l.typeStack[:lastIndex]
	}()
	return l.typeStack[lastIndex], nil
}

func (l *listener) Result() (ir.File, []ir.CompileError) {
	return l.file, l.errors
}

func (l *listener) VisitErrors() error {
	return errors.Join(l.visitErrors...)
}

func (l *listener) ExitSourceFile(ctx *parser.SourceFileContext) {
	l.file.Range = intervalTo2Pos(ctx.GetSourceInterval())
}

//
// Expressions
//

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
		l.file.Values = append(l.file.Values, ir.ValDecl{
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
			Name:  ctx.IDENTIFIER().GetText(),
			E:     expr,
		})
	}
}
func (l *listener) ExitExpression(ctx *parser.ExpressionContext) {
	if ctx.PrimaryExpr() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}
	if ctx.GetAdd_op() != nil {
		rhs, err := l.popExpr()
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			return
		}
		lhs, err := l.popExpr()
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			return
		}
		l.expressionStack = append(l.expressionStack, ir.BinaryOpExpr{
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
			Op:    parser.IleTokenInGo[ctx.GetAdd_op().GetTokenType()],
			Lhs:   lhs,
			Rhs:   rhs,
		})
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

func intervalTo2Pos(i antlr.Interval) ir.Range {
	return ir.Range{
		PosStart: token.Pos(i.Start),
		PosEnd:   token.Pos(i.Stop),
	}
}

func (l *listener) ExitLiteral(ctx *parser.LiteralContext) {
	if s := ctx.String_(); s != nil {
		if strLit := s.RAW_STRING_LIT(); strLit != nil {
			l.expressionStack = append(l.expressionStack, ir.BasicLit{
				Kind:  token.STRING,
				Value: strings.Trim(strLit.GetText(), "`"),
				Range: intervalTo2Pos(ctx.GetSourceInterval()),
			})
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {
			l.expressionStack = append(l.expressionStack, ir.BasicLit{
				Kind:  token.STRING,
				Value: strings.Trim(strLit.GetText(), "\""),
				Range: intervalTo2Pos(ctx.GetSourceInterval()),
			})
			return
		}
	}

	if ctx.Integer() != nil {
		l.expressionStack = append(l.expressionStack, ir.BasicLit{
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
			Kind:  token.INT,
			Value: ctx.Integer().GetText(),
		})
	}
}

//
// Functions
//

func (l *listener) ExitFunctionDecl(ctx *parser.FunctionDeclContext) {
	fun := ir.FuncDecl{
		Range:  intervalTo2Pos(ctx.GetSourceInterval()),
		Name:   ctx.IDENTIFIER().GetText(),
		Params: l.popAllParamDecl(),
	}
	defer func() {
		l.file.Functions = append(l.file.Functions, fun)
	}()

	expr, err := l.popExpr()
	if err != nil {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse an expression, but none found in function %v: %v", fun.Name, err))
		return
	}
	fun.Body = expr
	if ctx.Signature().Result() != nil {
		retT, err := l.popFunctionReturnType()
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			return
		}
		fun.Result = retT
	}
}

func (l *listener) ExitParameterDecl(ctx *parser.ParameterDeclContext) {
	typ, err := l.popType()
	if err != nil {
		l.visitErrors = append(l.visitErrors, err)
		return
	}
	l.paramDeclStack = append(l.paramDeclStack, ir.ParamDecl{
		Range: intervalTo2Pos(ctx.GetSourceInterval()),
		Name: ir.Ident{
			Range: intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
			Name:  ctx.IDENTIFIER().GetText(),
		},
		T: typ,
	})
}

// ExitResult as in function signature result
// we can expect an ir.Type to be present if a Result return type is present in the function
func (l *listener) ExitResult(ctx *parser.ResultContext) {
	if ctx.Type_() != nil {
		parsedT, err := l.popType()
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			return
		}
		l.functionReturnTypeStack = append(l.functionReturnTypeStack, parsedT)
	}
}

func (l *listener) ExitType_(ctx *parser.Type_Context) {
	typeName := ctx.TypeName()
	if typeName != nil {
		typeLit := ir.TypeLit{
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
		}
		if typeName.QualifiedIdent() != nil {
			typeLit.Package = typeName.QualifiedIdent().IDENTIFIER(0).GetText()
			typeLit.Name = typeName.QualifiedIdent().IDENTIFIER(1).GetText()
		} else {
			typeLit.Name = typeName.GetText()
		}

		l.typeStack = append(l.typeStack, typeLit)
	}
	// TODO composite types!
}

//
// Errors
//

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
