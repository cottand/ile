package frontend1

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
	errors []*ir.CompileError

	visitErrors []error

	expressionStack         []ir.Expr
	blockExprStack          []ir.Expr
	paramDeclStack          []ir.ParamDecl
	functionReturnTypeStack []ir.Type
	typeStack               []ir.Type

	pendingScopedExprStack []pendingScoped
}

type pendingScoped interface {
	assignRemainder(expr ir.Expr) ir.Expr
}

type pendingAssign ir.AssignExpr

func (p pendingAssign) assignRemainder(expr ir.Expr) ir.Expr {
	p.Remainder = expr
	return ir.AssignExpr(p)
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

func (l *listener) popBlockExpr() (ir.Expr, error) {
	if len(l.blockExprStack) <= 0 {
		return nil, errors.New("expected to have successfully parsed a block expression, but none were found")
	}
	lastIndex := len(l.blockExprStack) - 1
	defer func() {
		l.blockExprStack = l.blockExprStack[:lastIndex]
	}()
	return l.blockExprStack[len(l.blockExprStack)-1], nil
}

func (l *listener) popPending() (pendingScoped, bool) {
	if len(l.pendingScopedExprStack) <= 0 {
		return nil, false
	}
	lastIndex := len(l.pendingScopedExprStack) - 1
	defer func() {
		l.pendingScopedExprStack = l.pendingScopedExprStack[:lastIndex]
	}()
	return l.pendingScopedExprStack[len(l.pendingScopedExprStack)-1], true
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

func (l *listener) Result() (ir.File, []*ir.CompileError) {
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
		return
	}

	// declaration is inside block
	rem, err := l.popExpr()
	if err != nil {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression %s", err.Error()))
	}

	// declaration is inside some other expression, so it's an assignment
	l.pendingScopedExprStack = append(l.pendingScopedExprStack, pendingAssign(ir.AssignExpr{
		Range:     ir.Range{},
		IdentName: ctx.IDENTIFIER().GetText(),
		Rhs:       rem,
		Remainder: nil,
	}))
}

func (l *listener) ExitBlockExpr(ctx *parser.BlockExprContext) {
	hasChild := ctx.BlockExpr() != nil
	if !hasChild {
		// then this is the latest expression of the block, so we do nothing
		return
	}

	remainderExpr, err := l.popExpr()
	if err != nil {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression %s", err.Error()))
		return
	}
	scoped, ok := l.popPending()
	var fullExpr ir.Expr
	if ok {
		fullExpr = scoped.assignRemainder(remainderExpr)
	} else {
		latestExpr, err := l.popExpr()
		if err != nil {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression %s", err.Error()))
			return
		}
		fullExpr = ir.UnusedExpr{
			Range:     ir.GetRange(remainderExpr),
			Expr:      latestExpr,
			Remainder: remainderExpr,
		}
	}
	l.expressionStack = append(l.expressionStack, fullExpr)
	return
}

func (l *listener) ExitExpression(ctx *parser.ExpressionContext) {
	if ctx.PrimaryExpr() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}

	var binOp int
	switch {
	case ctx.GetAdd_op() != nil:
		binOp = ctx.GetAdd_op().GetTokenType()
	case ctx.GetMul_op() != nil:
		binOp = ctx.GetMul_op().GetTokenType()
	case ctx.GetRel_op() != nil:
		binOp = ctx.GetRel_op().GetTokenType()
	case ctx.LOGICAL_OR() != nil:
		binOp = ctx.LOGICAL_OR().GetSymbol().GetTokenType()
	case ctx.LOGICAL_AND() != nil:
		binOp = ctx.LOGICAL_AND().GetSymbol().GetTokenType()

	default:
		l.visitErrors = append(l.visitErrors, fmt.Errorf("unexpected binary operator: %v", ctx.GetText()))
		return
	}

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
		Op:    ir.PrimOp(parser.IleTokenInGo[binOp]),
		Lhs:   lhs,
		Rhs:   rhs,
	})
}

func (l *listener) ExitPrimaryExpr(ctx *parser.PrimaryExprContext) {
	if ctx.Operand() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}
}

func (l *listener) ExitOperand(ctx *parser.OperandContext) {
	if ctx.BlockExpr() != nil {
		// then the latest expression on the stack is this one, do nothing
		return
	}
	if ctx.Literal() != nil {
		// we will deal with in ExitLiteral
		return
	}

	// TODO qualified operands!
	if ctx.OperandName() != nil {
		l.expressionStack = append(l.expressionStack, ir.IdentifierLitExpr{
			Range:   intervalTo2Pos(ctx.GetSourceInterval()),
			NameLit: ctx.GetText(),
		})
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
			l.expressionStack = append(l.expressionStack, ir.BasicLitExpr{
				Kind:  token.STRING,
				Value: strings.Trim(strLit.GetText(), "`"),
				Range: intervalTo2Pos(ctx.GetSourceInterval()),
			})
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {
			l.expressionStack = append(l.expressionStack, ir.BasicLitExpr{
				Kind:  token.STRING,
				Value: strings.Trim(strLit.GetText(), "\""),
				Range: intervalTo2Pos(ctx.GetSourceInterval()),
			})
			return
		}
	}

	if ctx.Integer() != nil {
		l.expressionStack = append(l.expressionStack, ir.BasicLitExpr{
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
		Range:   intervalTo2Pos(ctx.GetSourceInterval()),
		NameLit: ctx.IDENTIFIER().GetText(),
		Params:  l.popAllParamDecl(),
	}
	defer func() {
		l.file.Functions = append(l.file.Functions, fun)
	}()

	expr, err := l.popExpr()
	if err != nil {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse an expression, but none found in function %v: %v", fun.NameLit, err))
		return
	}
	fun.BodyLit = expr
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
			typeLit.NameLit = typeName.QualifiedIdent().IDENTIFIER(1).GetText()
		} else {
			typeLit.NameLit = typeName.GetText()
		}

		l.typeStack = append(l.typeStack, typeLit)
	}
	// TODO composite types!
}

//
// Errors
//

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, &ir.CompileError{
		Message: "error at: " + node.GetText(),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
	//l.errors = append(l.errors, &ir.CompileError{
	//	Message: "terminal at: " + node.GetText(),
	//})
}
