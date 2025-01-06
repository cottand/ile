package frontend

import (
	"errors"
	"fmt"
	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/parser"
	"github.com/cottand/ile/util"
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

	file   ast.File
	errors []*ast.CompileError

	visitErrors []error

	expressionStack util.Stack[ast.Expr]

	blockExprStack          util.Stack[ast.Expr]
	paramDeclStack          util.Stack[string]
	functionReturnTypeStack util.Stack[string]
	typeStack               util.Stack[string]

	pendingScopedExprStack util.Stack[pendingScoped]
}

type pendingScoped interface {
	assignBody(expr ast.Expr) ast.Expr
}

type pendingAssign ast.Assign

func (p pendingAssign) assignBody(expr ast.Expr) ast.Expr {
	p.Body = expr
	assign := ast.Assign(p)
	return &assign
}

//func (l *listener) popAllParamDecl() []ast.ParamDecl {
//	defer func() {
//		l.paramDeclStack = make([]ast.ParamDecl, 0)
//	}()
//	return l.paramDeclStack
//}
//
//func (l *listener) popFunctionReturnType() (ast.Type, error) {
//	if len(l.functionReturnTypeStack) <= 0 {
//		return nil, errors.New("expected to have successfully parsed a functionReturnType, but none were found")
//	}
//	lastIndex := len(l.functionReturnTypeStack) - 1
//	defer func() {
//		l.functionReturnTypeStack = l.functionReturnTypeStack[:lastIndex]
//	}()
//	return l.functionReturnTypeStack[len(l.functionReturnTypeStack)-1], nil
//}
//
//func (l *listener) popType() (ast.Type, error) {
//	if len(l.typeStack) <= 0 {
//		return nil, errors.New("expected to have successfully parsed a type, but none were found")
//	}
//	lastIndex := len(l.typeStack) - 1
//	defer func() {
//		l.typeStack = l.typeStack[:lastIndex]
//	}()
//	return l.typeStack[lastIndex], nil
//}

func (l *listener) Result() (ast.File, []*ast.CompileError) {
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
		expr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			return // TODO append an errorDecl node here?
		}
		l.file.Declarations = append(l.file.Declarations, ast.Declaration{
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
			Name:  ctx.IDENTIFIER().GetText(),
			E:     expr,
		})
		return
	}

	// declaration is inside block
	rem, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression"))
	}

	// declaration is inside some other expression, so it's an assignment
	l.pendingScopedExprStack.Push(pendingAssign(ast.Assign{
		Range: ast.Range{},
		Var:   ctx.IDENTIFIER().GetText(),
		Value: rem,
		Body:  nil,
	}))
}

func (l *listener) ExitBlockExpr(ctx *parser.BlockExprContext) {
	hasChild := ctx.BlockExpr() != nil
	if !hasChild {
		// then this is the latest expression of the block, so we do nothing
		return
	}

	remainderExpr, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression"))
		return
	}
	scoped, ok := l.pendingScopedExprStack.Pop()
	var fullExpr ast.Expr
	if ok {
		fullExpr = scoped.assignBody(remainderExpr)
	} else {
		latestExpr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression"))
			return
		}
		fullExpr = &ast.Unused{
			Range: ast.GetRange(remainderExpr),
			Value: latestExpr,
			Body:  remainderExpr,
		}
	}
	l.expressionStack.Push(fullExpr)
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

	rhs, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		return
	}
	lhs, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		return
	}

	l.expressionStack.Push(&ast.Call{
		Func:  ast.BinOp(parser.IleTokenInGo[binOp], intervalTo2Pos(ctx.GetSourceInterval())),
		Args:  []ast.Expr{lhs, rhs},
		Range: ast.Range{},
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

		l.expressionStack.Push(&ast.Var{
			Name:  ctx.GetText(),
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
		})
		return
	}
}

func intervalTo2Pos(i antlr.Interval) ast.Range {
	return ast.Range{
		PosStart: token.Pos(i.Start),
		PosEnd:   token.Pos(i.Stop),
	}
}

func (l *listener) ExitLiteral(ctx *parser.LiteralContext) {
	if s := ctx.String_(); s != nil {
		if strLit := s.RAW_STRING_LIT(); strLit != nil {

			l.expressionStack.Push(ast.StringLiteral(
				strings.Trim(strLit.GetText(), "`"),
				intervalTo2Pos(ctx.GetSourceInterval()),
			))
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {

			l.expressionStack.Push(ast.StringLiteral(
				strings.Trim(strLit.GetText(), "\""),
				intervalTo2Pos(ctx.GetSourceInterval()),
			))
			return
		}
	}

	if ctx.Integer() != nil {

		l.expressionStack.Push(ast.IntLiteral(
			ctx.Integer().GetText(),
			intervalTo2Pos(ctx.GetSourceInterval()),
		))
	}
}

//
// Functions
//

func (l *listener) ExitFunctionDecl(ctx *parser.FunctionDeclContext) {
	fn := &ast.Func{
		ArgNames: l.paramDeclStack.PopAll(),
		Body:     nil,
		Range:    intervalTo2Pos(ctx.GetSourceInterval()),
	}
	decl := ast.Declaration{
		Range:  intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
		Name:   ctx.IDENTIFIER().GetText(),
		E:      fn,
	}
	defer func() {
		l.file.Declarations = append(l.file.Declarations, decl)
	}()

	expr, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse an expression, but none found in function %v", decl.Name))
		return
	}
	fn.Body = expr
	if ctx.Signature().Result() != nil {
		_, ok := l.functionReturnTypeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse a function return_type but none found in function %v", decl.Name))
			return
		}
		//decl.Result = retT TODO type annotations for functions!
	}
}

func (l *listener) ExitParameterDecl(ctx *parser.ParameterDeclContext) {
	_, ok := l.typeStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
		return
	}
	l.paramDeclStack.Push(ctx.IDENTIFIER().GetText())
	// TODO function type annotations!
	//l.paramDeclStack = append(l.paramDeclStack, ast.ParamDecl{
	//	Range: intervalTo2Pos(ctx.GetSourceInterval()),
	//	Name: ast.Ident{
	//		Range: intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
	//		Name:  ctx.IDENTIFIER().GetText(),
	//	},
	//	DeclaredT: typ,
	//})
}

// ExitResult as in function signature result
// we can expect an ast.Type to be present if a Result return type is present in the function
func (l *listener) ExitResult(ctx *parser.ResultContext) {
	if ctx.Type_() != nil {
		parsedT, ok := l.typeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
			return
		}
		l.functionReturnTypeStack.Push(parsedT)
	}
}

func (l *listener) ExitType_(ctx *parser.Type_Context) {
	typeName := ctx.TypeName()
	l.typeStack.Push(typeName.GetText())
	// TODO type annotations
	//if typeName != nil {
	//	typeLit := ast.TypeLit{
	//		Range: intervalTo2Pos(ctx.GetSourceInterval()),
	//	}
	//	if typeName.QualifiedIdent() != nil {
	//		typeLit.Package = typeName.QualifiedIdent().IDENTIFIER(0).GetText()
	//		typeLit.NameLit = typeName.QualifiedIdent().IDENTIFIER(1).GetText()
	//	} else {
	//		typeLit.NameLit = typeName.GetText()
	//	}
	//
	//	l.typeStack = append(l.typeStack, typeLit)
	//}
	// TODO composite types!
}

//
// Errors
//

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, &ast.CompileError{
		Message: "error at: " + node.GetText(),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
	//l.errors = append(l.errors, &ast.CompileError{
	//	Message: "terminal at: " + node.GetText(),
	//})
}
