package frontend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"go/token"
	"log/slog"
	"slices"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/parser"
	"github.com/cottand/ile/util"
)

// listener traverses the parse tree
// by using an expressionStack which keeps track of the latest
// successfully evaluated expressions
// when an expect an expression in the latest node, just pop it from
// the stack as it will already have been visited
type listener struct {
	*parser.BaseIleParserListener

	file   ast.File
	errors []ilerr.IleError

	visitErrors []error

	expressionStack util.Stack[ast.Expr]

	blockExprStack          util.Stack[ast.Expr]
	paramDeclStack          util.Stack[stringTypePair]
	functionReturnTypeStack util.Stack[ast.Type]
	typeStack               util.Stack[ast.Type]

	pendingScopedExprStack util.Stack[pendingScoped]

	*slog.Logger
}

type stringTypePair struct {
	str string
	typ ast.Type
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

func (l *listener) result() (ast.File, *ilerr.Errors) {
	result := ilerr.Errors{}
	if len(l.errors) > 0 {
		return l.file, result.With(l.errors...)
	}
	return l.file, &result
}

func (l *listener) VisitErrors() error {
	return errors.Join(l.visitErrors...)
}

func (l *listener) ExitSourceFile(ctx *parser.SourceFileContext) {
	l.file.Range = intervalTo2Pos(ctx.GetSourceInterval())
}

var restrictedIdentNames = map[string]bool{
	"True":  true,
	"False": true,
}

//
// Expressions
//

func (l *listener) EnterPackageClause(ctx *parser.PackageClauseContext) {
	l.file.PkgName = ctx.IDENTIFIER().GetText()
}

func (l *listener) ExitImportSpec(ctx *parser.ImportSpecContext) {
	if ctx.GetAlias() == nil {
		// TODO allow empty aliases https://github.com/cottand/ile/issues/3
		l.visitErrors = append(l.visitErrors, fmt.Errorf("empty aliases are not supported"))
		return
	}
	imp := ast.Import{
		Positioner: intervalTo2Pos(ctx.GetSourceInterval()),
		// TODO allow dot aliases https://github.com/cottand/ile/issues/4
		Alias:      ctx.GetAlias().GetText(),
		ImportPath: strings.Trim(ctx.ImportPath().GetText(), "\""),
	}
	l.file.Imports = append(l.file.Imports, imp)
}

func (l *listener) ExitVarDecl(ctx *parser.VarDeclContext) {
	identNameText := ctx.IDENTIFIER().GetText()

	declPos := getPosInclusiveBetween(ctx.IDENTIFIER(), ctx.ASSIGN())

	if restrictedIdentNames[identNameText] {
		l.errors = append(l.errors, ilerr.New(ilerr.NewRestrictedIdentName{
			Positioner: declPos,
			Name:       identNameText,
		}))
	}

	// declaration is at file source
	if _, ok := ctx.GetParent().GetParent().(*parser.SourceFileContext); ok {
		expr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			return // TODO append an errorDecl node here?
		}
		declaration := ast.Declaration{
			Range:    declPos,
			Name:     identNameText,
			E:        expr,
			Comments: l.findCommentsPreceding(ctx.GetParser().GetTokenStream(), ctx.IDENTIFIER().GetSymbol()),
		}
		if declaration.IsPublic() && ctx.Type_() == nil {
			l.errors = append(l.errors, ilerr.NewMissingTypeAnnotationInPublicDeclaration{
				Positioner: declaration,
				DeclName:   identNameText,
			})
		}
		l.file.Declarations = append(l.file.Declarations, declaration)
		return
	}

	// declaration is inside block
	rem, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression"))
	}

	// declaration is inside some other expression, so it's an assignment
	l.pendingScopedExprStack.Push(pendingAssign(ast.Assign{
		Range: declPos,
		Var:   identNameText,
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
			Range: ast.RangeOf(remainderExpr),
			Value: latestExpr,
			Body:  remainderExpr,
		}
	}
	l.expressionStack.Push(fullExpr)
	return
}

func (l *listener) ExitExpression(ctx *parser.ExpressionContext) {

}
func (l *listener) ExitArithmeticExpr(ctx *parser.ArithmeticExprContext) {
	if ctx.BlockExpr() != nil {
		// then the latest expression on the stack is this one, do nothing
		return
	}
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

func (l *listener) ExitOperand(ctx *parser.OperandContext) {
	if ctx.Literal() != nil {
		// we will deal with in ExitLiteral
		return
	}

	if ctx.OperandName() != nil {
		l.expressionStack.Push(&ast.Var{
			Name:  ctx.GetText(),
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
		})
		return
	}
}

func (l *listener) ExitPrimaryExpr(ctx *parser.PrimaryExprContext) {
	// function call case (a() or a.b())
	primaryExpr := ctx.PrimaryExpr()
	callArgs := ctx.FnCallArgs()
	if callArgs != nil {
		parsedArgs := callArgs.AllExpression()
		args := make([]ast.Expr, len(parsedArgs))
		for i, _ := range parsedArgs {
			var ok bool
			args[i], ok = l.expressionStack.Pop()
			if !ok {
				l.visitErrors = append(l.visitErrors, fmt.Errorf("fn args: expression stack is empty"))
				return
			}
		}
		// callee must be popped after the arguments as it was parsed first
		var callee ast.Expr
		if name := ctx.OperandName(); name != nil {
			callee = &ast.Var{
				Name:  name.GetText(),
				Range: intervalTo2Pos(name.GetSourceInterval()),
			}
		} else if primaryExpr != nil {
			var ok bool
			callee, ok = l.expressionStack.Pop()
			if !ok {
				l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			}
		}
		l.expressionStack.Push(&ast.Call{
			Func:  callee,
			Args:  args,
			Range: intervalTo2Pos(ctx.GetSourceInterval()),
		})
		return
	}

	// simple qualifier case (a.b)
	if primaryExpr != nil {
		qualifier, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		}
		l.expressionStack.Push(&ast.RecordSelect{
			Record: qualifier,
			Label:  ctx.IDENTIFIER().GetText(),
			Range:  intervalTo2Pos(ctx.GetSourceInterval()),
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

func getPos(i antlr.SyntaxTree) ast.Range {
	interval := i.GetSourceInterval()
	return ast.Range{
		PosStart: token.Pos(interval.Start),
		PosEnd:   token.Pos(interval.Stop),
	}
}

func getPosInclusiveBetween(left antlr.SyntaxTree, right antlr.SyntaxTree) ast.Range {
	return ast.Range{
		PosStart: token.Pos(left.GetSourceInterval().Start),
		PosEnd:   token.Pos(right.GetSourceInterval().Stop),
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

// findCommentsPreceding can be called when inside a function to find immediately adjacent comments.
// Comments are usually at the index of FN - 2
func (l *listener) findCommentsPreceding(stream antlr.TokenStream, t antlr.Token) []string {
	currentToken := t
	prevToken := stream.Get(currentToken.GetTokenIndex() - 2)
	var totalComment []string
	for {
		// previous line is a comment and is immediately before
		if strings.HasPrefix(prevToken.GetText(), "//") && prevToken.GetLine() == currentToken.GetLine()-1 {
			l.Debug("preceding comment line found", "line", prevToken.GetLine())

			trimmed := strings.TrimPrefix(prevToken.GetText(), "//")
			totalComment = append(totalComment, trimmed)

			currentToken = prevToken
			prevToken = stream.Get(currentToken.GetTokenIndex() - 2)
		} else {
			break
		}
	}

	// we found comments by working our way upwards, so now we must put them in the right order
	slices.Reverse(totalComment)
	return totalComment
}

func (l *listener) ExitFunctionDecl(ctx *parser.FunctionDeclContext) {
	pos := intervalTo2Pos(ctx.GetSourceInterval())

	params := l.paramDeclStack.PopAll()
	parsedParams := ctx.Signature().Parameters().AllParameterDecl()
	if len(params) != len(parsedParams) {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("for function %v, expected %d parameters, got %d", ctx.IDENTIFIER(), len(parsedParams), len(params)))
		return
	}

	var paramNames = make([]string, len(params))
	var tAnnotations = make([]ast.Type, len(params))
	for i, param := range params {
		paramNames[i] = param.str
		tAnnotations[i] = param.typ
	}
	var retT ast.Type = nil
	if ctx.Signature().Result() != nil {
		var ok bool
		retT, ok = l.functionReturnTypeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("function %v: expected return type but got none", ctx.IDENTIFIER()))
			return
		}
	}
	fn := &ast.Func{
		ArgNames: paramNames,
		Body:     nil,
		Range:    pos,
	}
	type_ := &ast.FnType{
		Args:   tAnnotations,
		Return: retT,
		Range:  pos,
	}
	decl := ast.Declaration{
		Range:    intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
		Name:     ctx.IDENTIFIER().GetText(),
		E:        &ast.Ascribe{Expr: fn, Type_: type_},
		Comments: l.findCommentsPreceding(ctx.GetParser().GetTokenStream(), ctx.FN().GetSymbol()),
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
}

func (l *listener) ExitParameterDecl(ctx *parser.ParameterDeclContext) {
	var typ ast.Type = nil
	if ctx.Type_() != nil {
		var ok bool
		typ, ok = l.typeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse a parameter type for %v but none found", ctx.Type_()))
		}
	}
	argName := ctx.IDENTIFIER().GetText()
	l.paramDeclStack.Push(stringTypePair{str: argName, typ: typ})
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
// we can expect an ast.Type to be present if a result return type is present in the function
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

func (l *listener) ExitWhenBlock(ctx *parser.WhenBlockContext) {
	cases := ctx.AllWhenCase()
	if len(cases) < 1 {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected at least one case enforced by grammar"))
		l.expressionStack.Push(&ast.ErrorExpr{
			Range:  getPos(ctx.WHEN()),
			Syntax: ctx.GetText(),
		})
		return
	}
	var astCases []ast.WhenCase
	// we must parse when cases from the bottom since that is the order expressions are pushed to the stack in

	for i := len(cases) - 1; i >= 0; i-- {
		astCase, err := l.doWhenCase(cases[i])
		if err != nil {
			l.visitErrors = append(l.visitErrors, err)
			continue
		}
		astCases = append(astCases, astCase)
	}
	// cases where reversed, let's order again
	slices.Reverse(astCases)

	// after we've popped the cases, the top expression should be the when subject
	subjectExpr, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse a subject expression but none found"))
		return
	}
	whenExpr := &ast.WhenMatch{
		Value:      subjectExpr,
		Cases:      astCases,
		Positioner: getPos(ctx.WHEN()),
	}
	l.expressionStack.Push(whenExpr)
}

func (l *listener) doWhenCase(ctx parser.IWhenCaseContext) (ast.WhenCase, error) {
	// extract expression in LHS of arrow in case
	var caseExpr ast.Expr
	if ctx.ArithmeticExpr() != nil {
		var ok bool
		caseExpr, ok = l.expressionStack.Pop()
		if !ok {
			caseExpr = &ast.ErrorExpr{
				Syntax: ctx.GetText(),
				Range:  getPos(ctx.ArithmeticExpr()),
			}
		}
	} else {
		caseExpr = &ast.ErrorExpr{
			Syntax: ctx.GetText(),
			Range:  getPos(ctx.ARROW()),
		}
	}

	matchPattern, err := l.doMatchPattern(ctx.MatchPattern())
	if err != nil {
		return ast.WhenCase{}, err
	}
	return ast.WhenCase{
		Pattern: matchPattern,
		Value:   caseExpr,
	}, nil
}

func (l *listener) doMatchPattern(ctx parser.IMatchPatternContext) (ast.MatchPattern, error) {
	panic("implement me")
}

func (l *listener) ExitType_(ctx *parser.Type_Context) {
	typeName := ctx.TypeName()
	if typeName != nil {
		typeLit := &ast.TypeName{
			Name: typeName.GetText(),
			Positioner:      intervalTo2Pos(ctx.GetSourceInterval()),
		}
		if typeName.QualifiedIdent() != nil {
			panic("qualified type names not implemented")
			//typeLit.Package = typeName.QualifiedIdent().IDENTIFIER(0).GetText()
			//typeLit.Name = typeName.QualifiedIdent().IDENTIFIER(1).GetText()
		} else {
			//typeLit.Name = typeName.GetText()
		}

		l.typeStack.Push(typeLit)
		return
	}
	panic(fmt.Sprintf("more complicated types not implemented: '%v'", ctx.TypeName()))
	// TODO type annotations
	// TODO composite types!
}

//
// Errors
//

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, ilerr.NewParse{
		ParserMessage: "error at: " + node.GetText(),
		Positioner:    getPos(node),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
}
