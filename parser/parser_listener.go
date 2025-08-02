package parser

import (
	"errors"
	"fmt"
	"go/token"
	"log/slog"
	"slices"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/util"
)

// listener traverses the parse tree
// by using an expressionStack which keeps track of the latest
// successfully evaluated expressions
// when an expect an expression in the latest node, just pop it from
// the stack as it will already have been visited
type listener struct {
	*BaseIleParserListener

	file   ast.File
	errors []ilerr.IleError

	visitErrors []error

	expressionStack util.Stack[ast.Expr]

	blockExprStack          util.Stack[*ast.BlockExpr]
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

type pendingVarDecl struct {
	Range   ast.Range
	Name    string
	TypeAnn ast.Type
	Value   ast.Expr
}

func (p pendingVarDecl) assignBody(expr ast.Expr) ast.Expr {
	// TODO VARDECL here is behaving the same as IR but it is constructed different
	// because it has no 'body' underneath
	//
	// the AST representation of a block should be a list
	// whereas the IR one should remain as is, but we should be able to go AST -> IR
	varDecl := &ast.VarDecl{
		Range:   p.Range,
		Name:    p.Name,
		TypeAnn: p.TypeAnn,
		Value:   expr,
	}
	return varDecl
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

func (l *listener) ExitSourceFile(ctx *SourceFileContext) {
	l.file.Range = intervalTo2Pos(ctx.GetSourceInterval())
}

var restrictedIdentNames = map[string]bool{
	"True":  true,
	"False": true,
}

//
// Expressions
//

func (l *listener) EnterPackageClause(ctx *PackageClauseContext) {
	l.file.PkgName = ctx.IDENTIFIER().GetText()
}

func (l *listener) ExitImportSpec(ctx *ImportSpecContext) {
	if ctx.GetAlias() == nil {
		// TODO allow empty aliases https://github.com/cottand/ile/issues/3
		l.visitErrors = append(l.visitErrors, fmt.Errorf("empty aliases are not supported"))
		return
	}
	imp := ast.Import{
		Range:      intervalTo2Pos(ctx.GetSourceInterval()),
		Alias:      ctx.GetAlias().GetText(),
		ImportPath: strings.Trim(ctx.ImportPath().GetText(), "\""),
	}
	l.file.Imports = append(l.file.Imports, imp)
}

func (l *listener) ExitVarDecl(ctx *VarDeclContext) {
	identNameText := ctx.IDENTIFIER().GetText()

	declPos := getPosInclusiveBetween(ctx.IDENTIFIER(), ctx.ASSIGN())

	// TODO move this to desugar
	if restrictedIdentNames[identNameText] {
		l.errors = append(l.errors, ilerr.New(ilerr.NewRestrictedIdentName{
			Positioner: declPos,
			Name:       identNameText,
		}))
	}

	// declaration is at file source
	if _, ok := ctx.GetParent().GetParent().(*SourceFileContext); ok {
		expr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			return // TODO append an errorDecl node here?
		}
		var typeAnn ast.Type
		if ctx.Type_() != nil {
			var ok bool
			typeAnn, ok = l.typeStack.Pop()
			if !ok {
				l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
			}
		}
		declaration := ast.Declaration{
			Range:    declPos,
			Name:     identNameText,
			Value:    expr,
			TypeAnn:  typeAnn,
			Comments: l.findCommentsPreceding(ctx.GetParser().GetTokenStream(), ctx.IDENTIFIER().GetSymbol()),
		}
		// TODO move this to desugar
		if declaration.Name[0] >= 'A' && declaration.Name[0] <= 'Z' && typeAnn == nil {
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

	// declaration is inside some other expression, so it's a variable declaration
	var typeAnn ast.Type
	if ctx.Type_() != nil {
		var ok bool
		typeAnn, ok = l.typeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
		}
	}
	l.expressionStack.Push(&ast.VarDecl{
		Range:   declPos,
		Name:    identNameText,
		TypeAnn: typeAnn,
		Value:   rem,
	})
}

func (l *listener) ExitBlockExpr(ctx *BlockExprContext) {
	if ctx.BlockExpr() == nil {
		expr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			return
		}
		l.expressionStack.Push(&ast.BlockExpr{
			Range:       getPos(ctx),
			Expressions: []ast.Expr{expr},
		})
		return
	}

	// We should have just finished parsing another blockExpr then.
	// We merge it with the latest expression at put it back in the stack
	expr, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		return
	}
	asBlockExpr, ok := expr.(*ast.BlockExpr)
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expression is not a block expression"))
		return
	}
	// ... and underneath the previous blockExpr, we should have the most recent expression
	expr, ok = l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		return
	}
	expressions := make([]ast.Expr, 0, len(asBlockExpr.Expressions)+1)
	expressions = append(expressions, expr)
	expressions = append(expressions, asBlockExpr.Expressions...)
	l.expressionStack.Push(&ast.BlockExpr{
		Range:       getPos(ctx),
		Expressions: expressions,
	})
}

func (l *listener) ExitBlock(ctx *BlockContext) {
}

func (l *listener) ExitExpression(ctx *ExpressionContext) {
	// Nothing to do here
}

func (l *listener) ExitArithmeticExpr(ctx *ArithmeticExprContext) {
	if ctx.BlockExpr() != nil {
		// then the latest expression on the stack is this one, do nothing
		return
	}
	if ctx.PrimaryExpr() != nil {
		// we will deal with in ExitPrimaryExpr
		return
	}
	if ctx.WhenBlock() != nil {
		return
	}

	var binOp token.Token
	var antlrPos int
	switch {
	case ctx.GetAdd_op() != nil:
		binOp = IleTokenInGo[ctx.GetAdd_op().GetTokenType()]
		antlrPos = ctx.GetAdd_op().GetStart()
	case ctx.GetMul_op() != nil:
		binOp = IleTokenInGo[ctx.GetMul_op().GetTokenType()]
		antlrPos = ctx.GetMul_op().GetStart()
	case ctx.GetRel_op() != nil:
		binOp = IleTokenInGo[ctx.GetRel_op().GetTokenType()]
		antlrPos = ctx.GetRel_op().GetStart()
	case ctx.LOGICAL_OR() != nil:
		binOp = IleTokenInGo[ctx.LOGICAL_OR().GetSymbol().GetTokenType()]
		antlrPos = ctx.LOGICAL_OR().GetSymbol().GetStart()
	case ctx.LOGICAL_AND() != nil:
		binOp = IleTokenInGo[ctx.LOGICAL_AND().GetSymbol().GetTokenType()]
		antlrPos = ctx.LOGICAL_AND().GetSymbol().GetStart()
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

	l.expressionStack.Push(&ast.BinaryExpr{
		Left:     lhs,
		Operator: binOp,
		Right:    rhs,
		Range:    intervalTo2Pos(antlr.Interval{Start: antlrPos, Stop: antlrPos}),
	})
}

func (l *listener) ExitOperand(ctx *OperandContext) {
	if ctx.Literal() != nil {
		// we will deal with in ExitLiteral
		return
	}

	if ctx.OperandName() != nil {
		l.expressionStack.Push(&ast.Identifier{
			Name:  ctx.GetText(),
			Range: getPos(ctx),
		})
		return
	}
}

func (l *listener) ExitPrimaryExpr(ctx *PrimaryExprContext) {
	// list literal case
	if ctx.ListLiteral() != nil {
		// The ExitListLiteral method will handle this
		return
	}

	// function call case (a() or a.b())
	primaryExpr := ctx.PrimaryExpr()
	callArgs := ctx.FnCallArgs()
	if callArgs != nil {
		parsedArgs := callArgs.AllExpression()
		args := make([]ast.Expr, len(parsedArgs))
		// we pop expressionStack from the end, so we must add to the args from the back
		for i, _ := range slices.Backward(parsedArgs) {
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
			callee = &ast.Identifier{
				Name:  name.GetText(),
				Range: getPos(name),
			}
		} else if primaryExpr != nil {
			var ok bool
			callee, ok = l.expressionStack.Pop()
			if !ok {
				l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
			}
		}
		l.expressionStack.Push(&ast.CallExpr{
			Function: callee,
			Args:     args,
			Range:    getPos(ctx),
		})
		return
	}

	// simple qualifier case (a.b)
	if primaryExpr != nil {
		qualifier, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		}
		l.expressionStack.Push(&ast.SelectExpr{
			X:     qualifier,
			Sel:   ctx.IDENTIFIER().GetText(),
			Range: getPos(ctx),
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

type antlrTokenPositioner interface {
	GetStart() antlr.Token
	GetStop() antlr.Token
}
type antlrPositioner interface {
	GetStart() int
	GetStop() int
}

func getPos(i antlrTokenPositioner) ast.Range {
	return ast.Range{
		PosStart: token.Pos(i.GetStart().GetStart()),
		PosEnd:   token.Pos(i.GetStop().GetStop()),
	}
}
func getTokenPos(i antlrPositioner) ast.Range {
	return ast.Range{
		PosStart: token.Pos(i.GetStart()),
		PosEnd:   token.Pos(i.GetStop()),
	}
}

func getPosInclusiveBetween(left antlr.SyntaxTree, right antlr.SyntaxTree) ast.Range {
	return ast.Range{
		PosStart: token.Pos(left.GetSourceInterval().Start),
		PosEnd:   token.Pos(right.GetSourceInterval().Stop),
	}
}

func (l *listener) ExitLiteral(ctx *LiteralContext) {
	if s := ctx.String_(); s != nil {
		if strLit := s.RAW_STRING_LIT(); strLit != nil {
			l.expressionStack.Push(&ast.Literal{
				Value: strings.Trim(strLit.GetText(), "`"),
				Kind:  token.STRING,
				Range: getPos(ctx),
			})
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {
			l.expressionStack.Push(&ast.Literal{
				Value: strings.Trim(strLit.GetText(), "\""),
				Kind:  token.STRING,
				Range: getPos(ctx),
			})
			return
		}
	}

	if ctx.Integer() != nil {
		l.expressionStack.Push(&ast.Literal{
			Value: ctx.Integer().GetText(),
			Kind:  token.INT,
			Range: getPos(ctx),
		})
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

func (l *listener) ExitFunctionDecl(ctx *FunctionDeclContext) {
	//pos := intervalTo2Pos(ctx.GetSourceInterval())
	pos := ast.Range{
		PosStart: token.Pos(ctx.FN().GetSymbol().GetStart()),
		PosEnd:   token.Pos(ctx.Block().GetStop().GetStart()),
	}

	params := l.paramDeclStack.PopAll()
	parsedParams := ctx.Signature().Parameters().AllParameterDecl()
	if len(params) != len(parsedParams) {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("for function %v, expected %d parameters, got %d", ctx.IDENTIFIER(), len(parsedParams), len(params)))
		return
	}

	var parameters []ast.Parameter
	for i, param := range params {
		parsedParam := parsedParams[i]
		parameters = append(parameters, ast.Parameter{
			Range:   getPos(parsedParam),
			Name:    param.str,
			TypeAnn: param.typ,
		})
	}

	var returnType ast.Type
	if ctx.Signature().Result() != nil {
		var ok bool
		returnType, ok = l.functionReturnTypeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("function %v: expected return type but got none", ctx.IDENTIFIER()))
			return
		}
	}

	blockExpr, ok := l.expressionStack.Pop()
	if !ok {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse an expression, but none found in function %v", ctx.IDENTIFIER().GetText()))
		return
	}

	// Create a function type for the type annotation
	funcType := &ast.FuncType{
		Range:      pos,
		ParamTypes: make([]ast.Type, len(parameters)),
		ReturnType: returnType,
	}
	for i, param := range parameters {
		funcType.ParamTypes[i] = param.TypeAnn
	}

	// Add the function declaration to the file
	l.file.Declarations = append(l.file.Declarations, ast.Declaration{
		Range:    intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
		Name:     ctx.IDENTIFIER().GetText(),
		Value:    &ast.FuncLit{Range: pos, Params: parameters, Body: blockExpr},
		TypeAnn:  funcType, // Set the function type as the type annotation
		Comments: l.findCommentsPreceding(ctx.GetParser().GetTokenStream(), ctx.FN().GetSymbol()),
	})
}

func (l *listener) ExitParameterDecl(ctx *ParameterDeclContext) {
	var typ ast.Type
	if ctx.Type_() != nil {
		var ok bool
		typ, ok = l.typeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expected to parse a parameter type for %v but none found", ctx.Type_()))
		}
	}
	argName := ctx.IDENTIFIER().GetText()
	l.paramDeclStack.Push(stringTypePair{str: argName, typ: typ})
}

// ExitResult as in function signature result
// we can expect an ast.Type to be present if a result return type is present in the function
func (l *listener) ExitResult(ctx *ResultContext) {
	if ctx.Type_() != nil {
		parsedT, ok := l.typeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
			return
		}
		l.functionReturnTypeStack.Push(parsedT)
	}
}

func (l *listener) ExitWhenBlock(ctx *WhenBlockContext) {
	cases := ctx.AllWhenCase()
	if len(cases) < 1 {
		l.visitErrors = append(l.visitErrors, fmt.Errorf("expected at least one case enforced by grammar"))
		return
	}
	var astCases []ast.WhenCase
	// we must parse when cases from the bottom since that is the order expressions are pushed to the stack in
	for _, branch := range slices.Backward(cases) {
		astCase, err := l.doWhenCase(branch)
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
	whenExpr := &ast.WhenExpr{
		Target: subjectExpr,
		Cases:  astCases,
		Range:  getTokenPos(ctx.WHEN().GetSymbol()),
	}
	l.expressionStack.Push(whenExpr)
}

func (l *listener) doWhenCase(ctx IWhenCaseContext) (ast.WhenCase, error) {
	// extract expression in LHS of arrow in case
	var caseExpr ast.Expr
	if ctx.ArithmeticExpr() != nil {
		var ok bool
		caseExpr, ok = l.expressionStack.Pop()
		if !ok {
			return ast.WhenCase{}, fmt.Errorf("when case: expression stack is empty")
		}
	} else {
		return ast.WhenCase{}, fmt.Errorf("when case: no arithmetic expression found")
	}

	matchPattern, err := l.doMatchPattern(ctx.MatchPattern())
	if err != nil {
		return ast.WhenCase{}, err
	}
	return ast.WhenCase{
		Range:   getPos(ctx),
		Pattern: matchPattern,
		Body:    caseExpr,
	}, nil
}

func (l *listener) doMatchPattern(ctx IMatchPatternContext) (ast.Type, error) {
	if ctx.Type_() == nil {
		return nil, fmt.Errorf("match pattern: expected type but got none (is this a pattern not implemented?)")
	}
	parsedType, ok := l.typeStack.Pop()
	if !ok {
		return nil, fmt.Errorf("match pattern: expected type but got none (is this a pattern not implemented?)")
	}
	return parsedType, nil
}

func (l *listener) doType(ctx IType_Context) ast.Type {
	typeName := ctx.TypeName()
	if typeName != nil {
		// Create a simple type name node
		return &ast.TypeName{
			Range: getPos(ctx),
			Name:  typeName.GetText(),
		}
	}
	if typeLiteral := ctx.Literal(); typeLiteral != nil {
		type_, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type literal: expression stack is empty"))
			return nil
		}
		// Convert the expression to a type
		if lit, ok := type_.(*ast.Literal); ok {
			return &ast.TypeLiteral{
				Range: ast.RangeOf(type_),
				Value: lit,
			}
		}
		l.visitErrors = append(l.visitErrors, fmt.Errorf("type literal: expected literal but got %T", type_))
		return nil
	}
	l.visitErrors = append(l.visitErrors, fmt.Errorf("type not implemented: '%v'", ctx.GetText()))
	return nil
}

func (l *listener) ExitType_(ctx *Type_Context) {
	t := l.doType(ctx)
	if t != nil {
		l.typeStack.Push(t)
	}
}

//
// Errors
//

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, ilerr.NewSyntax{
		ParserMessage: "error at: " + node.GetText(),
		Positioner:    getTokenPos(node.GetSymbol()),
	})
}

func (l *listener) VisitTerminal(node antlr.TerminalNode) {
}

func (l *listener) ExitListLiteral(ctx *ListLiteralContext) {
	expressions := ctx.AllExpression()
	elements := make([]ast.Expr, len(expressions))

	// We pop expressions from the stack in reverse order since they were pushed in order
	for i, _ := range slices.Backward(expressions) {
		var ok bool
		elements[i], ok = l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("list literal: expression stack is empty"))
			return
		}
	}

	l.expressionStack.Push(&ast.ListLit{
		Elements: elements,
		Range:    getPos(ctx),
	})
}
