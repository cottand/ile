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
	"github.com/cottand/ile/frontend/ir"
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

	file   ir.File
	errors []ilerr.IleError

	visitErrors []error

	expressionStack util.Stack[ir.Expr]

	blockExprStack          util.Stack[ir.Expr]
	paramDeclStack          util.Stack[stringTypePair]
	functionReturnTypeStack util.Stack[ir.Type]
	typeStack               util.Stack[ir.Type]

	pendingScopedExprStack util.Stack[pendingScoped]

	*slog.Logger
}

type stringTypePair struct {
	str string
	typ ir.Type
}

type pendingScoped interface {
	assignBody(expr ir.Expr) ir.Expr
}

type pendingAssign ir.Assign

func (p pendingAssign) assignBody(expr ir.Expr) ir.Expr {
	p.Body = expr
	assign := ir.Assign(p)
	return &assign
}

func (l *listener) result() (ir.File, *ilerr.Errors) {
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
	imp := ir.Import{
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
		if ctx.Type_() != nil {
			type_, ok := l.typeStack.Pop()
			if !ok {
				l.visitErrors = append(l.visitErrors, fmt.Errorf("type stack is empty"))
			} else {
				expr = &ir.Ascribe{Expr: expr, Type_: type_, Range: getTokenPos(ctx.COLON().GetSymbol())}
			}
		}
		declaration := ir.Declaration{
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
	l.pendingScopedExprStack.Push(pendingAssign(ir.Assign{
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
	var fullExpr ir.Expr
	if ok {
		fullExpr = scoped.assignBody(remainderExpr)
	} else {
		latestExpr, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("declaration without expression"))
			return
		}
		fullExpr = &ir.Unused{
			Range: ir.RangeOf(remainderExpr),
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
	if ctx.WhenBlock() != nil {
		return
	}

	var binOp int
	var antlrPos int
	switch {
	case ctx.GetAdd_op() != nil:
		binOp = ctx.GetAdd_op().GetTokenType()
		antlrPos = ctx.GetAdd_op().GetStart()
	case ctx.GetMul_op() != nil:
		binOp = ctx.GetMul_op().GetTokenType()
		antlrPos = ctx.GetMul_op().GetStart()
	case ctx.GetRel_op() != nil:
		binOp = ctx.GetRel_op().GetTokenType()
		antlrPos = ctx.GetRel_op().GetStart()
	case ctx.LOGICAL_OR() != nil:
		binOp = ctx.LOGICAL_OR().GetSymbol().GetTokenType()
		antlrPos = ctx.LOGICAL_OR().GetSymbol().GetStart()
	case ctx.LOGICAL_AND() != nil:
		binOp = ctx.LOGICAL_AND().GetSymbol().GetTokenType()
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

	l.expressionStack.Push(&ir.Call{
		Func:  ir.BinOp(parser.IleTokenInGo[binOp], getPos(ctx)),
		Args:  []ir.Expr{lhs, rhs},
		Range: intervalTo2Pos(antlr.Interval{Start: antlrPos, Stop: antlrPos}),
	})
}

func (l *listener) ExitOperand(ctx *parser.OperandContext) {
	if ctx.Literal() != nil {
		// we will deal with in ExitLiteral
		return
	}

	if ctx.OperandName() != nil {
		l.expressionStack.Push(&ir.Var{
			Name:  ctx.GetText(),
			Range: getPos(ctx),
		})
		return
	}
}

func (l *listener) ExitPrimaryExpr(ctx *parser.PrimaryExprContext) {
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
		args := make([]ir.Expr, len(parsedArgs))
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
		var callee ir.Expr
		if name := ctx.OperandName(); name != nil {
			callee = &ir.Var{
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
		l.expressionStack.Push(&ir.Call{
			Func:  callee,
			Args:  args,
			Range: getPos(ctx),
		})
		return
	}

	// simple qualifier case (a.b)
	if primaryExpr != nil {
		qualifier, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("expression stack is empty"))
		}
		l.expressionStack.Push(&ir.RecordSelect{
			Record: qualifier,
			Label:  ctx.IDENTIFIER().GetText(),
			Range:  getPos(ctx),
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

type antlrTokenPositioner interface {
	GetStart() antlr.Token
	GetStop() antlr.Token
}
type antlrPositioner interface {
	GetStart() int
	GetStop() int
}

func getPos(i antlrTokenPositioner) ir.Range {
	return ir.Range{
		PosStart: token.Pos(i.GetStart().GetStart()),
		PosEnd:   token.Pos(i.GetStop().GetStop()),
	}
}
func getTokenPos(i antlrPositioner) ir.Range {
	return ir.Range{
		PosStart: token.Pos(i.GetStart()),
		PosEnd:   token.Pos(i.GetStop()),
	}
}

func getPosInclusiveBetween(left antlr.SyntaxTree, right antlr.SyntaxTree) ir.Range {
	return ir.Range{
		PosStart: token.Pos(left.GetSourceInterval().Start),
		PosEnd:   token.Pos(right.GetSourceInterval().Stop),
	}
}

func (l *listener) ExitLiteral(ctx *parser.LiteralContext) {
	if s := ctx.String_(); s != nil {
		if strLit := s.RAW_STRING_LIT(); strLit != nil {

			l.expressionStack.Push(ir.StringLiteral(
				strings.Trim(strLit.GetText(), "`"),
				getPos(ctx),
			))
			return
		}
		if strLit := s.INTERPRETED_STRING_LIT(); strLit != nil {

			l.expressionStack.Push(ir.StringLiteral(
				strings.Trim(strLit.GetText(), "\""),
				getPos(ctx),
			))
			return
		}
	}

	if ctx.Integer() != nil {
		l.expressionStack.Push(ir.IntLiteral(
			ctx.Integer().GetText(),
			getPos(ctx),
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
	var tAnnotations = make([]ir.Type, len(params))
	for i, param := range params {
		paramNames[i] = param.str
		tAnnotations[i] = param.typ
	}
	var retT ir.Type = nil
	if ctx.Signature().Result() != nil {
		var ok bool
		retT, ok = l.functionReturnTypeStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("function %v: expected return type but got none", ctx.IDENTIFIER()))
			return
		}
	}
	fn := &ir.Func{
		ArgNames: paramNames,
		Body:     nil,
		Range:    pos,
	}
	type_ := &ir.FnType{
		Args:   tAnnotations,
		Return: retT,
		Range:  pos,
	}
	decl := ir.Declaration{
		Range:    intervalTo2Pos(ctx.IDENTIFIER().GetSourceInterval()),
		Name:     ctx.IDENTIFIER().GetText(),
		E:        &ir.Ascribe{Expr: fn, Type_: type_, Range: getPos(ctx)},
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
	var typ ir.Type = nil
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
		l.expressionStack.Push(&ir.ErrorExpr{
			Range:  getTokenPos(ctx.WHEN().GetSymbol()),
			Syntax: ctx.GetText(),
		})
		return
	}
	var astCases []ir.WhenCase
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
	whenExpr := &ir.WhenMatch{
		Value:      subjectExpr,
		Cases:      astCases,
		Positioner: getTokenPos(ctx.WHEN().GetSymbol()),
	}
	l.expressionStack.Push(whenExpr)
}

func (l *listener) doWhenCase(ctx parser.IWhenCaseContext) (ir.WhenCase, error) {
	// extract expression in LHS of arrow in case
	var caseExpr ir.Expr
	if ctx.ArithmeticExpr() != nil {
		var ok bool
		caseExpr, ok = l.expressionStack.Pop()
		if !ok {
			caseExpr = &ir.ErrorExpr{
				Syntax: ctx.GetText(),
				Range:  getPos(ctx.ArithmeticExpr()),
			}
		}
	} else {
		caseExpr = &ir.ErrorExpr{
			Syntax: ctx.GetText(),
			Range:  getTokenPos(ctx.ARROW().GetSymbol()),
		}
	}

	matchPattern, err := l.doMatchPattern(ctx.MatchPattern())
	if err != nil {
		return ir.WhenCase{}, err
	}
	return ir.WhenCase{
		Pattern: matchPattern,
		Value:   caseExpr,
	}, nil
}

func (l *listener) doMatchPattern(ctx parser.IMatchPatternContext) (ir.MatchPattern, error) {
	if ctx.Type_() == nil {
		return nil, fmt.Errorf("match pattern: expected type but got none (is this a pattern not implemented?)")
	}
	parsedType, ok := l.typeStack.Pop()
	if !ok {
		return nil, fmt.Errorf("match pattern: expected type but got none (is this a pattern not implemented?)")
	}
	return &ir.MatchTypePattern{
		Type_:      parsedType,
		Positioner: getPos(ctx.Type_()),
	}, nil
}

func (l *listener) doType(ctx parser.IType_Context) ir.Type {
	typeName := ctx.TypeName()
	if typeName != nil {
		type_ := &ir.TypeName{
			Name:  typeName.GetText(),
			Range: getPos(ctx),
		}
		if typeName.QualifiedIdent() != nil {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("qualified type names not implemented: %s", ctx.GetText()))
			//type_.Package = typeName.QualifiedIdent().IDENTIFIER(0).GetText()
			//type_.Name = typeName.QualifiedIdent().IDENTIFIER(1).GetText()
		}

		return type_
	}
	if typeLiteral := ctx.Literal(); typeLiteral != nil {
		type_, ok := l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type literal: expression stack is empty"))
			return &ir.NothingType{Positioner: getPos(ctx)}
		}
		asLit, ok := type_.(*ir.Literal)
		// when well written, this should be a type literal in the stack, because more complex expressions
		// cannot appear in the position of a type
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("type literal: expected literal but got %T", type_))
			return &ir.NothingType{Positioner: getPos(ctx)}
		}
		return asLit
	}
	l.visitErrors = append(l.visitErrors, fmt.Errorf("type not implemented: '%v'", ctx.GetText()))
	return &ir.NothingType{Positioner: getPos(ctx)}
}

func (l *listener) ExitType_(ctx *parser.Type_Context) {
	t := l.doType(ctx)
	l.typeStack.Push(t)
}


//
// Errors
//

func (l *listener) VisitErrorNode(node antlr.ErrorNode) {
	l.errors = append(l.errors, ilerr.NewParse{
		ParserMessage: "error at: " + node.GetText(),
		Positioner:    getTokenPos(node.GetSymbol()),
	})
}
func (l *listener) VisitTerminal(node antlr.TerminalNode) {
}

func (l *listener) ExitListLiteral(ctx *parser.ListLiteralContext) {
	expressions := ctx.AllExpression()
	args := make([]ir.Expr, len(expressions))

	// We pop expressions from the stack in reverse order since they were pushed in order
	for i, _ := range slices.Backward(expressions) {
		var ok bool
		args[i], ok = l.expressionStack.Pop()
		if !ok {
			l.visitErrors = append(l.visitErrors, fmt.Errorf("list literal: expression stack is empty"))
			return
		}
	}

	l.expressionStack.Push(&ir.ListLiteral{
		Args:       args,
		Positioner: getPos(ctx),
	})
}
