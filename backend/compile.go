package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/util"
	goast "go/ast"
	"go/token"
	"reflect"
)

const goVersion = "1.23.3"

// will need to handle more concrete tags, like random strings and specific ints,
// but for now we'll just do the basic types
func typeTagToGoType(t string) (goType string, ok bool) {
	switch t {
	case ir.IntTypeName:
		return "int64", true
	case ir.StringTypeName:
		return "string", true
	case ir.FloatTypeName:
		return "float64", true
	case ir.BoolTypeName, ir.TrueName, ir.FalseName:
		return "bool", true
	}
	return "", false
}

var ileToGoVars = map[string]string{
	"True":  "true",
	"False": "false",
}

var ileToGoOperators = map[string]token.Token{
	"+":  token.ADD,
	"/":  token.QUO,
	"%":  token.REM,
	"-":  token.SUB,
	"*":  token.MUL,
	">":  token.GTR,
	"<":  token.LSS,
	"<=": token.LEQ,
	">=": token.GEQ,
	"==": token.EQL,
	"!=": token.NEQ,
}

func nearestGoType(lit *ir.Literal) (goType string, err error) {
	switch lit.Kind {
	case token.INT:
		// TODO we would like to narrow types here (like 3 becomes uint8, not int64)
		return "int64", nil
	default:
		return "", fmt.Errorf("nearestGoType: unexpected literal type %s", lit.Kind.String())
	}
}

func isSuitableForGoConst(t ir.Type) bool {
	_, ok := t.(*ir.Literal)
	return ok
}

type Transpiler struct {
	types       *types.TypeCtx
	currentExpr ir.Expr
	// inFunctionSignature is used during transpileType to determine if we can use const types or not
	inFunctionSignature bool
}

func NewTranspiler(typeCtx *types.TypeCtx) *Transpiler {
	return &Transpiler{
		types: typeCtx,
	}
}

func (tp *Transpiler) TranspilePackage(name string, syntax []ir.File) ([]goast.File, error) {
	var decls []goast.Decl
	var err error

	//astDecls := pkg.Declarations()
	// TODO multi-file https://github.com/cottand/ile/issues/10
	if len(syntax) < 1 {
		panic("empty package")
		return nil, nil
	}
	currentFile := syntax[0]
	astDecls := currentFile.Declarations

	goImports := goast.GenDecl{Tok: token.IMPORT}

	for _, import_ := range currentFile.GoImports {
		goImports.Specs = append(goImports.Specs, &goast.ImportSpec{
			Name: goast.NewIdent(import_.Alias),
			Path: &goast.BasicLit{Value: "\"" + import_.ImportPath + "\"", Kind: token.STRING},
		})
	}
	decls = append(decls, &goImports)

	if len(astDecls) > 0 {
		declarations, err := tp.transpileDeclarations(astDecls)
		if err != nil {
			return nil, err
		}
		for _, decl := range declarations {
			decls = append(decls, &decl)
		}
	}
	functions, err := tp.transpileFunctionDecls(astDecls)
	if err != nil {
		return nil, err
	}
	if len(tp.types.Failures) != 0 {
		err = fmt.Errorf("failure during secondary pass of type inference: %s", errors.Join(tp.types.Failures...))
	}
	decls = append(decls, functions...)
	return []goast.File{{
		Name:      &goast.Ident{Name: name},
		GoVersion: goVersion,
		Decls:     decls,
	}}, err
}

// transpileDeclarations works at the top level of the file
func (tp *Transpiler) transpileDeclarations(vars []ir.Declaration) ([]goast.GenDecl, error) {
	goDecls := make([]goast.Spec, 0)
	goConstDecls := make([]goast.Spec, 0)
	errs := make([]error, 0)
	for _, decl := range vars {
		// we skip top-level functions, as they are transpiled separately
		_, ok := unwrapAscribe(decl.E).(*ir.Func)
		if ok {
			continue
		}

		value, err := tp.transpileExpr(decl.E)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		var type_ goast.Expr
		// Always use the inferred type for variables. If the declared type was different, the frontend
		// would have failed to typecheck, so it is safe to assume that the inferred type is the correct one.
		declType := tp.types.TypeOf(decl.E)
		if isSuitableForGoConst(declType) {
			// we do not explicitly declare the type of a Go const, so that it remains as a Go 'untyped' const type
			goConstDecls = append(goConstDecls, &goast.ValueSpec{
				Names:  []*goast.Ident{goast.NewIdent(decl.Name)},
				Values: []goast.Expr{value},
			})
			continue
		}
		type_, err = tp.transpileType(declType)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		spec := &goast.ValueSpec{
			Names:  []*goast.Ident{goast.NewIdent(decl.Name)},
			Values: []goast.Expr{value},
			Type:   type_,
		}
		goDecls = append(goDecls, spec)
	}
	joined := errors.Join(errs...)
	return []goast.GenDecl{
		{
			Specs: goDecls,
			Tok:   token.VAR,
		},
		{
			Specs: goConstDecls,
			Tok:   token.CONST,
		},
	}, joined
}

func unwrapAscribe(expr ir.Expr) ir.Expr {
	if ascribe, ok := expr.(*ir.Ascribe); ok {
		return unwrapAscribe(ascribe.Expr)
	}
	return expr
}

func (tp *Transpiler) transpileFunctionDecls(fs []ir.Declaration) ([]goast.Decl, error) {
	var goDecls []goast.Decl
	var errs []error
	for _, irFunc := range fs {
		if fn, ok := unwrapAscribe(irFunc.E).(*ir.Func); ok {
			paramDecl, err := tp.transpileParameterDecls(irFunc, fn)
			if err != nil {
				errs = append(errs, err)
				continue
			}

			body, err := tp.transpileExpressionToStatements(fn.Body, "return")
			if err != nil {
				errs = append(errs, err)
				continue
			}
			var resultList *goast.FieldList
			t, ok := tp.types.TypeOf(fn).(*ir.FnType)
			if !ok {
				errs = append(errs, errors.New("expected a function type"))
			}
			if t.Return != ir.UnitType {
				tp.inFunctionSignature = true
				resultType, err := tp.transpileType(t.Return)
				tp.inFunctionSignature = false
				if err != nil {
					errs = append(errs, err)
					continue
				}
				resultList = &goast.FieldList{
					List: []*goast.Field{{
						Type: resultType,
					}},
				}
			}
			goDecl := goast.FuncDecl{
				Name: goast.NewIdent(irFunc.Name),
				Type: &goast.FuncType{
					Params:  &paramDecl,
					Results: resultList,
				},
				Body: &goast.BlockStmt{List: body},
			}
			goDecls = append(goDecls, &goDecl)
		}
	}
	return goDecls, errors.Join(errs...)
}

func (tp *Transpiler) transpileExpr(expr ir.Expr) (goast.Expr, error) {
	expr = unwrapAscribe(expr)
	oldExpr := tp.currentExpr
	tp.currentExpr = expr
	defer func() { tp.currentExpr = oldExpr }()

	switch e := expr.(type) {
	case *ir.Literal:
		switch e.Kind {
		case token.STRING:
			return &goast.BasicLit{
				Kind: e.Kind,
				// TODO reconsider escaping!
				Value: "`" + e.Syntax + "`",
			}, nil
		case token.INT:
			return &goast.BasicLit{
				Kind:  e.Kind,
				Value: e.Syntax,
			}, nil
		default:
			return nil, fmt.Errorf("for basicLit expr, unexpected token %v for type %v", e.Kind.String(), reflect.TypeOf(expr))
		}

	case *ir.Var:
		if goVar := ileToGoVars[e.Name]; goVar != "" {
			return goast.NewIdent(goVar), nil
		}
		return goast.NewIdent(e.Name), nil

		// Go does not have ternary operators or anything that lets us inline logic into an expression,
		// so we inline a function call
	case *ir.RecordSelect:
		selected, err := tp.transpileExpr(e.Record)
		if err != nil {
			return nil, fmt.Errorf("for record, failed to transpile record: %v", err)
		}

		return &goast.SelectorExpr{
			X:   selected,
			Sel: goast.NewIdent(e.Label),
		}, nil
	case *ir.Call:
		if asVar, ok := e.Func.(*ir.Var); ok {
			operator, isOperator := ileToGoOperators[asVar.Name]
			if isOperator {
				switch len(e.Args) {
				case 0:
					panic("TODO nullary operators")
				case 1:
					panic("TODO unary operators")
				case 2:
					lhs, err1 := tp.transpileExpr(e.Args[0])
					rhs, err2 := tp.transpileExpr(e.Args[1])
					if err1 != nil || err2 != nil {
						return nil, fmt.Errorf("for call expr, unexpected errors: %v", errors.Join(err1, err2))
					}
					return &goast.BinaryExpr{
						X:  lhs,
						Y:  rhs,
						Op: operator,
					}, nil
				default:
					return nil, fmt.Errorf("for call expr, less than 3 arguments, got %d", len(e.Args))
				}
			}
		}
		goFn, err := tp.transpileExpr(e.Func)
		if err != nil {
			return nil, fmt.Errorf("for call expr function, unexpected error: %v", err)
		}
		args := make([]goast.Expr, len(e.Args))
		for i, arg := range e.Args {
			var err error
			args[i], err = tp.transpileExpr(arg)
			if err != nil {
				return nil, fmt.Errorf("for call expr param, unexpected error: %v", err)
			}
		}
		return &goast.CallExpr{
			Fun:  goFn,
			Args: args,
		}, nil
	case *ir.WhenMatch:
		statements, err := tp.transpileExpressionToStatements(expr, "return")
		if err != nil {
			return nil, err
		}
		exprType, err := tp.transpileType(tp.types.TypeOf(expr))
		if err != nil {
			return nil, fmt.Errorf("for when match, unexpected error transpiling type: %v", err)
		}

		return &goast.CallExpr{
			Fun: &goast.FuncLit{Type: &goast.FuncType{Results: &goast.FieldList{
				List: []*goast.Field{{Type: exprType}},
			}},
				Body: &goast.BlockStmt{List: statements}},
		}, nil

	default:
		return nil, fmt.Errorf("for expr, unexpected type %v", reflect.TypeOf(expr))
	}
}

func (tp *Transpiler) transpileParameterDecls(decl ir.Declaration, fn *ir.Func) (goast.FieldList, error) {
	fieldList := goast.FieldList{}
	errs := make([]error, 0)
	tp.inFunctionSignature = true
	t, err := tp.transpileType(tp.types.TypeOf(decl.E))
	tp.inFunctionSignature = false
	if err != nil {
		errs = append(errs, fmt.Errorf("for declaration %v, error when transpiling type: %v", decl.Name, err))
		return fieldList, errors.Join(errs...)
	}
	arrowT, ok := t.(*goast.FuncType)
	if !ok {
		errs = append(errs, fmt.Errorf("for declaration %v, expected FuncType but got type: %v", decl.Name, err))
	}
	for i, arg := range fn.ArgNames {
		fieldList.List = append(fieldList.List, &goast.Field{
			Names: []*goast.Ident{{
				Name: arg,
			}},
			Type: arrowT.Params.List[i].Type,
		})
	}
	return fieldList, errors.Join(errs...)
}

func (tp *Transpiler) transpileType(t ir.Type) (goast.Expr, error) {
	if t == nil {
		return nil, nil
	}
	// some shortcuts
	switch t.Hash() {
	case ir.BoolType.Hash(), ir.BoolTypeUnaliased.Hash():
		return goast.NewIdent("bool"), nil
	}
	switch e := t.(type) {
	case *ir.FnType:
		if e == nil {
			panic("nil type for arrow function")
		}
		var params = make([]*goast.Field, len(e.Args))
		var errs error
		for i, arg := range e.Args {
			tParam, err := tp.transpileType(arg)
			errs = errors.Join(errs, err)
			params[i] = &goast.Field{Type: tParam}
		}
		retType, err := tp.transpileType(e.Return)
		errs = errors.Join(errs, err)
		if err != nil {
			return nil, err
		}
		return &goast.FuncType{
			Params:  &goast.FieldList{List: params},
			Results: &goast.FieldList{List: []*goast.Field{{Type: retType}}},
		}, nil

	case *ir.TypeName:
		goEquivalent, ok := typeTagToGoType(e.Name)
		if ok {
			return goast.NewIdent(goEquivalent), nil
		}
		return goast.NewIdent(e.Name), nil

		// inferred to literal value
	case *ir.Literal:
		if tp.inFunctionSignature {
			goType, err := nearestGoType(e)
			return goast.NewIdent(goType), err
		}
		// if we are not in a function signature, we can just use the literal value without a type signature
		// to rely on Go inference to make literals be const (i.e., untyped types according to Go spec)
		return nil, nil

		// generic type!
	case *ir.TypeVar:
		return nil, fmt.Errorf("generics are not implemented yet, but got %v", t.ShowIn(ir.DumbShowCtx, 0))

	case *ir.AnyType:
		return goast.NewIdent("any"), nil

	default:
		return nil, fmt.Errorf("transpileType: unexpected ast.Type type: %v: %T", ir.TypeString(e), e)
	}
}

func (tp *Transpiler) transpileWhen(e *ir.WhenMatch) (statements []goast.Stmt, finalExpr goast.Expr, err error) {
	whenResultT, err := tp.transpileType(tp.types.TypeOf(e))
	if err != nil {
		return nil, nil, fmt.Errorf("for when, failed to transpile type %v: %v", tp.types.TypeOf(e), err)
	}
	resultIdent := goast.NewIdent(util.MangledIdentFrom(e, "whenResult"))
	finalExpr = resultIdent
	subjectExpr, err := tp.transpileExpr(e.Value)
	if err != nil {
		return nil, nil, fmt.Errorf("for when, failed to transpile when subject expression: %v", err)
	}
	var subjectIdent *goast.Ident
	if ident, ok := subjectExpr.(*goast.Ident); ok {
		subjectIdent = ident
	} else {
		// if the subject is not an identifier, store it in a temporary variable to not evaluate it twice
		// -> `var subjectIdent = (subjectExpr)`
		subjectIdent = goast.NewIdent(util.MangledIdentFrom(e, e.ExprName()))
		statements = append(statements,
			&goast.DeclStmt{Decl: &goast.GenDecl{
				Tok: token.VAR,
				Specs: []goast.Spec{&goast.ValueSpec{
					Names: []*goast.Ident{subjectIdent},
					//Type: nil, // rely on go inference for now
					Values: []goast.Expr{subjectExpr},
				}},
			}})
	}
	// declare the variable that will hold the result of the when
	// -> `var result whenResultT`
	statements = append(statements,
		&goast.DeclStmt{Decl: &goast.GenDecl{
			Tok: token.VAR,
			Specs: []goast.Spec{&goast.ValueSpec{
				Names: []*goast.Ident{resultIdent},
				Type:  whenResultT,
			}},
		}})
	var currentIf *goast.IfStmt

	// addIf adds a new if statement to the current chain, or starts a new chain if currentIf is nil
	addIf := func(cond *goast.IfStmt) {
		if currentIf == nil {
			statements = append(statements, cond)
		} else {
			currentIf.Else = cond
		}
		currentIf = cond
	}
	for _, case_ := range e.Cases {
		branchResultPath, err := tp.transpileExpressionToStatements(case_.Value, resultIdent.Name)
		if err != nil {
			return nil, nil, fmt.Errorf("failed to transpile when case value expression: %v", err)
		}
		// TODO here we want to transpile when cases directly with their own helper
		pattern := case_.Pattern.(*ir.MatchTypePattern)
		switch astType := pattern.Type_.(type) {
		// type check against a literal - we do a simple equality check
		case *ir.Literal:
			goPredicate, err := tp.transpileExpr(astType)
			if err != nil {
				return nil, nil, fmt.Errorf("failed to transpile when case literal expression: %v", err)
			}
			// -> `if subjectIdent == goPredicate { branchResultPath }`
			addIf(&goast.IfStmt{
				Cond: &goast.BinaryExpr{
					X:  subjectIdent,
					Op: token.EQL,
					Y:  goPredicate,
				},
				Body: &goast.BlockStmt{List: branchResultPath},
			})
			// type check against a type name - we do a Go type assertion
		case *ir.TypeName:
			// this is the discard pattern - this is the final else case, so we do not need a new if condition
			// -> `else { branchResultPath }`
			if astType.Name == "_" {
				currentIf.Else = &goast.BlockStmt{List: branchResultPath}
				continue
			}
			goType, err := tp.transpileType(astType)
			if err != nil {
				return nil, nil, fmt.Errorf("failed to transpile when case type name expression: %v", err)
			}
			okIdent := goast.NewIdent("ok")
			asserted := goast.NewIdent("asserted")
			// -> `if asserted, ok := subjectIdent.(goType) { branchResultPath }`
			addIf(&goast.IfStmt{
				Init: &goast.AssignStmt{
					Lhs: []goast.Expr{asserted, okIdent},
					Rhs: []goast.Expr{&goast.TypeAssertExpr{X: subjectIdent, Type: goType}},
					Tok: token.DEFINE,
				},
				Cond: okIdent,
				Body: &goast.BlockStmt{List: branchResultPath},
			})

		default:
			return nil, nil, fmt.Errorf("unsupported type in when pattern case: %T", pattern.Type_)

		}
	}
	return statements, finalExpr, nil
}

// transpileExpressionToStatements makes a []ast.Stmt
// that produces the result of expr, and places it on a final variable finalLocalVarName
// the caller will then need to synthesise a final statement that use that value.
// Exceptionally, if finalLocalVarName is a string with "return",
// then transpileExpressionToStatements makes the last statement a return statement
// for the resulting ast.Expr
func (tp *Transpiler) transpileExpressionToStatements(expr ir.Expr, finalLocalVarName string) ([]goast.Stmt, error) {
	var statements []goast.Stmt
	var finalExpr goast.Expr
	switch e := expr.(type) {
	// add non inlineable Exprs here!

	// some ast.Expr we can inline directly to a Go expression and return that
	case *ir.RecordSelect, *ir.Literal, *ir.Call, *ir.Var:
		goExpr, err := tp.transpileExpr(e)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile when expression: %v", err)
		}
		finalExpr = goExpr

	case *ir.Assign:
		goRHSExpr, err := tp.transpileExpr(e.Value)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile when expression: %v", err)
		}
		remainder, err := tp.transpileExpressionToStatements(e.Body, finalLocalVarName)
		t, err := tp.transpileType(tp.types.TypeOf(e))
		if err != nil {
			return nil, fmt.Errorf("failed to transpile type: %v", err)
		}
		final := []goast.Stmt{
			&goast.DeclStmt{Decl: &goast.GenDecl{
				Tok: token.VAR,
				Specs: []goast.Spec{
					&goast.ValueSpec{
						Names:  []*goast.Ident{{Name: e.Var}},
						Values: []goast.Expr{goRHSExpr},
						Type:   t,
					},
				},
			}},
		}

		return append(final, remainder...), nil

		// a when without clauses is equivalent to a Go switch without a subject
	case *ir.WhenMatch:
		var err error
		statements, finalExpr, err = tp.transpileWhen(e)
		if err != nil {
			return nil, err
		}

	// we do like in Go, and add a statement that should evaluate the expression,
	// but we do not actually do anything with it
	case *ir.Unused:
		goStatement, err := tp.transpileExpr(e.Value)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		remainder, err := tp.transpileExpressionToStatements(e.Body, finalLocalVarName)
		if err != nil {
			return nil, err
		}
		final := []goast.Stmt{&goast.ExprStmt{X: goStatement}}

		return append(final, remainder...), nil

	case nil:
		panic("unexpected nil expression")

	default:
		panic("unimplemented expression type: " + reflect.TypeOf(expr).String())
	}

	var finalStatement goast.Stmt
	switch finalLocalVarName {
	case "":
		return nil, errors.New("unexpected empty local variable name")
	case "return":
		finalStatement = &goast.ReturnStmt{
			Results: []goast.Expr{finalExpr},
		}
	default:
		finalStatement = &goast.AssignStmt{
			Lhs: []goast.Expr{goast.NewIdent(finalLocalVarName)},
			Rhs: []goast.Expr{finalExpr},
			Tok: token.ASSIGN,
		}
	}

	statements = append(statements, finalStatement)

	return statements, nil

}
