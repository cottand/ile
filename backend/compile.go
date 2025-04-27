package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/types"
	"github.com/cottand/ile/util"
	goast "go/ast"
	"go/token"
	"reflect"
)

const goVersion = "1.23.3"

// will need to handle more concrete tags, like random strings and specific ints,
// but for now we'll just do the basic types
func typeTagToGoType(t *ast.TypeTag) (goType string, err error) {
	goType, ok := ileToGoTypes[t.Name]
	if ok {
		return goType, nil
	}
	return "", fmt.Errorf("failed to convert type tag %s", t.Name)
}

var ileToGoTypes = map[string]string{
	ast.IntTypeName: "int64",
	"String":        "string",
	"Float":         "float64",
	"Bool":          "bool",
}

var ileToGoVars = map[string]string{
	"True":  "true",
	"False": "false",
}

type Transpiler struct {
	types *types.TypeCtx
}

func NewTranspiler() *Transpiler {
	return &Transpiler{}
}

func (tp *Transpiler) TranspilePackage(name string, syntax []ast.File) ([]goast.File, error) {
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
	decls = append(decls, functions...)
	return []goast.File{{
		Name:      &goast.Ident{Name: name},
		GoVersion: goVersion,
		Decls:     decls,
	}}, nil
}

func (tp *Transpiler) TranspileFile(file ast.File) (*goast.File, error) {
	var decls []goast.Decl
	var err error

	if len(file.Declarations) > 0 {
		declarations, err := tp.transpileDeclarations(file.Declarations)
		if err != nil {
			return nil, err
		}
		for _, decl := range declarations {
			decls = append(decls, &decl)
		}
	}

	functions, err := tp.transpileFunctionDecls(file.Declarations)
	if err != nil {
		return nil, err
	}
	decls = append(decls, functions...)
	return &goast.File{
		Name:      &goast.Ident{Name: file.PkgName},
		GoVersion: goVersion,
		Decls:     decls,
	}, nil
}

// transpileDeclarations works at the top level of the file
func (tp *Transpiler) transpileDeclarations(vars []ast.Declaration) ([]goast.GenDecl, error) {
	goDecls := make([]goast.Spec, 0)
	goConstDecls := make([]goast.Spec, 0)
	errs := make([]error, 0)
	for _, decl := range vars {
		_, ok := decl.E.(*ast.Func)
		if !ok {
			value, err := tp.transpileExpr(decl.E)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			var type_ goast.Expr
			type_, err = tp.transpileType(decl.Type)
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

func (tp *Transpiler) transpileFunctionDecls(fs []ast.Declaration) ([]goast.Decl, error) {
	var goDecls []goast.Decl
	var errs []error
	for _, irFunc := range fs {
		if fn, ok := irFunc.E.(*ast.Func); ok {
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
			t, ok := tp.types.TypeOf(fn).(*ast.FnType)
			if !ok {
				errs = append(errs, errors.New("expected a function type"))
			}
			if t.Return != ast.UnitType {
				resultType, err := tp.transpileType(t.Return)
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

func (tp *Transpiler) transpileExpr(expr ast.Expr) (goast.Expr, error) {
	switch e := expr.(type) {
	case *ast.Literal:
		switch e.Kind {
		case token.STRING:
			return &goast.BasicLit{
				Kind: e.Kind,
				// TODO reconsider escaping!
				Value: "`" + e.Syntax + "`",
			}, nil
		case token.INT:
			return &goast.CallExpr{
				// TODO we want to use the original type here, not always unt64
				Fun: goast.NewIdent("int64"),
				Args: []goast.Expr{
					&goast.BasicLit{
						Kind:  e.Kind,
						Value: e.Syntax,
					}},
			}, nil
		default:
			return nil, fmt.Errorf("for basicLit expr, unexpected token %v for type %v", e.Kind.String(), reflect.TypeOf(expr))
		}

	case *ast.Var:
		if goVar := ileToGoVars[e.Name]; goVar != "" {
			return goast.NewIdent(goVar), nil
		}
		return goast.NewIdent(e.Name), nil

		// Go does not have ternary operators or anything that lets us inline logic into an expression,
		// so we inline a function call
	case *ast.WhenMatch:
		whenResultT, err := tp.transpileType(e.Type())
		if err != nil {
			return nil, fmt.Errorf("for when, failed to transpile type %v: %v", tp.types.TypeOf(e), err)
		}
		whenBlock, err := tp.transpileExpressionToStatements(e, "return")
		if err != nil {
			return nil, fmt.Errorf("for when, failed to transpile: %v", err)
		}
		fLiteral := &goast.FuncLit{
			Type: &goast.FuncType{
				TypeParams: &goast.FieldList{List: []*goast.Field{}},
				Params:     &goast.FieldList{List: []*goast.Field{}},
				Results:    &goast.FieldList{List: []*goast.Field{{Type: whenResultT}}},
			},
			Body: &goast.BlockStmt{List: whenBlock},
		}

		return &goast.CallExpr{
			Fun:  fLiteral,
			Args: []goast.Expr{},
		}, nil
	case *ast.RecordSelect:
		selected, err := tp.transpileExpr(e.Record)
		if err != nil {
			return nil, fmt.Errorf("for record, failed to transpile record: %v", err)
		}

		return &goast.SelectorExpr{
			X:   selected,
			Sel: goast.NewIdent(e.Label),
		}, nil
	case *ast.Call:

		switch fn := e.Func.(type) {
		// here, e is an operator call, not a normal function call
		case *ast.Literal:
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
					Op: fn.Kind,
				}, nil
			default:
				return nil, fmt.Errorf("for call expr, less than 3 arguments, got %d", len(e.Args))
			}

		default:
			goFn, err := tp.transpileExpr(fn)
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
		}
	default:
		return nil, fmt.Errorf("for expr, unexpected type %v", reflect.TypeOf(expr))
	}
}

func (tp *Transpiler) transpileParameterDecls(decl ast.Declaration, fn *ast.Func) (goast.FieldList, error) {
	fieldList := goast.FieldList{}
	errs := make([]error, 0)
	t, err := tp.transpileType(decl.Type)
	if err != nil {
		errs = append(errs, fmt.Errorf("for declaration %v, error when transpiling type: %v", decl.Name, err))
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

func (tp *Transpiler) transpileType(t ast.Type) (goast.Expr, error) {
	if t == nil {
		return nil, nil
	}
	switch e := t.(type) {
	case *ast.FnType:
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

	case *ast.TypeTag:
		goEquivalent, ok := ileToGoTypes[e.Name]
		if ok {
			return goast.NewIdent(goEquivalent), nil
		}
		return goast.NewIdent(e.Name), nil

		// generic type!
	case *ast.TypeVar:
		return nil, fmt.Errorf("generics are not implemented yet, but got %v", t.ShowIn(ast.DumbShowCtx, 0))

	default:
		return nil, fmt.Errorf("transpileType: unexpected ast.Type type: %v: %T", e, e)
	}
}

// transpileExpressionToStatements makes a []ast.Stmt
// that produces the result of expr, and places it on a final variable finalLocalVarName
// the caller will then need to synthesise a final statement that use that value.
// Exceptionally, if finalLocalVarName is a string with "return",
// then transpileExpressionToStatements makes the last statement a return statement
// for the resulting ast.Expr
func (tp *Transpiler) transpileExpressionToStatements(expr ast.Expr, finalLocalVarName string) ([]goast.Stmt, error) {
	var statements []goast.Stmt
	var finalExpr goast.Expr
	switch e := expr.(type) {
	// add non inlineable Exprs here!

	// some ast.Expr we can inline directly to a Go expression and return that
	case *ast.RecordSelect, *ast.Literal, *ast.Call, *ast.Var:
		goExpr, err := tp.transpileExpr(e)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		finalExpr = goExpr

	case *ast.Assign:
		goRHSExpr, err := tp.transpileExpr(e.Value)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
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
	case *ast.WhenMatch:
		whenResultT, err := tp.transpileType(tp.types.TypeOf(e))
		if err != nil {
			return nil, fmt.Errorf("for when, failed to transpile type %v: %v", tp.types.TypeOf(e), err)
		}
		switchResultIdent := goast.NewIdent(util.MangledIdentFrom(e, e.ExprName()))
		statements = append(statements, &goast.DeclStmt{Decl: &goast.GenDecl{
			Tok: token.VAR,
			Specs: []goast.Spec{&goast.ValueSpec{
				Names: []*goast.Ident{switchResultIdent},
				Type:  whenResultT,
			}},
		}})
		assignToResult := func(expr goast.Expr) *goast.AssignStmt {
			return &goast.AssignStmt{
				Tok: token.ASSIGN,
				Lhs: []goast.Expr{switchResultIdent},
				Rhs: []goast.Expr{expr},
			}
		}
		var caseClauses []goast.Stmt
		for _, case_ := range e.Cases {
			goPredicate, err := tp.transpileExpr(case_.Pattern.(*ast.ValueLiteralPattern).Value)
			if err != nil {
				return nil, fmt.Errorf("failed to transpile when case predicate expression: %v", err)
			}
			goValue, err := tp.transpileExpr(case_.Value)
			if err != nil {
				return nil, fmt.Errorf("failed to transpile when case value expression: %v", err)
			}
			caseClauses = append(caseClauses, &goast.CaseClause{
				List: []goast.Expr{goPredicate},
				Body: []goast.Stmt{assignToResult(goValue)},
			})
		}
		statements = append(statements,
			&goast.SwitchStmt{
				Body: &goast.BlockStmt{List: caseClauses},
			})
		finalExpr = switchResultIdent

	// we do like in Go, and add a statement that should evaluate the expression,
	// but we do not actually do anything with it
	case *ast.Unused:
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
			Tok: token.DEFINE,
		}
	}

	statements = append(statements, finalStatement)

	return statements, nil

}
