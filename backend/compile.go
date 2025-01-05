package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/ir"
	goast "go/ast"
	"go/token"
	"reflect"
)

const goVersion = "1.23.3"

var ileToGoTypes = map[string]string{
	"Int":    "int64",
	"String": "string",
	"Float":  "float64",
	"Bool":   "bool",
}

func TranspileFile(file ir.File) (*goast.File, error) {
	var decls []goast.Decl
	var declarations *goast.GenDecl
	var err error

	if len(file.Values) > 0 {
		declarations, err = transpileValDeclarations(file.Values)
		if err != nil {
			return nil, err
		}
		decls = append(decls, declarations)
	}
	functions, err := transpileFunctionDecls(file.Functions)
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

// returns
func transpileValDeclarations(vars []ir.ValDecl) (*goast.GenDecl, error) {
	goDecls := make([]goast.Spec, 0)
	errs := make([]error, 0)
	for _, rawVar := range vars {
		value, err := transpileExpr(rawVar.E)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		var type_ goast.Expr
		if rawVar.T != nil {
			type_, err = transpileType(rawVar.T)
			if err != nil {
				errs = append(errs, err)
				continue
			}

		}
		spec := &goast.ValueSpec{
			Names:  []*goast.Ident{goast.NewIdent(rawVar.Name)},
			Values: []goast.Expr{value},
			Type:   type_,
		}
		goDecls = append(goDecls, spec)
	}
	joined := errors.Join(errs...)
	return &goast.GenDecl{
		Specs: goDecls,
		Tok:   token.VAR,
	}, joined
}

func transpileExpr(expr ir.Expr) (goast.Expr, error) {
	switch e := expr.(type) {
	case ir.BasicLitExpr:
		switch e.Kind {
		case token.STRING:
			return &goast.BasicLit{
				Kind: e.Kind,
				// TODO reconsider escaping!
				Value: "`" + e.Value + "`",
			}, nil
		case token.INT:
			return &goast.CallExpr{
				Fun: goast.NewIdent("int64"),
				Args: []goast.Expr{
					&goast.BasicLit{
						Kind:  e.Kind,
						Value: e.Value,
					}},
			}, nil
		default:
			return nil, fmt.Errorf("for basicLit expr, unexpected type %v", reflect.TypeOf(expr))
		}

	case ir.IdentifierLitExpr:
		return goast.NewIdent(e.NameLit), nil

	case ir.BinaryOpExpr:
		lhs, err1 := transpileExpr(e.Lhs)
		rhs, err2 := transpileExpr(e.Rhs)
		if err1 != nil || err2 != nil {
			return nil, errors.Join(err1, err2)
		}
		return &goast.BinaryExpr{
			X:  lhs,
			Y:  rhs,
			Op: e.Op.Token(),
		}, nil
	default:
		return nil, fmt.Errorf("for expr, unexpected type %v", reflect.TypeOf(expr))
	}
}

func transpileFunctionDecls(fs []ir.FuncDecl) ([]goast.Decl, error) {
	var goDecls []goast.Decl
	var errs []error
	for _, irFunc := range fs {
		paramDecl, err := transpileParameterDecls(irFunc.Params)
		if err != nil {
			errs = append(errs, err)
			continue
		}

		body, err := transpileExpressionToStatements(irFunc.BodyLit, "return")
		if err != nil {
			errs = append(errs, err)
			continue
		}
		var resultList *goast.FieldList
		if irFunc.Result != nil {
			resultType, err := transpileType(irFunc.Result)
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
			Name: goast.NewIdent(irFunc.NameLit),
			Type: &goast.FuncType{
				Params:  &paramDecl,
				Results: resultList,
			},
			Body: &goast.BlockStmt{List: body},
		}
		goDecls = append(goDecls, &goDecl)
	}
	return goDecls, errors.Join(errs...)
}

func transpileParameterDecls(params []ir.ParamDecl) (goast.FieldList, error) {
	fieldList := goast.FieldList{}
	errs := make([]error, 0)
	for _, param := range params {
		t, err := transpileType(param.T)
		if err != nil {
			errs = append(errs, err)
			continue
		}

		fieldList.List = append(fieldList.List, &goast.Field{
			Names: []*goast.Ident{{
				Name: param.Name.Name,
			}},
			Type: t,
		})
	}
	return fieldList, errors.Join(errs...)
}

func transpileType(t ir.Type) (goast.Expr, error) {
	if t == nil {
		return nil, nil
	}
	switch e := t.(type) {
	case ir.TypeLit:
		goEquivalent, ok := ileToGoTypes[e.NameLit]
		if ok {
			return goast.NewIdent(goEquivalent), nil
		}
		return goast.NewIdent(e.NameLit), nil
	default:
		return nil, fmt.Errorf("unexpected ir.Type type: %v", e)
	}
}

// transpileExpressionToStatements makes a []ast.Stmt
// that produces the result of expr, and places it on a final variable finalLocalVarName
// the caller will then need to synthesise a final statement that use that value.
// Exceptionally, if finalLocalVarName is a string with "return",
// then transpileExpressionToStatements makes the last statement a return statement
// for the resulting ir.Expr
func transpileExpressionToStatements(expr ir.Expr, finalLocalVarName string) ([]goast.Stmt, error) {
	var statements []goast.Stmt
	var finalExpr goast.Expr
	switch e := expr.(type) {
	// add non inlineable Exprs here!

	// some ir.Expr we can inline directly to a Go expression and return that
	case ir.BasicLitExpr, ir.BinaryOpExpr, ir.IdentifierLitExpr:
		goExpr, err := transpileExpr(e)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		finalExpr = goExpr

	case ir.AssignExpr:
		goRHSExpr, err := transpileExpr(e.Rhs)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		remainder, err := transpileExpressionToStatements(e.Remainder, finalLocalVarName)
		t, err := transpileType(e.Type)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile type: %v", err)
		}
		final := []goast.Stmt{
			&goast.DeclStmt{Decl: &goast.GenDecl{
				Tok: token.VAR,
				Specs: []goast.Spec{
					&goast.ValueSpec{
						Names:  []*goast.Ident{{Name: e.IdentName}},
						Values: []goast.Expr{goRHSExpr},
						Type:   t,
					},
				},
			}},
		}

		return append(final, remainder...), nil

	// we do like in Go, and add a statement that should evaluate the expression,
	// but we do not actually do anything with it
	case ir.UnusedExpr:
		goStatement, err := transpileExpr(e.Expr)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		remainder, err := transpileExpressionToStatements(e.Remainder, finalLocalVarName)
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
