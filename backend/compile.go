package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/ir"
	"go/ast"
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

func TranspileFile(file ir.File) (*ast.File, error) {
	var decls []ast.Decl
	var declarations *ast.GenDecl
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
	return &ast.File{
		Name:      &ast.Ident{Name: file.PkgName},
		GoVersion: goVersion,
		Decls:     decls,
	}, nil
}

// returns
func transpileValDeclarations(vars []ir.ValDecl) (*ast.GenDecl, error) {
	goDecls := make([]ast.Spec, 0)
	errs := make([]error, 0)
	for _, rawVar := range vars {
		value, err := transpileExpr(rawVar.E)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		type_, err := transpileType(rawVar.T)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{
				ast.NewIdent(rawVar.Name),
			},
			Values: []ast.Expr{value},
			Type:   type_,
		}
		goDecls = append(goDecls, spec)
	}
	joined := errors.Join(errs...)
	return &ast.GenDecl{
		Specs: goDecls,
		Tok:   token.VAR,
	}, joined
}

func transpileExpr(expr ir.Expr) (ast.Expr, error) {
	switch e := expr.(type) {
	case ir.BasicLitExpr:
		switch e.Kind {
		case token.STRING:
			return &ast.BasicLit{
				Kind: e.Kind,
				// TODO reconsider escaping!
				Value: "`" + e.Value + "`",
			}, nil
		case token.INT:
			return &ast.BasicLit{
				Kind:  e.Kind,
				Value: e.Value,
			}, nil
		default:
			return nil, fmt.Errorf("for basicLit expr, unexpected type %v", reflect.TypeOf(expr))
		}

	case ir.IdentifierLitExpr:
		return ast.NewIdent(e.Name), nil

	case ir.BinaryOpExpr:
		lhs, err1 := transpileExpr(e.Lhs)
		rhs, err2 := transpileExpr(e.Rhs)
		if err1 != nil || err2 != nil {
			return nil, errors.Join(err1, err2)
		}
		return &ast.BinaryExpr{
			X:  lhs,
			Y:  rhs,
			Op: e.Op,
		}, nil
	default:
		return nil, fmt.Errorf("for expr, unexpected type %v", reflect.TypeOf(expr))
	}
}

func transpileFunctionDecls(fs []ir.FuncDecl) ([]ast.Decl, error) {
	var goDecls []ast.Decl
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
		var resultList *ast.FieldList
		if irFunc.Result != nil {
			resultType, err := transpileType(irFunc.Result)
			if err != nil {
				errs = append(errs, err)
				continue
			}
			resultList = &ast.FieldList{
				List: []*ast.Field{{
					Type: resultType,
				}},
			}
		}
		goDecl := ast.FuncDecl{
			Name: ast.NewIdent(irFunc.NameLit),
			Type: &ast.FuncType{
				Params:  &paramDecl,
				Results: resultList,
			},
			Body: &ast.BlockStmt{List: body},
		}
		goDecls = append(goDecls, &goDecl)
	}
	return goDecls, errors.Join(errs...)
}

func transpileParameterDecls(params []ir.ParamDecl) (ast.FieldList, error) {
	fieldList := ast.FieldList{}
	errs := make([]error, 0)
	for _, param := range params {
		t, err := transpileType(param.T)
		if err != nil {
			errs = append(errs, err)
			continue
		}

		fieldList.List = append(fieldList.List, &ast.Field{
			Names: []*ast.Ident{{
				Name: param.Name.Name,
			}},
			Type: t,
		})
	}
	return fieldList, errors.Join(errs...)
}

func transpileType(t ir.Type) (ast.Expr, error) {
	switch e := t.(type) {
	case ir.TypeLit:
		goEquivalent, ok := ileToGoTypes[e.NameLit]
		if ok {
			return ast.NewIdent(goEquivalent), nil
		}
		return ast.NewIdent(e.NameLit), nil
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
func transpileExpressionToStatements(expr ir.Expr, finalLocalVarName string) ([]ast.Stmt, error) {
	var statements []ast.Stmt
	var finalExpr ast.Expr
	switch e := expr.(type) {
	// add non inlineable Exprs here!

	// some ir.Expr we can inline directly to a Go expression and return that
	case ir.BasicLitExpr, ir.BinaryOpExpr, ir.IdentifierLitExpr:
		goExpr, err := transpileExpr(e)
		if err != nil {
			return nil, fmt.Errorf("failed to transpile expression: %v", err)
		}
		finalExpr = goExpr
	default:
	}

	var finalStatement ast.Stmt
	switch finalLocalVarName {
	case "":
		return nil, errors.New("unexpected empty local variable name")
	case "return":
		finalStatement = &ast.ReturnStmt{
			Results: []ast.Expr{finalExpr},
		}
	default:
		finalStatement = &ast.AssignStmt{
			Lhs: []ast.Expr{ast.NewIdent(finalLocalVarName)},
			Rhs: []ast.Expr{finalExpr},
			Tok: token.DEFINE,
		}
	}

	statements = append(statements, finalStatement)

	return statements, nil

}
