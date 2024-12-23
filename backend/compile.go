package backend

import (
	"errors"
	"github.com/cottand/ile/ir"
	"go/ast"
	"go/token"
)

const goVersion = "1.23.3"

func TranspileFile(file ir.File) (*ast.File, error) {
	declarations, err := transpileValDeclarations(file.Declarations)
	if err != nil {
		return nil, err
	}
	return &ast.File{
		Name:      &ast.Ident{Name: file.PkgName},
		GoVersion: goVersion,
		Decls:     []ast.Decl{declarations},
	}, nil
}

func transpileValDeclarations(vars []ir.ValDecl) (*ast.GenDecl, error) {
	goDecls := make([]ast.Spec, 0)
	errs := make([]error, 0)
	for _, rawVar := range vars {
		value, err := transpileExpr(rawVar.E)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		spec := &ast.ValueSpec{
			Names: []*ast.Ident{
				ast.NewIdent(rawVar.Name),
			},
			Values: []ast.Expr{value},
		}
		goDecls = append(goDecls, spec)
	}
	return &ast.GenDecl{
		Specs: goDecls,
		Tok:   token.VAR,
	}, errors.Join(errs...)
}

func transpileExpr(expr ir.Expr) (ast.Expr, error) {
	switch e := expr.(type) {
	case *ir.BasicLit:
		return &ast.BasicLit{
			Kind:  e.Kind,
			Value: e.Value,
		}, nil
	default:
		return nil, errors.New("unexpected BasicLit type")
	}
}
