package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	goast "go/ast"
	"go/token"
	"log/slog"
)

type Transpiler struct {
	types       *types.TypeCtx
	currentExpr ir.Expr
	// inFunctionSignature is used during transpileType to determine if we can use const types or not
	inFunctionSignature bool

	*slog.Logger
}

func NewTranspiler(typeCtx *types.TypeCtx) *Transpiler {
	return &Transpiler{
		types: typeCtx,
		Logger: slog.With("section", "transpiler"),
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