package backend

import (
	"errors"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
	goast "go/ast"
	"go/token"
)

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

			var resultList *goast.FieldList
			t, ok := tp.types.TypeOf(fn).(*ir.FnType)
			if !ok {
				errs = append(errs, errors.New("expected a function type"))
			}
			isUnitFunc := types.Equal(t.Return, ir.NilType)

			var body []goast.Stmt
			if isUnitFunc {
				body, err = tp.transpileExpressionToStatements(fn.Body, "")
			} else {
				body, err = tp.transpileExpressionToStatements(fn.Body, "return")
			}
			if err != nil {
				errs = append(errs, err)
				continue
			}

			if !isUnitFunc {
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
