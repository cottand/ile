package backend

import (
	"errors"
	"fmt"
	goast "go/ast"
	"go/token"
	"strings"
	"unicode"

	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
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
		if isSuitableForGoConst(decl) {
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

func isUnitType(t ir.Type) bool {
	return types.Equal(t, ir.NilType) || types.Equal(t, &ir.NothingType{})

}

func (tp *Transpiler) transpileFunctionDecls(fs []ir.Declaration) ([]goast.Decl, error) {
	var goDecls []goast.Decl
	var errs []error
	for _, irFunc := range fs {
		if fn, ok := unwrapAscribe(irFunc.E).(*ir.Func); ok {
			annotationTypeVars, annotationType := extractAnnotationTypeVars(irFunc.E)

			if len(annotationTypeVars) > 0 {
				// TODO extract each decl into its own function,
				//  and use defer (tp.typeVarNames = nil)
				//  instead of the current ugly nil-setting every time
				tp.typeVarNames = make(map[string]string, len(annotationTypeVars))
				for i, tv := range annotationTypeVars {
					tp.typeVarNames[tv.Identifier] = typeVarGoName(tv, i)
				}
			}

			var fnType ir.Type
			if annotationType != nil && len(annotationTypeVars) > 0 {
				fnType = annotationType
			} else {
				fnType = tp.types.TypeOf(irFunc.E)
			}

			typeVars, bounds := extractTypeVars(fnType)

			paramDecl, err := tp.transpileGenericParameterDecls(irFunc, fn, fnType)
			if err != nil {
				tp.typeVarNames = nil
				errs = append(errs, err)
				continue
			}

			t, ok := fnType.(*ir.FnType)
			if !ok {
				if ct, ok2 := fnType.(*ir.ConstrainedType); ok2 {
					t, ok = ct.Base.(*ir.FnType)
				}
			}
			if !ok {
				tp.typeVarNames = nil
				errs = append(errs, errors.New("expected a function type"))
				continue
			}
			isUnitFunc := isUnitType(t.Return)

			var body []goast.Stmt
			if isUnitFunc {
				body, err = tp.transpileExpressionToStatements(fn.Body, "")
			} else {
				body, err = tp.transpileExpressionToStatements(fn.Body, "return")
			}
			if err != nil {
				tp.typeVarNames = nil
				errs = append(errs, err)
				continue
			}

			var resultList *goast.FieldList
			if !isUnitFunc {
				tp.inFunctionSignature = true
				resultType, err := tp.transpileType(t.Return)
				tp.inFunctionSignature = false
				if err != nil {
					tp.typeVarNames = nil
					errs = append(errs, err)
					continue
				}
				resultList = &goast.FieldList{
					List: []*goast.Field{{
						Type: resultType,
					}},
				}
			}

			var typeParamList *goast.FieldList
			if len(typeVars) > 0 {
				typeParamFields := make([]*goast.Field, 0, len(typeVars))
				for i, tv := range typeVars {
					goName := typeVarGoName(tv, i)
					constraint := boundsToGoConstraint(bounds[tv.Identifier])
					typeParamFields = append(typeParamFields, &goast.Field{
						Names: []*goast.Ident{goast.NewIdent(goName)},
						Type:  goast.NewIdent(constraint),
					})
				}
				typeParamList = &goast.FieldList{List: typeParamFields}
			}

			goDecl := goast.FuncDecl{
				Name: goast.NewIdent(irFunc.Name),
				Type: &goast.FuncType{
					TypeParams: typeParamList,
					Params:     &paramDecl,
					Results:    resultList,
				},
				Body: &goast.BlockStmt{List: body},
			}
			tp.typeVarNames = nil
			goDecls = append(goDecls, &goDecl)
		}
	}
	return goDecls, errors.Join(errs...)
}

// extractAnnotationTypeVars extracts TypeVars from a function's Ascribe annotation.
// If the expression is Ascribe(Func, FnType) and the FnType contains TypeVars,
// it returns those TypeVars and the FnType.
func extractAnnotationTypeVars(expr ir.Expr) ([]*ir.TypeVar, *ir.FnType) {
	ascribe, ok := expr.(*ir.Ascribe)
	if !ok {
		return nil, nil
	}
	fnType, ok := ascribe.Type_.(*ir.FnType)
	if !ok {
		return nil, nil
	}
	typeVars, _ := extractTypeVars(fnType)
	if len(typeVars) == 0 {
		return nil, nil
	}
	return typeVars, fnType
}

// transpileGenericParameterDecls transpiles function parameters using the given type
// (which may be the annotation type for generic functions or the inferred type).
func (tp *Transpiler) transpileGenericParameterDecls(decl ir.Declaration, fn *ir.Func, fnType ir.Type) (goast.FieldList, error) {
	fieldList := goast.FieldList{}
	var errs []error
	tp.inFunctionSignature = true
	t, err := tp.transpileType(fnType)
	tp.inFunctionSignature = false
	if err != nil {
		errs = append(errs, fmt.Errorf("for declaration %v, error when transpiling type: %v", decl.Name, err))
		return fieldList, errors.Join(errs...)
	}
	arrowT, ok := t.(*goast.FuncType)
	if !ok {
		errs = append(errs, fmt.Errorf("for declaration %v, expected FuncType but got: %T", decl.Name, t))
		return fieldList, errors.Join(errs...)
	}
	for i, arg := range fn.ArgNames {
		if i >= len(arrowT.Params.List) {
			errs = append(errs, fmt.Errorf("for declaration %v, param count mismatch", decl.Name))
			break
		}
		fieldList.List = append(fieldList.List, &goast.Field{
			Names: []*goast.Ident{{Name: arg}},
			Type:  arrowT.Params.List[i].Type,
		})
	}
	return fieldList, errors.Join(errs...)
}

func extractTypeVars(t ir.Type) ([]*ir.TypeVar, map[string]*ir.TypeBounds) {
	var typeVars []*ir.TypeVar
	seen := map[string]bool{}
	bounds := map[string]*ir.TypeBounds{}

	var walk func(ir.Type)
	walk = func(t ir.Type) {
		if t == nil {
			return
		}
		switch e := t.(type) {
		case *ir.ConstrainedType:
			for _, entry := range e.Where {
				bounds[entry.Var.Identifier] = entry.Bounds
			}
			walk(e.Base)
		case *ir.FnType:
			for _, arg := range e.Args {
				walk(arg)
			}
			walk(e.Return)
		case *ir.TypeVar:
			if !seen[e.Identifier] {
				seen[e.Identifier] = true
				typeVars = append(typeVars, e)
			}
		case *ir.AppliedType:
			for _, arg := range e.Args {
				walk(arg)
			}
		case *ir.UnionType:
			walk(e.Left)
			walk(e.Right)
		case *ir.IntersectionType:
			walk(e.Left)
			walk(e.Right)
		case *ir.RecordType:
			for _, f := range e.Fields {
				walk(f.Type.Out)
			}
		}
	}
	walk(t)
	return typeVars, bounds
}

func typeVarGoName(tv *ir.TypeVar, index int) string {
	name := tv.NameHint
	if name == "" {
		name = strings.TrimPrefix(tv.Identifier, "'")
	}
	if name == "" {
		return fmt.Sprintf("T%d", index+1)
	}
	runes := []rune(name)
	runes[0] = unicode.ToUpper(runes[0])
	return string(runes)
}

func boundsToGoConstraint(b *ir.TypeBounds) string {
	if b == nil {
		return "any"
	}
	_, lowerIsNothing := b.Lower.(*ir.NothingType)
	_, upperIsAny := b.Upper.(*ir.AnyType)
	if lowerIsNothing && upperIsAny {
		return "any"
	}
	if upperIsAny {
		return "any"
	}
	if name, ok := b.Upper.(*ir.TypeName); ok {
		return name.Name
	}
	return "any"
}
