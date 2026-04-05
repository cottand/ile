package backend

import (
	"cmp"
	"errors"
	"fmt"
	goast "go/ast"
	"go/token"
	"reflect"
	"slices"
	"strconv"
	"sync"

	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/frontend/types"
)

// injectPotentialNothingType wraps goExpr in a closure that returns a zero-value if ileExprType is Nothing
//
// (by now the compiler has asserted such a value would never be used anyway)
//
// We could just do this for every expression, but there are plenty of them we don't need to query the type
// system for (like record select, literals, etc). So instead we call this in scenarios we need to guard against
// this only
func (tp *Transpiler) injectPotentialNothingType(goExpr goast.Expr, ileExprType ir.Type) (goast.Expr, error) {
	if !types.Equal(ileExprType, ir.NothingTypePtr) {
		return goExpr, nil
	}

	return &goast.CallExpr{
		Fun: &goast.FuncLit{Type: &goast.FuncType{Results: &goast.FieldList{
			List: []*goast.Field{{Type: goast.NewIdent("any")}},
		}},
			Body: &goast.BlockStmt{List: []goast.Stmt{
				// evaluate the desired expression, then return any
				&goast.ExprStmt{X: goExpr},
				&goast.ReturnStmt{Results: []goast.Expr{goast.NewIdent("nil")}},
			}}},
	}, nil
}

// newSliceExpr tales the existing goExpr and makes it a slice by adding a function closure
func (tp *Transpiler) newSliceExpr(expr goast.Expr, retType goast.Expr) *goast.CallExpr {
	return &goast.CallExpr{
		Fun: &goast.FuncLit{Type: &goast.FuncType{Results: &goast.FieldList{
			List: []*goast.Field{{Type: retType}},
		}},
			Body: &goast.BlockStmt{List: []goast.Stmt{
				&goast.AssignStmt{
					Tok: token.DEFINE,
					Lhs: []goast.Expr{goast.NewIdent("slice")},
					Rhs: []goast.Expr{expr},
				},
				&goast.ReturnStmt{Results: []goast.Expr{
					&goast.SliceExpr{
						X: goast.NewIdent("slice"),
					},
				}},
			}}},
	}
}

func (tp *Transpiler) transpileExpr(expr ir.Expr) (goast.Expr, error) {
	originalExpr := expr
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
		return tp.transpileFnCall(e)
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

	case *ir.ListLiteral:
		l, err := tp.transpileListLiteral(originalExpr)
		return l, err
	case *ir.Func:
		fnType, ok := tp.types.TypeOf(e).(*ir.FnType)
		if !ok {
			return nil, fmt.Errorf("expected function type for lambda")
		}

		params := &goast.FieldList{}
		for i, argName := range e.ArgNames {
			argType, err := tp.transpileType(fnType.Args[i])
			if err != nil {
				return nil, fmt.Errorf("lambda param %s: %v", argName, err)
			}
			params.List = append(params.List, &goast.Field{
				Names: []*goast.Ident{goast.NewIdent(argName)},
				Type:  argType,
			})
		}

		var results *goast.FieldList
		if !isUnitType(fnType.Return) {
			retType, err := tp.transpileType(fnType.Return)
			if err != nil {
				return nil, fmt.Errorf("lambda return: %v", err)
			}
			results = &goast.FieldList{
				List: []*goast.Field{{Type: retType}},
			}
		}

		body, err := tp.transpileExpressionToStatements(e.Body, "return")
		if err != nil {
			return nil, fmt.Errorf("lambda body: %v", err)
		}

		return &goast.FuncLit{
			Type: &goast.FuncType{Params: params, Results: results},
			Body: &goast.BlockStmt{List: body},
		}, nil

	case *ir.RecordLit:
		recType, err := tp.transpileType(tp.types.TypeOf(originalExpr))
		if err != nil {
			return nil, fmt.Errorf("for record literal, failed to transpile type: %v", err)
		}
		sorted := make([]ir.LabelValue, len(e.Fields))
		copy(sorted, e.Fields)
		slices.SortFunc(sorted, func(a, b ir.LabelValue) int {
			return cmp.Compare(a.Label.Name, b.Label.Name)
		})
		elts := make([]goast.Expr, len(sorted))
		for i, field := range sorted {
			val, err := tp.transpileExpr(field.Value)
			if err != nil {
				return nil, fmt.Errorf("for record literal field %s: %v", field.Label.Name, err)
			}
			elts[i] = &goast.KeyValueExpr{
				Key:   goast.NewIdent(field.Label.Name),
				Value: val,
			}
		}
		return &goast.CompositeLit{
			Type: recType,
			Elts: elts,
		}, nil
	default:
		return nil, fmt.Errorf("for expr, unexpected type %v", reflect.TypeOf(expr))
	}
}

func (tp *Transpiler) transpileFnCall(e *ir.Call) (expr goast.Expr, err error) {
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
	fnType := sync.OnceValue(func() ir.Type {
		return tp.types.TypeOf(e.Func)
	})
	args := make([]goast.Expr, 0, len(e.Args))
	ellipsis := token.Pos(0)
	for i, arg := range e.Args {
		var err error
		if i == len(e.Args)-1 {
			if ft, ok := fnType().(*ir.FnType); ok && ft.Variadic {

				// break down the list literal and inline each individual element into the Go vararg as arguments
				// TODO should that not be an optimisation, where we inline a list literal into
				//  each individual arg? Might be possible but I struggle how that could be done without
				//  decoupling the transpiler and TypeCtx, because we the 'optimised' ir.Expr would not type anymore!
				if arg, ok := arg.(*ir.ListLiteral); ok {
					for _, listLitArg := range arg.Args {
						goArg, err := tp.transpileFunctionParamExpr(listLitArg)
						if err != nil {
							return nil, fmt.Errorf("for call expr param, unexpected error: %v", err)
						}
						args = append(args, goArg)
					}
				} else {
					// ellipsis should only be set if we are NOT inlining the vararg args
					ellipsis = token.Pos(1)
					return nil, fmt.Errorf("TODO for call expr, not handling intermediary storage of the vararg argument")
				}
				continue
			}
		}
		goArg, err := tp.transpileFunctionParamExpr(arg)
		if err != nil {
			return nil, fmt.Errorf("for call expr param, unexpected error: %v", err)
		}
		args = append(args, goArg)
	}

	return &goast.CallExpr{
		Fun:      goFn,
		Args:     args,
		Ellipsis: ellipsis,
	}, nil
}

func (tp *Transpiler) transpileFunctionParamExpr(arg ir.Expr) (goast.Expr, error) {
	goArg, err := tp.transpileExpr(arg)
	if err != nil {
		return nil, fmt.Errorf("for call expr param, unexpected error: %w", err)
	}

	ret, err := tp.injectPotentialNothingType(goArg, tp.types.TypeOf(arg))
	if err != nil {
		return nil, fmt.Errorf("for call expr param, unexpected error when wrapping: %w", err)
	}

	return ret, nil
}

var s string = fmt.Sprintf(`hello, %s!`, []any{"a"}...)

func (tp *Transpiler) transpileListLiteral(expr ir.Expr) (goast.Expr, error) {
	typed := tp.types.TypeOf(expr)
	switch typed := typed.(type) {
	//case *ir.ListType:
	//	// indeterminate length: use a Go slice

	case *ir.ListLiteralType:
		asLiteral, ok := unwrapAscribe(expr).(*ir.ListLiteral)
		if !ok {
			return nil, fmt.Errorf("expected a list literal type")
		}
		transpiledExprs := make([]goast.Expr, 0, len(asLiteral.Args))
		for _, elem := range asLiteral.Args {
			transpiled, err := tp.transpileExpr(elem)
			if err != nil {
				return nil, fmt.Errorf("failed to transpile list literal element: %v", err)
			}
			transpiledExprs = append(transpiledExprs, transpiled)
		}
		goInner, err := tp.transpileType(typed.InnerType())
		if err != nil {
			return nil, fmt.Errorf("failed to transpile inner type: %v", err)
		}
		return &goast.CompositeLit{
			Type: &goast.ArrayType{
				Len: &goast.BasicLit{
					Kind:  token.INT,
					Value: strconv.Itoa(len(typed.ElementTypes)),
				},
				Elt: goInner,
			},
			Elts: transpiledExprs,
		}, nil
	default:
		return nil, fmt.Errorf("unexpected list literal type %v", reflect.TypeOf(typed))
	}
}

// transpileExpressionToStatements makes a []ast.Stmt
// that produces the result of expr, and places it on a final variable finalLocalVarName.
//
// The caller will then need to synthesise a final statement that use that value.
//
// Exceptionally, if finalLocalVarName is a string with
//
//   - "return":
//     then transpileExpressionToStatements makes the last statement a return statement
//     for the resulting ast.Expr.
//
//   - Empty string "": the result is not placed in any variables (useful for discarding results)
func (tp *Transpiler) transpileExpressionToStatements(expr ir.Expr, finalLocalVarName string) ([]goast.Stmt, error) {
	oldExpr := tp.currentExpr
	tp.currentExpr = expr
	defer func() {
		tp.currentExpr = oldExpr
	}()
	var statements []goast.Stmt
	var finalExpr goast.Expr
	switch e := expr.(type) {
	// add non inlineable Exprs here!

	// some ast.Expr we can inline directly to a Go expression and return that
	case *ir.RecordSelect, *ir.Literal, *ir.Call, *ir.Var, *ir.RecordLit, *ir.Func:
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
		t, err := tp.transpileType(tp.types.TypeOf(e.Value))
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
		finalStatement = &goast.ExprStmt{X: finalExpr}
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
