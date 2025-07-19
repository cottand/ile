package backend

import (
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	goast "go/ast"
	"go/token"
)

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
		case *ir.AnyType:
			// -> `else { branchResultPath }`
			currentIf.Else = &goast.BlockStmt{List: branchResultPath}
			continue
		case *ir.TypeName:
			if astType.Name == ir.TrueName {
				addIf(&goast.IfStmt{
					Cond: subjectIdent,
					Body: &goast.BlockStmt{List: branchResultPath},
				})
				continue
			}
			if astType.Name == ir.FalseName {
				addIf(&goast.IfStmt{
					Cond: &goast.UnaryExpr{
						Op: token.NOT,
						X:  subjectIdent,
					},
					Body: &goast.BlockStmt{List: branchResultPath},
				})
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
