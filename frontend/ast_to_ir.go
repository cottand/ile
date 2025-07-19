package frontend

import (
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ir"
	"hash/fnv"
	"log/slog"
	"reflect"
)

// desugarExpr converts an AST expression to an IR expression
func desugarExpr(expr ast.Expr) ir.Expr {
	if expr == nil {
		return nil
	}

	switch e := expr.(type) {
	case *ast.Identifier:
		return &ir.Var{
			Name:  e.Name,
			Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.Literal:
		return &ir.Literal{
			Syntax: e.Value,
			Kind:   e.Kind,
			Range:  ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.CallExpr:
		args := make([]ir.Expr, len(e.Args))
		for i, arg := range e.Args {
			args[i] = desugarExpr(arg)
		}
		return &ir.Call{
			Func:  desugarExpr(e.Function),
			Args:  args,
			Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.FuncLit:
		params := make([]string, len(e.Params))
		for i, param := range e.Params {
			params[i] = param.Name
		}
		return &ir.Func{
			ArgNames: params,
			Body:     desugarExpr(e.Body),
			Range:    ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.BinaryExpr:
		// Binary expressions need to be converted to function calls
		return &ir.Call{
			Func: &ir.Var{
				Name:  e.Operator.String(),
				Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
			},
			Args:  []ir.Expr{desugarExpr(e.Left), desugarExpr(e.Right)},
			Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.UnaryExpr:
		// Unary expressions need to be converted to function calls
		return &ir.Call{
			Func: &ir.Var{
				Name:  e.Operator.String(),
				Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
			},
			Args:  []ir.Expr{desugarExpr(e.Operand)},
			Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.SelectExpr:
		return &ir.RecordSelect{
			Record: desugarExpr(e.X),
			Label:  e.Sel,
			Range:  ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.ListLit:
		elements := make([]ir.Expr, len(e.Elements))
		for i, elem := range e.Elements {
			elements[i] = desugarExpr(elem)
		}
		return &ir.ListLiteral{
			Args:       elements,
			Positioner: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.StructLit:
		fields := make([]ir.LabelValue, len(e.Fields))
		for i, field := range e.Fields {
			fields[i] = ir.LabelValue{
				Label: ir.Var{
					Name:  field.Name,
					Range: ir.Range{PosStart: field.Range.PosStart, PosEnd: field.Range.PosEnd},
				},
				Value: desugarExpr(field.Value),
			}
		}
		return &ir.RecordLit{
			Fields: fields,
			Range:  ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.WhenExpr:
		cases := make([]ir.WhenCase, len(e.Cases))
		for i, c := range e.Cases {
			cases[i] = ir.WhenCase{
				Pattern: desugarPattern(c.Pattern),
				Value:   desugarExpr(c.Body),
			}
		}
		return &ir.WhenMatch{
			Value:      desugarExpr(e.Target),
			Cases:      cases,
			Positioner: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.ParenExpr:
		// Parentheses are just for grouping in the source code, so we can just convert the expression inside
		return desugarExpr(e.X)
	case *ast.VarDecl:
		// Variable declarations are converted to assignments
		var typeAnn ir.Type
		if e.TypeAnn != nil {
			typeAnn = ConvertType(e.TypeAnn)
		}

		expr := desugarExpr(e.Value)
		if typeAnn != nil {
			expr = &ir.Ascribe{
				Expr:  expr,
				Type_: typeAnn,
				Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
			}
		}

		// When we're processing a variable declaration, we need to create an Assign node
		// that will be properly linked with the next expression in the block.
		// The Body field will be set by the caller for top-level declarations,
		// but for declarations inside blocks, we need to handle it here.
		return &ir.Assign{
			Var:   e.Name,
			Value: expr,
			Body:  nil, // This will be set by the caller
			Range: ir.Range{PosStart: e.Range.PosStart, PosEnd: e.Range.PosEnd},
		}
	case *ast.BlockExpr:
		// Handle block expressions by chaining them together
		// The last expression is the result of the block
		// Previous expressions are chained using Unused nodes
		if len(e.Expressions) == 0 {
			// Empty block, return an error
			return &ir.ErrorExpr{
				Range:  ir.Range{PosStart: e.Pos(), PosEnd: e.End()},
				Syntax: "Empty block expression",
			}
		}

		// Start with the last expression as the result
		result := desugarExpr(e.Expressions[len(e.Expressions)-1])

		// Chain the previous expressions in reverse order
		for i := len(e.Expressions) - 2; i >= 0; i-- {
			expr := desugarExpr(e.Expressions[i])

			// If the expression is a variable declaration (which becomes an Assign),
			// set its Body field to the next expression in the block
			if assign, ok := expr.(*ir.Assign); ok {
				assign.Body = result
				result = assign
			} else {
				// Otherwise, use an Unused node to chain the expressions
				result = &ir.Unused{
					Value: expr,
					Body:  result,
					Range: ir.Range{PosStart: e.Expressions[i].Pos(), PosEnd: result.End()},
				}
			}
		}

		return result
	default:
		slog.With("section", "desugar.expr").Warn("unsupported expression type", "type", reflect.TypeOf(e))
		// If we don't know how to convert this expression, return an error expression
		return &ir.ErrorExpr{
			Range:  ir.Range{PosStart: e.Pos(), PosEnd: e.End()},
			Syntax: "Unsupported expression type",
		}
	}
}

// desugarPattern converts an AST pattern to an IR pattern
func desugarPattern(pattern ast.Type) ir.MatchPattern {

	// Desugaring for discard pattern `_`
	if typeName, ok := pattern.(*ast.TypeName); ok && typeName.Name == "_" {
		// Use a TypeVar with a special name to represent the wildcard
		return &ir.MatchTypePattern{
			Type_: &ir.AnyType{
				Positioner: typeName,
			},
			Positioner: ir.RangeOf(pattern),
		}
	}

	// Default case: convert the pattern as a type
	return &ir.MatchTypePattern{
		Type_:      ConvertType(pattern),
		Positioner: ir.Range{PosStart: pattern.Pos(), PosEnd: pattern.End()},
	}
}

// ConvertType converts an AST type to an IR type
func ConvertType(t ast.Type) ir.Type {
	if t == nil {
		return nil
	}

	switch typ := t.(type) {
	case *ast.TypeName:
		return &ir.TypeName{
			Name:  typ.Name,
			Range: ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.FuncType:
		params := make([]ir.Type, len(typ.ParamTypes))
		for i, param := range typ.ParamTypes {
			params[i] = ConvertType(param)
		}
		return &ir.FnType{
			Args:   params,
			Return: ConvertType(typ.ReturnType),
			Range:  ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.ListType:
		return &ir.ListType{
			ElementType: ConvertType(typ.ElementType),
			Positioner:  ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.StructType:
		fields := make([]ir.RecordField, len(typ.Fields))
		for i, field := range typ.Fields {
			fields[i] = ir.RecordField{
				Name: ir.Var{
					Name:  field.Name,
					Range: ir.Range{PosStart: field.Range.PosStart, PosEnd: field.Range.PosEnd},
				},
				Type: ir.FieldType{
					Out:   ConvertType(field.Type),
					Range: ir.Range{PosStart: field.Range.PosStart, PosEnd: field.Range.PosEnd},
				},
				Range: ir.Range{PosStart: field.Range.PosStart, PosEnd: field.Range.PosEnd},
			}
		}
		return &ir.RecordType{
			Fields: fields,
			Range:  ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.UnionType:
		return &ir.UnionType{
			Left:       ConvertType(typ.Left),
			Right:      ConvertType(typ.Right),
			Positioner: ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.IntersectionType:
		return &ir.IntersectionType{
			Left:       ConvertType(typ.Left),
			Right:      ConvertType(typ.Right),
			Positioner: ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
		}
	case *ast.TypeApplication:
		args := make([]ir.Type, len(typ.Args))
		for i, arg := range typ.Args {
			args[i] = ConvertType(arg)
		}

		baseType := ConvertType(typ.Base)
		if typeName, ok := baseType.(*ir.TypeName); ok {
			return &ir.AppliedType{
				Base:       *typeName,
				Args:       args,
				Positioner: ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
			}
		}

		// If the base type is not a TypeName, return an error type
		return &ErrorType{
			Range:  ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
			Syntax: "Applied type base must be a type name",
		}
	case *ast.TypeLiteral:
		// Type literals are converted to type names
		if typ.Value != nil {
			return &ir.Literal{
				Syntax: typ.Value.Value,
				Kind:   typ.Value.Kind,
				Range:  ir.RangeOf(typ.Value),
			}
		}

		// If the value is nil, return an error type
		return &ErrorType{
			Range:  ir.Range{PosStart: typ.Range.PosStart, PosEnd: typ.Range.PosEnd},
			Syntax: "Type literal value is nil",
		}
	default:
		// If we don't know how to convert this type, return an error type
		return &ErrorType{
			Range:  ir.Range{PosStart: t.Pos(), PosEnd: t.End()},
			Syntax: "Unsupported type",
		}
	}
}

// ErrorType is a type that represents an error in the type system
type ErrorType struct {
	ir.Range
	Syntax string
}

func (t *ErrorType) ShowIn(ctx ir.ShowCtx, _ uint16) string {
	return "ErrorType(" + t.Syntax + ")"
}

func (t *ErrorType) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("ErrorType"))
	_, _ = h.Write([]byte(t.Syntax))
	return h.Sum64()
}
