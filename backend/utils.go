package backend

import (
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"go/token"
)

const goVersion = "1.23.3"

var ileToGoVars = map[string]string{
	"True":  "true",
	"False": "false",
}

var ileToGoOperators = map[string]token.Token{
	"+":  token.ADD,
	"/":  token.QUO,
	"%":  token.REM,
	"-":  token.SUB,
	"*":  token.MUL,
	">":  token.GTR,
	"<":  token.LSS,
	"<=": token.LEQ,
	">=": token.GEQ,
	"==": token.EQL,
	"!=": token.NEQ,
}

// will need to handle more concrete tags, like random strings and specific ints,
// but for now we'll just do the basic types
func typeTagToGoType(t string) (goType string, ok bool) {
	switch t {
	case ir.IntTypeName:
		return "int64", true
	case ir.StringTypeName:
		return "string", true
	case ir.FloatTypeName:
		return "float64", true
	case ir.BoolTypeName, ir.TrueName, ir.FalseName:
		return "bool", true
	}
	return "", false
}

func nearestGoType(lit *ir.Literal) (goType string, err error) {
	switch lit.Kind {
	case token.INT:
		// TODO we would like to narrow types here (like 3 becomes uint8, not int64)
		return "int64", nil
	default:
		return "", fmt.Errorf("nearestGoType: unexpected literal type %s", lit.Kind.String())
	}
}

func isSuitableForGoConst(t ir.Type) bool {
	_, ok := t.(*ir.Literal)
	return ok
}

func unwrapAscribe(expr ir.Expr) ir.Expr {
	if ascribe, ok := expr.(*ir.Ascribe); ok {
		return unwrapAscribe(ascribe.Expr)
	}
	return expr
}