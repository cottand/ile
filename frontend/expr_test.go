package frontend

import (
	"fmt"
	"github.com/cottand/ile/ir"
	"github.com/stretchr/testify/assert"
	"go/token"
	"testing"
)

func testExpr(t *testing.T, expr string, expected ir.Expr) {
	f := fmt.Sprintf(`
package main

a = %s
`, expr)
	irFile, _ := testAntlrParse(t, f)
	actual := irFile.Declarations[0].E

	assertTypesEqual(t, expr, expected, actual)

}

func assertTypesEqual(t *testing.T, parentExp string, expected ir.Expr, actual ir.Expr) {
	assert.IsType(t, expected, actual)

	switch expected := expected.(type) {
	case *ir.BasicLit:
		actual := actual.(*ir.BasicLit)
		assert.Equal(t, expected.Kind, actual.Kind, "original expr: "+parentExp)
		assert.Equal(t, expected.Value, actual.Value, "original expr: "+parentExp)

	default:
		t.Error("type comparison not implemented yet")
	}
}

func TestStringLits(t *testing.T) {
	testExpr(t, `"aa"`, &ir.BasicLit{
		Kind:  token.STRING,
		Value: "aa",
	})
	testExpr(t, "`aa`", &ir.BasicLit{
		Kind:  token.STRING,
		Value: "aa",
	})
	testExpr(t, "`aa\n`", &ir.BasicLit{
		Kind:  token.STRING,
		Value: "aa\n",
	})
}
func TestStringEscapingLits(t *testing.T) {
	t.Skip("TODO string escaping")
	testExpr(t, `"aa\n"`, &ir.BasicLit{
		Kind:  token.STRING,
		Value: "aa\n",
	})
}
