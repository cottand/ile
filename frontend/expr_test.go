package frontend

import (
	"bytes"
	"fmt"
	"github.com/cottand/ile/ir"
	"github.com/stretchr/testify/assert"
	"go/token"
	"slices"
	"strings"
	"testing"
)

func testExpr(t *testing.T, expr string, expected ir.Expr) {
	t.Run("expression "+expr, func(t *testing.T) {
		f := fmt.Sprintf(`
package main

a = %s
`, expr)
		irFile, _ := testAntlrParse(t, f)
		actual := irFile.Values[0].E

		assertTypesEqual(t, expr, expected, actual)
	})
}

func testBadExpr(t *testing.T, expr string, containsErr ...string) {
	t.Run("failing expression "+expr, func(t *testing.T) {
		f := fmt.Sprintf(`
package main

a = %s

`, expr)

		_, cErrs, err := ParseToIR(bytes.NewBufferString(f))
		assert.NoError(t, err)
		assert.NotEmpty(t, cErrs)
		messages := make([]string, 0)
		found := slices.ContainsFunc(cErrs, func(err *ir.CompileError) bool {
			messages = append(messages, err.Message)
			for _, c := range containsErr {
				if !strings.Contains(err.Message, c) {
					return false
				}
			}
			return true
		})
		if !found {
			t.Fatalf("expected to find %s in %v", containsErr, messages)
		}
	})
}

func assertTypesEqual(t *testing.T, parentExp string, expected ir.Expr, actual ir.Expr) {
	assert.IsType(t, expected, actual)

	switch expected := expected.(type) {
	case ir.BasicLitExpr:
		actual := actual.(ir.BasicLitExpr)
		assert.Equal(t, expected.Kind, actual.Kind, "original expr: "+parentExp)
		assert.Equal(t, expected.Value, actual.Value, "original expr: "+parentExp)

	default:
		t.Error("type comparison not implemented yet")
	}
}

func TestStringLits(t *testing.T) {
	testExpr(t, `"aa"`, ir.BasicLitExpr{
		Kind:  token.STRING,
		Value: "aa",
	})
	testExpr(t, "`aa`", ir.BasicLitExpr{
		Kind:  token.STRING,
		Value: "aa",
	})
	testExpr(t, "`aa\n`", ir.BasicLitExpr{
		Kind:  token.STRING,
		Value: "aa\n",
	})
}
func TestStringEscapingLits(t *testing.T) {
	t.Skip("TODO string escaping")
	testExpr(t, `"aa\n"`, &ir.BasicLitExpr{
		Kind:  token.STRING,
		Value: "aa\n",
	})
}

func TestBadBinaryOps(t *testing.T) {
	testBadExpr(t, `1 + "a"`, "type mismatch", "Int", "String")
	t.Skip("HM type inference struggles to handle non happy paths")
	testBadExpr(t, `1 + a`, "undefined variable", "a")
}
