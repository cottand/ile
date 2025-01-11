package frontend_test

import (
	"bytes"
	"fmt"
	"github.com/cottand/ile/frontend"
	"github.com/stretchr/testify/assert"
	"slices"
	"strings"
	"testing"
)

func TestCompilerError(t *testing.T) {
	exprCases := map[string][]string{
		`1 + "a"`:        {"type mismatch", "Int", "String"},
		`"a" + 1`:        {"type mismatch", "Int", "String"},
		`a = 1; "a" + a`: {"type mismatch", "Int", "String"},
	}

	for expr, expected := range exprCases {
		t.Run(expr, func(t *testing.T) {
			progTemplate := fmt.Sprintf(`
package main

ExprTest = (%v)
		`, expr)
			_, errs, err := frontend.ParseToIR(bytes.NewBufferString(progTemplate))
			assert.NoError(t, err)
			if len(expected) == 0 {
				assert.Empty(t, errs)
				return
			}

			errsAsStrings := make([]string, len(errs.Errors()))
			for i, err := range errs.Errors() {
				errsAsStrings[i] = err.Error()
			}
			for _, expectedMessage := range expected {
				found := slices.ContainsFunc(errsAsStrings, func(s string) bool {
					return strings.Contains(s, expectedMessage)
				})
				assert.True(t, found, "expected to find message %s in %v", expectedMessage, errsAsStrings)
			}
		})
	}
}
