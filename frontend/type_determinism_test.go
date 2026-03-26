package frontend_test

import (
	"fmt"
	"testing"

	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/ile"
	"github.com/stretchr/testify/assert"
)

func TestDeterministicTypes(t *testing.T) {
	exprCases := map[string]struct {
		Type  string
		Cases []string
	}{
		"switch case order": {
			Type: `"red" | "blue" | "green"`,
			Cases: []string{
				`when x {
  "a" -> "blue"
  "b" -> "green"
  "c" -> "red"
}`,
				`when x {
  "c" -> "red"
  "b" -> "green"
  "a" -> "blue"
}`,
				`when x {
  "b" -> "green"
  "c" -> "red"
  "a" -> "blue"
}`,
			},
		},

		"switch case keys": {
			Type: `"red" | "blue" | "green"`,
			Cases: []string{
				`when x {
  "a" -> "blue"
  "b" -> "green"
  "c" -> "red"
}`,
				`when x {
  "a" -> "red"
  "b" -> "green"
  "c" -> "blue"
}`,
				`when x {
  "a" -> "green"
  "b" -> "red"
  "c" -> "blue"
}`,
			},
		},
	}
	for name, testCase := range exprCases {
		t.Run(name, func(t *testing.T) {
			for _, c := range testCase.Cases {

				progTemplate := fmt.Sprintf(`
package main

val x: String = "a"

val exprTest = (%v)
		`, c)

				pkg, errs, err := ile.NewPackageFromBytes([]byte(progTemplate), "test")
				assert.NoError(t, err)
				if errs.HasError() {
					println(progTemplate)
					t.Fatal(errs.Errors())
				}

				actualT := ir.TypeString(pkg.Syntax()[0].Declarations[1].Type)

				assert.Equal(t, testCase.Type, actualT, "expected types to be equal")
			}
		})
	}
}
