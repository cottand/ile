package ile

import (
	"github.com/cottand/ile/frontend/ilerr"
	_ "github.com/cottand/ile/internal/log"
	"github.com/stretchr/testify/assert"
	"strings"
	"testing"
)

func testError(t *testing.T, prog string, shouldContain ...string) {
	pkg, ilerrs, err := NewPackageFromBytes([]byte(prog), "test.ile")
	assert.NoError(t, err)

	if !ilerrs.HasError() {
		ts, _ := pkg.DisplayTypes()
		t.Log("\n" + ts)
		t.Fatal("expected an error")
	}

	sb := strings.Builder{}
	for _, err := range ilerrs.Errors() {
		sb.WriteString(ilerr.FormatWithCodeAndSource(err, pkg))
		sb.WriteString("\n-----------\n")
	}
	errMsg := sb.String()
	for _, s := range shouldContain {
		assert.Contains(t, errMsg, s)
	}
	t.Log("error message:\n" + errMsg)
}

func TestErrorOffsetEOF(t *testing.T) {
	prog := `package main


// asd
val a = 1 + 2 + 

`
	testError(t, prog, "test.ile:7:1:")
}

func TestErrorOffsetStartOfLine(t *testing.T) {
	prog := `package main

val a = 1


fn main() {
 val b = 2
 b + 1 +
}`
	testError(t, prog, "test.ile:9:1:")
}

func TestMultilineHighlights(t *testing.T) {
	prog := `package main

// a long comment
fn other(a) {
  a + 2
}

fn aa(x: String) {
  other(x)
}`

	testError(t, prog, "test.ile:9:3:")
}

func TestErrorOffsetLongFile(t *testing.T) {
	prog := `package main

















val a = 1 + 2 + 
// aa
val b = 2
// other comment
`
	testError(t, prog, "test.ile:21:1")
}

func TestFibValidation(t *testing.T) {
	testError(t, `package main

fn thing(x) {
  when(x) {
    1 -> 2
    0 -> 3
  }
}

fn main() {
  thing(3)
}
`, "mismatch")
}
