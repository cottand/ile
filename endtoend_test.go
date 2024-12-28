package ile

import (
	"bytes"
	"embed"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend"
	"github.com/stretchr/testify/assert"
	"github.com/traefik/yaegi/interp"
	"go/format"
	"go/token"
	"path"
	"strings"
	"testing"
)

// embeds the test folder
//
//go:embed test/*.ile
var testSet embed.FS

// format is as follows:
//
//	//ile:compilerTest expr to evaluate | expected value
func extractTestComment(t *testing.T, str string) (eval, expected string) {
	firstLine := strings.Split(str, "\n")[0]
	trimmed := strings.TrimPrefix(firstLine, "//ile:compilerTest ")
	elems := strings.Split(trimmed, "|")
	if len(elems) < 2 {
		t.Fatalf("could not parse comment string: '%v'", firstLine)
	}
	return elems[0], elems[1]
}

func TestAllEndToEnd(t *testing.T) {
	files, err := testSet.ReadDir("test")
	assert.NoError(t, err)
	for _, f := range files {
		t.Run(f.Name(), func(t *testing.T) {
			content, err := testSet.ReadFile(path.Join("test", f.Name()))
			assert.NoError(t, err)

			eval, expected := extractTestComment(t, string(content))
			i := interp.New(interp.Options{})
			assert.NoError(t, err)

			transpiled, _, err := frontend.ParseToIR(string(content))
			assert.NoError(t, err)

			goAst, err := backend.TranspileFile(transpiled)
			assert.NoError(t, err)

			sourceBuf := bytes.NewBuffer(nil)

			//_ = format.Node(sourceBuf, token.NewFileSet(), goAst)
			//t.Errorf("could not compile AST: %v\n%v", err, sourceBuf.String())
			prog, err := i.CompileAST(goAst)
			if err != nil {
				// Print the generated Go code
				_ = format.Node(sourceBuf, token.NewFileSet(), goAst)
				t.Fatalf("could not compile AST: %v\n%v", err, sourceBuf.String())
			}
			_, err = i.Execute(prog)
			assert.NoError(t, err)

			resActual, err := i.Eval(eval)
			assert.NoError(t, err)

			iClean := interp.New(interp.Options{})
			resExpected, err := iClean.Eval(expected)
			assert.NoError(t, err)

			assert.True(t, resExpected.Equal(resActual), "not equal: expected=%v actual=%v ", resExpected, resActual)
		})
	}
}
