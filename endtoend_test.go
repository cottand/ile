package main

import (
	"bytes"
	"embed"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/stretchr/testify/assert"
	"github.com/traefik/yaegi/interp"
	"go/format"
	"go/token"
	"io/fs"
	"path"
	"strings"
	"testing"
)

// embeds the test folder
//
//go:embed test
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

var ignoreEndToEndTests = map[string]bool{
	// test uses when-without-subject which is not happening anymore,
	// ignored until refactored
	"expressions/whenBoolFib.ile":    true,
	"expressions/whenSimpleBool.ile": true,
}

func TestExpressionsEndToEnd(t *testing.T) {
	files, err := testSet.ReadDir("test/expressions")
	assert.NoError(t, err)
	for _, f := range files {
		if f.IsDir() || !strings.HasSuffix(f.Name(), ".ile") {
			continue
		}
		testFile(t, "expressions", f)
	}
}

func TestFunctionsEndToEnd(t *testing.T) {
	files, err := testSet.ReadDir("test/functions")
	assert.NoError(t, err)
	for _, f := range files {
		if f.IsDir() || !strings.HasSuffix(f.Name(), ".ile") {
			continue
		}
		testFile(t, "functions", f)
	}
}

func testFile(t *testing.T, at string, f fs.DirEntry) bool {
	return t.Run(f.Name(), func(t *testing.T) {
		name := path.Join(at, f.Name())
		if ignoreEndToEndTests[name] {
			t.Skip("marked as ignored")
		}
		content, err := testSet.ReadFile(path.Join("test", name))
		assert.NoError(t, err)

		eval, expected := extractTestComment(t, string(content))
		i := interp.New(interp.Options{})
		assert.NoError(t, err)

		transpiled, cErrs, err := frontend.NewPackageFromBytes(content)
		assert.NoError(t, err)
		if err != nil {
			// don't try to keep going if the FE failed
			t.Fatal(err)
		}
		if cErrs.HasError() {
			var errStrings []string
			for _, e := range cErrs.Errors() {
				errStrings = append(errStrings, ilerr.FormatWithCode(e))
			}
			assert.Empty(t, cErrs.Errors(), "compilation errors found: %s", strings.Join(errStrings, ", "))
		}

		tp := backend.Transpiler{}
		goAst, err := tp.TranspilePackage(transpiled)
		assert.NoError(t, err)

		sourceBuf := bytes.NewBuffer(nil)
		//defer func() {
		//	err := recover()
		//	if err != nil {
		//		// Print the generated Go code
		//		t.Errorf("could not compile AST: %v\n-----\n%v", err, goAst)
		//		panic(err)
		//	}
		//}()

		prog, err := i.CompileAST(goAst)
		_ = format.Node(sourceBuf, token.NewFileSet(), goAst)
		if err != nil {
			t.Fatalf("could not compile AST: %v\n%v\nfrom original IR:\n%v", err, sourceBuf.String(), transpiled)
		}
		_, err = i.Execute(prog)
		assert.NoError(t, err)

		resActual, err := i.Eval(eval)
		assert.NoError(t, err, "go program:\n-------\n%v---------", sourceBuf.String())

		iClean := interp.New(interp.Options{})
		resExpected, err := iClean.Eval(expected)
		assert.NoError(t, err)

		assert.True(t, resExpected.Equal(resActual), "not equal: expected=%v actual=%v ", resExpected, resActual)
	})
}
