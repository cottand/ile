package main

import (
	"bytes"
	"embed"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/ile"
	"github.com/stretchr/testify/assert"
	"github.com/traefik/yaegi/stdlib"

	"github.com/traefik/yaegi/interp"
	"go/build"
	"go/format"
	"go/token"
	"io/fs"
	"path"
	"strings"
	"testing"
)

const logGoAST = true

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

func TestInteropEndToEnd(t *testing.T) {
	files, err := testSet.ReadDir("test/interop")
	assert.NoError(t, err)
	for _, f := range files {
		if f.IsDir() || !strings.HasSuffix(f.Name(), ".ile") {
			continue
		}
		testFile(t, "interop", f)
	}
}

func testFile(t *testing.T, at string, f fs.DirEntry) bool {
	return t.Run(f.Name(), func(t *testing.T) {
		name := path.Join(at, f.Name())
		if ignoreEndToEndTests[name] {
			t.Skip("marked as ignored: " + name)
		}

		defer func() {
			//if recover() != nil {
			//t.Fatalf("test panicked: %v", recover())
			//}
		}()
		content, err := testSet.ReadFile(path.Join("test", name))
		assert.NoError(t, err)

		eval, expected := extractTestComment(t, string(content))
		i := interp.New(interp.Options{GoPath: build.Default.GOPATH})
		err = i.Use(stdlib.Symbols)
		assert.NoError(t, err)

		pkg, cErrs, err := ile.NewPackageFromBytes(content)
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

		tp := backend.NewTranspiler(pkg.TypeCtx)
		goAst, err := tp.TranspilePackage(pkg.Name(), pkg.Syntax())
		assert.NoError(t, err)

		sourceBuf := bytes.NewBuffer(nil)
		firstGoFile := goAst[0]
		err = format.Node(sourceBuf, &token.FileSet{}, &firstGoFile)
		assert.NoError(t, err)

		_, err = i.Eval(sourceBuf.String())
		assert.NoError(t, err, "compilation errors: ----\n%s\n----", sourceBuf.String())

		resActual, err := i.Eval(eval)
		assert.NoError(t, err, "go program:\n-------\n%v---------", sourceBuf.String())
		if logGoAST {
			println("go AST:\n-------", sourceBuf.String(), "\n-------")
		}

		iClean := interp.New(interp.Options{})
		resExpected, err := iClean.Eval(expected)
		assert.NoError(t, err)

		assert.True(t, resExpected.Equal(resActual), "not equal: expected=(%v)%v actual=(%v)%v\nfor source:\n----\n%v\n----", resExpected.Type(), resExpected, resActual.Type(), resActual, sourceBuf.String())
	})
}
