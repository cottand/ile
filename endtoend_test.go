package main

import (
	"bytes"
	"context"
	"embed"
	"fmt"
	"go/format"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"runtime/debug"
	"strings"
	"testing"

	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/ile"
	"github.com/stretchr/testify/assert"
	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

const logGoAST = true

// embeds the test folder
//
//go:embed test
var testSet embed.FS

var testSetRoot = func() string {
	name, err := os.Getwd()
	if err != nil {
		return ""
	}
	return path.Join(name, "test")
}()

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

func TestGenericsEndToEnd(t *testing.T) {
	files, err := testSet.ReadDir("test/generics")
	assert.NoError(t, err)
	for _, f := range files {
		if f.IsDir() || !strings.HasSuffix(f.Name(), ".ile") {
			continue
		}
		testFile(t, "generics", f)
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
		t.Parallel()
		name := path.Join(at, f.Name())
		if ignoreEndToEndTests[name] {
			t.Skip("marked as ignored: " + name)
		}

		defer func() {
			if v := recover(); v != nil {
				t.Errorf("test panicked: %v", v)
				t.Log(string(debug.Stack()))
			}
		}()

		content, err := testSet.ReadFile(path.Join("test", name))
		assert.NoError(t, err)

		eval, expected := extractTestComment(t, string(content))

		pkg, cErrs, err := ile.NewPackageFromBytes(content, path.Join(testSetRoot, name))
		assert.NoError(t, err, "Unexpected failures in NewPackageFromBytes")
		if err != nil {
			// the FE had errors, so we add a recover() so that we can handle nil panics
			// due to those errors
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("test panicked: %v", r)
				}
			}()
		}
		if cErrs.HasError() {
			var errStrings []string
			for _, e := range cErrs.Errors() {
				errStrings = append(errStrings, ilerr.FormatWithCodeAndSource(e, pkg))
			}
			// we don't try to continue for proper compile errors
			assert.Empty(t, cErrs.Errors(), "compilation errors found: %s", strings.Join(errStrings, ", "))
			return
		}
		typsStr, err := pkg.DisplayTypes()
		assert.NoError(t, err)
		t.Logf("program types:\n---\n%s---", typsStr)

		tp := backend.NewTranspiler(pkg.TypeCtx)
		goAst, err := tp.TranspilePackage(pkg.Name(), pkg.Syntax())
		assert.NoError(t, err)

		sourceBuf := bytes.NewBuffer(nil)
		firstGoFile := goAst[0]
		err = format.Node(sourceBuf, &token.FileSet{}, &firstGoFile)
		assert.NoError(t, err)

		// print program types again just so they are at the bottom of the logs
		t.Logf("program types:\n---\n%s---", typsStr)
		t.Log("go AST:\n-------\n", sourceBuf.String(), "\n-------")

		testEvalWithWazero(t, sourceBuf.String(), eval, expected)
	})
}

func testEvalWithWazero(t *testing.T, program string, expectedExpr, evalActualExpr string) {
	t.Helper()

	pkgName := extractPackageName(t, program)
	mainProgram := strings.Replace(program, "package "+pkgName, "package main", 1)

	tmpDir := t.TempDir()

	goMod := fmt.Sprintf("module tmpmod\n\ngo %s\n", goVersion())
	err := os.WriteFile(filepath.Join(tmpDir, "go.mod"), []byte(goMod), 0644)
	if !assert.NoError(t, err) {
		return
	}

	err = os.WriteFile(filepath.Join(tmpDir, "gen.go"), []byte(mainProgram), 0644)
	if !assert.NoError(t, err) {
		return
	}

	mainGo := fmt.Sprintf(`package main

import (
	"fmt"
	"os"
	"reflect"
)

func main() {
	actual := %s
	expected := %s
	if reflect.DeepEqual(actual, expected) {
		fmt.Fprint(os.Stdout, "PASS")
	} else {
		fmt.Fprintf(os.Stdout, "FAIL: expected=(%%T)%%v actual=(%%T)%%v", expected, expected, actual, actual)
	}
}
`, evalActualExpr, expectedExpr)

	err = os.WriteFile(filepath.Join(tmpDir, "main.go"), []byte(mainGo), 0644)
	if !assert.NoError(t, err) {
		return
	}

	wasmPath := filepath.Join(tmpDir, "output.wasm")
	cmd := exec.Command("go", "build", "-o", wasmPath, ".")
	cmd.Dir = tmpDir
	cmd.Env = append(os.Environ(), "GOOS=wasip1", "GOARCH=wasm")
	buildOutput, err := cmd.CombinedOutput()
	if !assert.NoError(t, err, "go build to wasm failed:\n%s", string(buildOutput)) {
		return
	}

	wasmBytes, err := os.ReadFile(wasmPath)
	if !assert.NoError(t, err) {
		return
	}

	ctx := context.Background()
	rt := wazero.NewRuntime(ctx)
	defer rt.Close(ctx)

	wasi_snapshot_preview1.MustInstantiate(ctx, rt)

	var stdout, stderr bytes.Buffer
	config := wazero.NewModuleConfig().
		WithStdout(&stdout).
		WithStderr(&stderr)

	_, err = rt.InstantiateWithConfig(ctx, wasmBytes, config)
	if err != nil {
		t.Fatalf("wasm execution failed: %v\nstderr: %s", err, stderr.String())
	}

	result := stdout.String()
	if !strings.HasPrefix(result, "PASS") {
		t.Fatalf("wazero eval: %s", result)
	}
}

func extractPackageName(t *testing.T, source string) string {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "", source, parser.PackageClauseOnly)
	if !assert.NoError(t, err, "failed to parse package name from generated source") {
		t.FailNow()
	}
	return f.Name.Name
}

var goVersionCached string

func goVersion() string {
	if goVersionCached != "" {
		return goVersionCached
	}
	out, err := exec.Command("go", "env", "GOVERSION").Output()
	if err != nil {
		return "1.24"
	}
	v := strings.TrimSpace(string(out))
	v = strings.TrimPrefix(v, "go")
	parts := strings.SplitN(v, ".", 3)
	if len(parts) >= 2 {
		goVersionCached = parts[0] + "." + parts[1]
	} else {
		goVersionCached = v
	}
	return goVersionCached
}
