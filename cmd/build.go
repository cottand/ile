package cmd

import (
	"fmt"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/ile"
	"github.com/cottand/ile/internal/log"
	"github.com/spf13/cobra"
	goast "go/ast"
	"go/format"
	"go/token"
	"io/fs"
	"log/slog"
	"os"
	"path"
	"path/filepath"
	"strings"
)

var BuildCmd = &cobra.Command{
	Use:          "build ./folder|file.ile",
	Short:        "Build an ile project",
	RunE:         runBuild,
	Args:         cobra.MinimumNArgs(1),
	SilenceUsage: true,
}

var (
	buildOutPath *string
	logLevel     *int
)

func init() {
	buildOutPath = BuildCmd.Flags().StringP("out", "o", "", "output path")
	logLevel = BuildCmd.Flags().IntP("log-level", "l", int(slog.LevelError), "log level")
}

func buildAt(pkg *ile.Package, outPath string) error {
	if pkg.Errors().HasError() {
		sb := &strings.Builder{}
		for _, ileError := range pkg.Errors().Errors() {
			sb.WriteString("\n")
			sb.WriteString(ilerr.FormatWithCodeAndSource(ileError, pkg))
		}
		return fmt.Errorf("errors found during compilation:\n%s", sb.String())
	}

	tp := backend.NewTranspiler(pkg.TypeCtx)
	f, err := tp.TranspilePackage(pkg.Name(), pkg.Syntax())

	if err != nil {
		return fmt.Errorf("could not transpile package: %w", err)
	}

	if pkg.Errors().HasError() {
		sb := &strings.Builder{}
		for _, ileError := range pkg.Errors().Errors() {
			sb.WriteString("\n")
			sb.WriteString(ilerr.FormatWithCodeAndSource(ileError, pkg))
		}
		return fmt.Errorf("errors found during compilation:\n%s", sb.String())
	}

	for i, file := range f {
		if err := write(file, filepath.Join(outPath, file.Name.Name)); err != nil {
			return fmt.Errorf("could not write file %d: %w", i, err)
		}
	}
	if err := writeGoMod(filepath.Join(outPath, "go.mod")); err != nil {
		return fmt.Errorf("could not write go.mod: %w", err)
	}

	return nil
}

func runBuild(cmd *cobra.Command, args []string) error {
	log.SetLevel(slog.Level(*logLevel))

	target, err := filepath.Abs(args[0])
	if err != nil {
		return fmt.Errorf("could not get absolute path of target: %w", err)
	}

	stat, err := os.Stat(target)
	if err != nil {
		return fmt.Errorf("could not stat target: %w", err)
	}

	var folderFS fs.FS
	var rootDir string
	if stat.IsDir() {
		rootDir = target
		folderFS = os.DirFS(target)
	} else {
		parent := filepath.Dir(target)
		rootDir = parent
		folderFS = os.DirFS(parent)
	}

	err = os.Mkdir(*buildOutPath, os.ModePerm)
	if err != nil && !os.IsExist(err) {
		return fmt.Errorf("could not create output directory: %w", err)
	}

	pkg, err := ile.LoadPackage(folderFS, ile.PkgLoadSettings{
		MetadataRootDir: rootDir,
	})
	if err != nil {
		return fmt.Errorf("could not load package (this is a bug and not a compile error): %w", err)
	}
	return buildAt(pkg, *buildOutPath)
}

func writeGoMod(at string) error {
	f, err := os.Create(path.Clean(at))
	if err != nil {
		return fmt.Errorf("could not create file: %w", err)
	}

	_, err = f.WriteString(`
module ileProject

go 1.23.3
`)
	if err != nil {
		return fmt.Errorf("could not write to file: %w", err)
	}

	return nil

}

func write(goAst goast.File, at string) error {
	p := path.Clean(at)
	f, err := os.Create(p + ".go")
	if err != nil {
		return fmt.Errorf("could not create file: %w", err)
	}

	err = format.Node(f, token.NewFileSet(), &goAst)
	if err != nil {
		return fmt.Errorf("could not write to file: %w", err)
	}
	return nil
}
