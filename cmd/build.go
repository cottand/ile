package cmd

import (
	"fmt"
	"github.com/cottand/ile/backend"
	"github.com/cottand/ile/frontend"
	"github.com/cottand/ile/ir"
	"github.com/spf13/cobra"
	"go/ast"
	"go/format"
	"go/token"
	"log"
	"os"
	"path"
)

var BuildCmd = &cobra.Command{
	Use:          "build ./folder",
	Short:        "Build an ile project",
	RunE:         runBuild,
	Args:         cobra.MinimumNArgs(1),
	SilenceUsage: true,
}

func runBuild(cmd *cobra.Command, args []string) error {
	fs, _, err := frontend1.FilesetFrom(args[0])
	if err != nil {
		return err
	}
	f, cErrs, err := irFromFile(args[0])
	if err != nil {
		return err
	}
	if len(cErrs) > 0 {
		log.Printf("%d errors occurred during build\n", len(cErrs))
		for _, cErr := range cErrs {
			log.Println("at " + fs.Position(cErr.At.PosStart).String())
			log.Println(cErr.Message)
		}
		return fmt.Errorf("%d errors occurred during build", len(cErrs))
	}

	transpiled, err := backend.TranspileFile(*f)
	if err != nil {
		return err
	}
	return write(transpiled, args[0])
}

func write(goAst *ast.File, at string) error {
	p := path.Clean(at)
	f, err := os.Create(p + ".go")
	if err != nil {
		return fmt.Errorf("could not create file: %w", err)
	}

	err = format.Node(f, token.NewFileSet(), goAst)
	if err != nil {
		return fmt.Errorf("could not write to file: %w", err)
	}
	return nil
}

func irFromFile(at string) (*ir.File, []*ir.CompileError, error) {
	p := path.Clean(at)
	f, err := os.Open(p)
	if err != nil {
		return nil, nil, fmt.Errorf("could not read file %s: %w", p, err)
	}
	node, cErrs, err := frontend1.ParseToIR(f)
	if err != nil {
		return nil, nil, fmt.Errorf("could not compile file %s: %w", p, err)
	}
	return &node, cErrs, nil
}
