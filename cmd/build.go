package cmd

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/failed"
	"github.com/spf13/cobra"
	goast "go/ast"
	"go/format"
	"go/token"
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
	panic("implement me")
	//fs, _, err := frontend. NewFileset(args[0])
	//if err != nil {
	//	return err
	//}
	//f, cErrs, err := irFromFile(args[0])
	//if err != nil {
	//	return err
	//}
	//if cErrs.HasError() {
	//	for _, cErr := range cErrs.Errors() {
	//		log.Println("at " + fs.Position(cErr.At.Pos()).String())
	//		log.Println(cErr.Message)
	//	}
	//	return fmt.Errorf("%d errors occurred during build", len(cErrs.Errors()))
	//}
	//
	//tp := backend.Transpiler{}
	//transpiled, err := tp.TranspileFile(*f)
	//if err != nil {
	//	return err
	//}
	//return write(transpiled, args[0])
}

func write(goAst *goast.File, at string) error {
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

func irFromFile(at string) (*ast.File, *failed.CompileResult, error) {
	panic("implement me")
}
