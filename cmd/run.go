package cmd

import (
	"fmt"
	"github.com/cottand/ile/ile"
	"github.com/cottand/ile/internal/log"
	"github.com/spf13/cobra"
	"io/fs"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
)

var RunCmd = &cobra.Command{
	Use:          "run [./folder|file.ile]",
	Short:        "Run an ile project",
	RunE:         runRun,
	Args:         cobra.MinimumNArgs(1),
	SilenceUsage: true,
}

func runRun(cmd *cobra.Command, args []string) error {
	tmpFolder, err := os.MkdirTemp("", "ile-run-*")
	if err != nil {
		return err
	}
	defer func(path string) {
		_ = os.RemoveAll(path)
	}(tmpFolder)

	log.SetLevel(slog.LevelError)

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

	pkg, err := ile.LoadPackage(folderFS, ile.PkgLoadSettings{
		MetadataRootDir: rootDir,
	})
	if err != nil {
		return fmt.Errorf("could not load package (this is a bug and not a compile error): %w", err)
	}

	err = buildAt(pkg, tmpFolder)
	if err != nil {
		return fmt.Errorf("could not build package: %w", err)
	}

	// run go run:
	command := exec.CommandContext(cmd.Context(), "go", "build", "./.")
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	command.Dir = tmpFolder
	err = command.Run()
	if err != nil {
		return fmt.Errorf("could not run go build: %w", err)
	}

	command = exec.CommandContext(cmd.Context(), filepath.Join(tmpFolder, "ileProject"))
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	err = command.Run()
	if err != nil {
		return err
	}
	return nil
}
