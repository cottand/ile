//go:build !( js || wasm)

package main

import (
	"github.com/cottand/ile/cmd"
	"github.com/spf13/cobra"
	"os"
)

func main() {
	err := rootCmd.Execute()
	if err != nil {
		//_, _ = fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

var rootCmd = &cobra.Command{
	Use:   "ile [subcommand]",
	Short: "ile 🌴\n a chill and pragmatic general-purpose functional language",
	Args:  cobra.MinimumNArgs(1),
	//SilenceErrors: true,
	SilenceUsage: true,
}

func init() {
	rootCmd.AddCommand(cmd.BuildCmd)
	rootCmd.AddCommand(cmd.RunCmd)
}
