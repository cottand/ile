package util

import (
	"fmt"
	"go/ast"
	"strconv"
)

// StringTakeUntil returns the string up to and excluding char as well as the remainder excluding char
//
// if char was not found, then tail returns the empty string
func StringTakeUntil(s string, char rune) (head string, tail string) {
	for i, r := range s {
		if r == char && len(s[i:]) != 0 {
			return s[:i], s[i+1:]
		}
	}
	return s, ""
}

// MangledIdentFrom returns a deterministic string resulting from pos and name, which is also a valid Go identifier
//
// It is useful when creating intermediary identifiers to be used in codegen/desugaring,
// because if we are not careful and use arbitrary strings, we can end up with naming conflicts.
//
// Therefore, it serves 2 scenarios:
//   - Repeatedly generating names from an ast.Node that we plan to reuse (so we need determinism)
//   - Generating several names from a type of ast.Node, which we want to lead to different names if and only if
//     the ast.Node has not changed
func MangledIdentFrom(node ast.Node, name string) string {
	start := strconv.Itoa(int(node.Pos()))
	end := strconv.Itoa(int(node.End()))
	return fmt.Sprintf("ile_%v_at_%v_%v", name, start, end)
}
