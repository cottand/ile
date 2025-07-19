package ast

import (
	"encoding/binary"
	"hash/fnv"
)

// Node is the base interface for all AST nodes.
type Node interface {
	Positioner
	Hash() uint64
}

// Expr is the interface for all expression nodes in the AST.
type Expr interface {
	Node
	exprNode() // Marker method to distinguish expressions
}

// Stmt is the interface for all statement nodes in the AST.
type Stmt interface {
	Node
	stmtNode() // Marker method to distinguish statements
}

// Type is the interface for all type nodes in the AST.
type Type interface {
	Node
	typeNode() // Marker method to distinguish types
}

// File represents a source file in the AST.
type File struct {
	Range
	PkgName      string
	Declarations []Declaration
	Imports      []Import
}

// Hash returns a hash value for the File, based on its structural characteristics
func (f *File) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("File")
	_, _ = h.Write([]byte(f.PkgName))
	arr = binary.LittleEndian.AppendUint64(arr, f.Range.Hash())

	// Hash all declarations
	for _, decl := range f.Declarations {
		arr = binary.LittleEndian.AppendUint64(arr, (&decl).Hash())
	}

	// Hash all imports
	for _, imp := range f.Imports {
		arr = binary.LittleEndian.AppendUint64(arr, (&imp).Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// Import represents an import statement in the AST.
type Import struct {
	Range
	Alias      string // Empty string means no alias
	ImportPath string
}

// Hash returns a hash value for the Import, based on its structural characteristics
func (i *Import) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Import")
	_, _ = h.Write([]byte(i.Alias))
	_, _ = h.Write([]byte(i.ImportPath))
	arr = binary.LittleEndian.AppendUint64(arr, i.Range.Hash())

	_, _ = h.Write(arr)
	return h.Sum64()
}

// Declaration represents a top-level declaration in the AST.
type Declaration struct {
	Range
	Name     string
	Value    Expr
	TypeAnn  Type // Optional type annotation
	Comments []string
}

// Hash returns a hash value for the Declaration, based on its structural characteristics
func (d *Declaration) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Declaration")
	_, _ = h.Write([]byte(d.Name))
	arr = binary.LittleEndian.AppendUint64(arr, d.Range.Hash())

	if d.Value != nil {
		arr = binary.LittleEndian.AppendUint64(arr, d.Value.Hash())
	}

	if d.TypeAnn != nil {
		arr = binary.LittleEndian.AppendUint64(arr, d.TypeAnn.Hash())
	}

	for _, comment := range d.Comments {
		_, _ = h.Write([]byte(comment))
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}
