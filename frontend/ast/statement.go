package ast

import (
	"encoding/binary"
	"hash/fnv"
)

// All statement types implement the Stmt interface

// BlockStmt represents a block of statements enclosed in braces.
type BlockStmt struct {
	Range
	Stmts []Stmt
}

func (s *BlockStmt) stmtNode() {}

// Hash returns a hash value for the BlockStmt, based on its structural characteristics
func (s *BlockStmt) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("BlockStmt")
	arr = binary.LittleEndian.AppendUint64(arr, s.Range.Hash())

	for _, stmt := range s.Stmts {
		if stmt != nil {
			arr = binary.LittleEndian.AppendUint64(arr, stmt.Hash())
		}
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// ExprStmt represents an expression used as a statement.
type ExprStmt struct {
	Range
	X Expr
}

func (s *ExprStmt) stmtNode() {}

// Hash returns a hash value for the ExprStmt, based on its structural characteristics
func (s *ExprStmt) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("ExprStmt")
	arr = binary.LittleEndian.AppendUint64(arr, s.Range.Hash())

	if s.X != nil {
		arr = binary.LittleEndian.AppendUint64(arr, s.X.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// FuncDecl represents a function declaration.
type FuncDecl struct {
	Range
	Name         string
	TypeParams   []TypeParam // Optional type parameters
	Params       []Parameter
	ReturnType   Type // Optional return type
	Body         *BlockStmt
}

func (s *FuncDecl) stmtNode() {}

// Hash returns a hash value for the FuncDecl, based on its structural characteristics
func (s *FuncDecl) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("FuncDecl")
	_, _ = h.Write([]byte(s.Name))
	arr = binary.LittleEndian.AppendUint64(arr, s.Range.Hash())

	for _, typeParam := range s.TypeParams {
		arr = binary.LittleEndian.AppendUint64(arr, (&typeParam).Hash())
	}

	for _, param := range s.Params {
		arr = binary.LittleEndian.AppendUint64(arr, (&param).Hash())
	}

	if s.ReturnType != nil {
		arr = binary.LittleEndian.AppendUint64(arr, s.ReturnType.Hash())
	}

	if s.Body != nil {
		arr = binary.LittleEndian.AppendUint64(arr, s.Body.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// TypeParam represents a type parameter in a function or type declaration.
type TypeParam struct {
	Range
	Name string
	// Constraint is an optional type constraint
	Constraint Type
}

// Hash returns a hash value for the TypeParam, based on its structural characteristics
func (t *TypeParam) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("TypeParam")
	_, _ = h.Write([]byte(t.Name))
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	if t.Constraint != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Constraint.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}
