package ast

import (
	"encoding/binary"
	"hash/fnv"
)

// All type nodes implement the Type interface

// TypeName represents a named type (e.g., Int, String).
type TypeName struct {
	Range
	Name string
	// Package is the package qualifier (e.g., "fmt" in fmt.Stringer)
	// Empty string means the type is from the current package
	Package string
}

func (t *TypeName) typeNode() {}

// Hash returns a hash value for the TypeName, based on its structural characteristics
func (t *TypeName) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("TypeName")
	_, _ = h.Write([]byte(t.Name))
	_, _ = h.Write([]byte(t.Package))
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	_, _ = h.Write(arr)
	return h.Sum64()
}

// FuncType represents a function type (e.g., fn(Int, String) -> Bool).
type FuncType struct {
	Range
	ParamTypes []Type
	ReturnType Type
}

func (t *FuncType) typeNode() {}

// Hash returns a hash value for the FuncType, based on its structural characteristics
func (t *FuncType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("FuncType")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	for _, paramType := range t.ParamTypes {
		if paramType != nil {
			arr = binary.LittleEndian.AppendUint64(arr, paramType.Hash())
		}
	}

	if t.ReturnType != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.ReturnType.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// ListType represents a list type (e.g., [Int]).
type ListType struct {
	Range
	ElementType Type
}

func (t *ListType) typeNode() {}

// Hash returns a hash value for the ListType, based on its structural characteristics
func (t *ListType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("ListType")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	if t.ElementType != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.ElementType.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// StructType represents a struct type (e.g., {name: String, age: Int}).
type StructType struct {
	Range
	Fields []StructTypeField
}

func (t *StructType) typeNode() {}

// Hash returns a hash value for the StructType, based on its structural characteristics
func (t *StructType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("StructType")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	for _, field := range t.Fields {
		arr = binary.LittleEndian.AppendUint64(arr, (&field).Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// StructTypeField represents a field in a struct type.
type StructTypeField struct {
	Range
	Name string
	Type Type
}

// Hash returns a hash value for the StructTypeField, based on its structural characteristics
func (f *StructTypeField) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("StructTypeField")
	_, _ = h.Write([]byte(f.Name))
	arr = binary.LittleEndian.AppendUint64(arr, f.Range.Hash())

	if f.Type != nil {
		arr = binary.LittleEndian.AppendUint64(arr, f.Type.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// UnionType represents a union type (e.g., Int | String).
type UnionType struct {
	Range
	Left  Type
	Right Type
}

func (t *UnionType) typeNode() {}

// Hash returns a hash value for the UnionType, based on its structural characteristics
func (t *UnionType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("UnionType")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	if t.Left != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Left.Hash())
	}

	if t.Right != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Right.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// IntersectionType represents an intersection type
type IntersectionType struct {
	Range
	Left  Type
	Right Type
}

func (t *IntersectionType) typeNode() {}

// Hash returns a hash value for the IntersectionType, based on its structural characteristics
func (t *IntersectionType) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("IntersectionType")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	if t.Left != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Left.Hash())
	}

	if t.Right != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Right.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// TypeApplication represents a type application (e.g., List<Int>).
type TypeApplication struct {
	Range
	Base Type
	Args []Type
}

func (t *TypeApplication) typeNode() {}

// Hash returns a hash value for the TypeApplication, based on its structural characteristics
func (t *TypeApplication) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("TypeApplication")
	arr = binary.LittleEndian.AppendUint64(arr, t.Range.Hash())

	if t.Base != nil {
		arr = binary.LittleEndian.AppendUint64(arr, t.Base.Hash())
	}

	for _, arg := range t.Args {
		if arg != nil {
			arr = binary.LittleEndian.AppendUint64(arr, arg.Hash())
		}
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}
