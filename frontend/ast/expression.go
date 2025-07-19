package ast

import (
	"encoding/binary"
	"go/token"
	"hash/fnv"
)

// All expression types implement the Expr interface

// Identifier represents a variable or function name.
type Identifier struct {
	Range
	Name string
}

func (e *Identifier) exprNode() {}

// Hash returns a hash value for the Identifier, based on its structural characteristics
func (e *Identifier) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Identifier")
	_, _ = h.Write([]byte(e.Name))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	_, _ = h.Write(arr)
	return h.Sum64()
}

// Literal represents a literal value (integer, string, etc.).
type Literal struct {
	Range
	Value string
	Kind  token.Token // INT, STRING, etc.
}

func (e *Literal) exprNode() {}

// Hash returns a hash value for the Literal, based on its structural characteristics
func (e *Literal) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Literal")
	_, _ = h.Write([]byte(e.Value))
	_, _ = h.Write([]byte(e.Kind.String()))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	_, _ = h.Write(arr)
	return h.Sum64()
}

// BinaryExpr represents a binary operation (a + b, a * b, etc.).
type BinaryExpr struct {
	Range
	Left     Expr
	Operator token.Token // +, -, *, /, etc.
	Right    Expr
}

func (e *BinaryExpr) exprNode() {}

// Hash returns a hash value for the BinaryExpr, based on its structural characteristics
func (e *BinaryExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("BinaryExpr")
	_, _ = h.Write([]byte(e.Operator.String()))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.Left != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Left.Hash())
	}

	if e.Right != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Right.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// UnaryExpr represents a unary operation (!a, -b, etc.).
type UnaryExpr struct {
	Range
	Operator token.Token // !, -, etc.
	Operand  Expr
}

func (e *UnaryExpr) exprNode() {}

// Hash returns a hash value for the UnaryExpr, based on its structural characteristics
func (e *UnaryExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("UnaryExpr")
	_, _ = h.Write([]byte(e.Operator.String()))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.Operand != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Operand.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// CallExpr represents a function call (f(x, y)).
type CallExpr struct {
	Range
	Function Expr
	Args     []Expr
}

func (e *CallExpr) exprNode() {}

// Hash returns a hash value for the CallExpr, based on its structural characteristics
func (e *CallExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("CallExpr")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.Function != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Function.Hash())
	}

	for _, arg := range e.Args {
		if arg != nil {
			arr = binary.LittleEndian.AppendUint64(arr, arg.Hash())
		}
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// FuncLit represents a function literal (fn x, y -> x + y).
type FuncLit struct {
	Range
	Params []Parameter
	Body   Expr
}

func (e *FuncLit) exprNode() {}

// Hash returns a hash value for the FuncLit, based on its structural characteristics
func (e *FuncLit) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("FuncLit")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	for _, param := range e.Params {
		arr = binary.LittleEndian.AppendUint64(arr, (&param).Hash())
	}

	if e.Body != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Body.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// Parameter represents a function parameter.
type Parameter struct {
	Range
	Name    string
	TypeAnn Type // Optional type annotation
}

// Hash returns a hash value for the Parameter, based on its structural characteristics
func (p *Parameter) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Parameter")
	_, _ = h.Write([]byte(p.Name))
	arr = binary.LittleEndian.AppendUint64(arr, p.Range.Hash())

	if p.TypeAnn != nil {
		arr = binary.LittleEndian.AppendUint64(arr, p.TypeAnn.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// SelectExpr represents a field selection (a.b).
type SelectExpr struct {
	Range
	X    Expr
	Sel  string
}

func (e *SelectExpr) exprNode() {}

// Hash returns a hash value for the SelectExpr, based on its structural characteristics
func (e *SelectExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("SelectExpr")
	_, _ = h.Write([]byte(e.Sel))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.X != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.X.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// ListLit represents a list literal ([a, b, c]).
type ListLit struct {
	Range
	Elements []Expr
}

func (e *ListLit) exprNode() {}

// Hash returns a hash value for the ListLit, based on its structural characteristics
func (e *ListLit) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("ListLit")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	for _, elem := range e.Elements {
		if elem != nil {
			arr = binary.LittleEndian.AppendUint64(arr, elem.Hash())
		}
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// StructLit represents a struct literal ({a: 1, b: 2}).
type StructLit struct {
	Range
	Fields []StructField
}

func (e *StructLit) exprNode() {}

// Hash returns a hash value for the StructLit, based on its structural characteristics
func (e *StructLit) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("StructLit")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	for _, field := range e.Fields {
		arr = binary.LittleEndian.AppendUint64(arr, (&field).Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// StructField represents a field in a struct literal.
type StructField struct {
	Range
	Name  string
	Value Expr
}

// Hash returns a hash value for the StructField, based on its structural characteristics
func (f *StructField) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("StructField")
	_, _ = h.Write([]byte(f.Name))
	arr = binary.LittleEndian.AppendUint64(arr, f.Range.Hash())

	if f.Value != nil {
		arr = binary.LittleEndian.AppendUint64(arr, f.Value.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// WhenExpr represents a pattern matching expression.
type WhenExpr struct {
	Range
	Target Expr
	Cases  []WhenCase
}

func (e *WhenExpr) exprNode() {}

// Hash returns a hash value for the WhenExpr, based on its structural characteristics
func (e *WhenExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("WhenExpr")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.Target != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Target.Hash())
	}

	for _, c := range e.Cases {
		arr = binary.LittleEndian.AppendUint64(arr, (&c).Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// WhenCase represents a case in a when expression.
type WhenCase struct {
	Range
	Pattern Type
	Body    Expr
}

// Hash returns a hash value for the WhenCase, based on its structural characteristics
func (c *WhenCase) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("WhenCase")
	arr = binary.LittleEndian.AppendUint64(arr, c.Range.Hash())

	if c.Pattern != nil {
		arr = binary.LittleEndian.AppendUint64(arr, c.Pattern.Hash())
	}

	if c.Body != nil {
		arr = binary.LittleEndian.AppendUint64(arr, c.Body.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// ParenExpr represents a parenthesized expression.
type ParenExpr struct {
	Range
	X Expr
}

func (e *ParenExpr) exprNode() {}

// Hash returns a hash value for the ParenExpr, based on its structural characteristics
func (e *ParenExpr) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("ParenExpr")
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.X != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.X.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// VarDecl represents a variable declaration (val x = expr).
type VarDecl struct {
	Range
	Name    string
	TypeAnn Type // Optional type annotation
	Value   Expr
}

func (e *VarDecl) exprNode() {}
func (e *VarDecl) stmtNode() {}

// Hash returns a hash value for the VarDecl, based on its structural characteristics
func (e *VarDecl) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("VarDecl")
	_, _ = h.Write([]byte(e.Name))
	arr = binary.LittleEndian.AppendUint64(arr, e.Range.Hash())

	if e.TypeAnn != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.TypeAnn.Hash())
	}

	if e.Value != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Value.Hash())
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}
