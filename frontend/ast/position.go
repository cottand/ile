package ast

import (
	"encoding/binary"
	"fmt"
	"go/token"
	"hash/fnv"
)

// Positioner allows finding the location in the original source file.
type Positioner interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// Range represents a range of positions in the source code.
type Range struct {
	PosStart token.Pos
	PosEnd   token.Pos
}

// Hash returns a hash value for the Range
func (r Range) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte{}
	arr = binary.LittleEndian.AppendUint64(arr, uint64(r.PosStart))
	arr = binary.LittleEndian.AppendUint64(arr, uint64(r.PosEnd))
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Pos returns the starting position of the range.
func (r Range) Pos() token.Pos { return r.PosStart }

// End returns the ending position of the range.
func (r Range) End() token.Pos { return r.PosEnd }

// String returns a string representation of the range.
func (r Range) String() string {
	if r.PosStart == r.PosEnd {
		return fmt.Sprintf("%v", r.PosStart)
	}
	return fmt.Sprintf("%v-%v", r.PosStart, r.PosEnd)
}

// RangeBetween creates a Range between two Positioners.
func RangeBetween(fst, snd Positioner) Range {
	return Range{fst.Pos(), snd.End()}
}

// RangeOf creates a Range from a Positioner.
func RangeOf(expr Positioner) Range {
	if expr == nil {
		return Range{}
	}
	if asRange, ok := expr.(*Range); ok {
		return *asRange
	}
	if asRange, ok := expr.(Range); ok {
		return asRange
	}
	return Range{expr.Pos(), expr.End()}
}
