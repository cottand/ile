package util

import (
	"github.com/benbjohnson/immutable"
	"iter"
)

type Pair[A, B any] struct {
	Fst A
	Snd B
}

func NewPair[A, B any](fst A, snd B) Pair[A, B] {
	return Pair[A, B]{
		Fst: fst,
		Snd: snd,
	}
}

func SetIterator[T any](set immutable.Set[T]) iter.Seq[T] {
	return func(yield func(T) bool) {
		iterator := set.Iterator()
		for v, ok := iterator.Next(); ok; {
			if iterator.Done() {
				return
			}

			if !yield(v) {
				return
			}
		}
	}
}
