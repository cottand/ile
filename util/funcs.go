package util

import (
	"iter"
	"slices"
)

// Equivalenceable allows checking if two elements are equivalent
type Equivalenceable[A any] interface {
	Equivalent(other A) bool
}

func SlicesEquivalent[A any, B Equivalenceable[A]](fst []B, snd []A) bool {
	return slices.EqualFunc(fst, snd, func(e1 B, e2 A) bool {
		return e1.Equivalent(e2)
	})
}

func ConcatIter[A any](iter ...iter.Seq[A]) iter.Seq[A] {
	return func(yield func(A) bool) {
		for _, thisIter := range iter {
			for v := range thisIter {
				if !yield(v) {
					return
				}
			}
		}
	}
}

func ConcatIter2[A, B any](iter ...iter.Seq2[A, B]) iter.Seq2[A, B] {
	return func(yield func(A, B) bool) {
		for _, thisIter := range iter {
			for v, w := range thisIter {
				if !yield(v, w) {
					return
				}
			}
		}
	}
}

func Reverse[A any](slice []A) iter.Seq[A] {
	return func(yield func(A) bool) {
		for i := len(slice) - 1; i >= 0; i-- {
			if !yield(slice[i]) {
				return
			}
		}
	}
}
