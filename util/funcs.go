package util

import (
	"github.com/hashicorp/go-set/v3"
	"iter"
	"slices"
)

func SlicesEquivalent[A set.Hash, B, BB set.Hasher[A]](fst []B, snd []BB) bool {
	return slices.EqualFunc(fst, snd, func(e1 B, e2 BB) bool {
		return e1.Hash() == e2.Hash()
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

func SingleIter[A any](elem A) iter.Seq[A] {
	return func(yield func(A) bool) {
		yield(elem)
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

func IterFirstOrPanic[A any](iter iter.Seq[A]) A {
	for elem := range iter {
		return elem
	}
	panic("empty iterator")
}

func MapIter[A, B any](iter iter.Seq[A], f func(A) B) iter.Seq[B] {
	return func(yield func(B) bool) {
		for v := range iter {
			if !yield(f(v)) {
				return
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

func SetFromSeq[V comparable](s iter.Seq[V], size int) *set.Set[V] {
	newSet := set.New[V](size)
	for item := range s {
		newSet.Insert(item)
	}
	return newSet
}

func CopySet[V comparable](s set.Collection[V]) *set.Set[V] {
	return SetFromSeq(s.Items(), s.Size())
}

func CopyHashSet[T set.Hasher[H], H set.Hash](s set.Collection[T]) *set.HashSet[T, H] {
	newSet := set.NewHashSet[T, H](s.Size())
	for item := range s.Items() {
		newSet.Insert(item)
	}
	return newSet
}
