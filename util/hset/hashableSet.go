// Package hset implements a set of hashable elements, JVM style
package hset

import (
	"github.com/benbjohnson/immutable"
	"iter"
)

// HSet is a shallow wrapper around a map
// use immutable.Set if you are not going to be modifying this
// as it is more copy efficient
type HSet[A any] struct {
	hasher immutable.Hasher[A]
	underlying map[uint32]A
}

func Empty[A any](hasher immutable.Hasher[A]) HSet[A] {
	return HSet[A]{
		hasher: hasher,
		underlying: make(map[uint32]A),
	}
}

func New[A any](hasher immutable.Hasher[A], elems ...A) HSet[A] {
	n := Empty(hasher)
	for _, elem := range elems {
		n.Add(elem)
	}
	return n
}

func (s HSet[A]) Add(elems ...A) {
	for _, elem := range elems {
		s.underlying[s.hasher.Hash(elem)] = elem
	}
}

func (s HSet[A]) Remove(elems ...A) {
	for _, elem := range elems {
		delete(s.underlying, s.hasher.Hash(elem))
	}
}

func (s HSet[A]) Contains(elem A) bool {
	_, ok := s.underlying[s.hasher.Hash(elem)]
	return ok
}

func (s HSet[A]) Len() int {
	return len(s.underlying)
}

func (s HSet[A]) All() iter.Seq[A]  {
	return func(yield func(A) bool) {
		for _, elem := range s.underlying {
			if !yield(elem) {
				return
			}
		}
	}
}

//func (s MSet[A]) AsSlice() []A  {
//	slice := make([]A, 0, len(s.underlying))
//	for elem, _ := range s.underlying {
//		slice = append(slice, elem)
//	}
//	return slice
//}
