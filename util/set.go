package util

import (
	"github.com/benbjohnson/immutable"
	"iter"
)

// MSet is a shallow wrapper around a map
// use immutable.Set if you are not going to be modifying this
// as it is more copy efficient
type MSet[A comparable] struct {
	underlying map[A]struct{}
}

func NewEmptySet[A comparable]() MSet[A] {
	return MSet[A]{
		underlying: make(map[A]struct{}),
	}
}

func NewSetOf[A comparable](elems []A) MSet[A] {
	underlying := make(map[A]struct{}, len(elems))
	for _, elem := range elems {
		underlying[elem] = struct{}{}
	}
	return MSet[A]{
		underlying: underlying,
	}
}

func (s MSet[A]) Add(elems ...A) {
	for _, elem := range elems {
		s.underlying[elem] = struct{}{}
	}
}

func (s MSet[A]) Remove(elems ...A) {
	for _, elem := range elems {
		delete(s.underlying, elem)
	}
}

func (s MSet[A]) Contains(elem A) bool {
	_, ok := s.underlying[elem]
	return ok
}

func (s MSet[A]) Len() int {
	return len(s.underlying)
}

func (s MSet[A]) All() iter.Seq[A]  {
	return func(yield func(A) bool) {
		for elem, _ := range s.underlying {
			if !yield(elem) {
				return
			}
		}
	}
}

func (s MSet[A]) AsSlice() []A  {
	slice := make([]A, 0, len(s.underlying))
	for elem, _ := range s.underlying {
		slice = append(slice, elem)
	}
	return slice
}

func (s MSet[A]) Immutable(hasher immutable.Hasher[A]) immutable.Set[A] {
	return immutable.NewSet(hasher, s.AsSlice()...)
}