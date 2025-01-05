package util

type Stack[A any] struct {
	items []A
}

func (s *Stack[A]) Push(v A) {
	s.items = append(s.items, v)
}

func (s *Stack[A]) Pop() (ret A, ok bool) {
	if len(s.items) <= 0 {
		return ret, false
	}
	lastIndex := len(s.items) - 1
	defer func() {
		s.items = s.items[:lastIndex]
	}()
	return s.items[len(s.items)-1], true
}

func (s *Stack[A]) PopAll() []A {
	defer func() {
		s.items = make([]A, 0)
	}()
	return s.items
}
