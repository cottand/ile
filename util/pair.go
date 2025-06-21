package util

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

func (p Pair[A, B]) Split() (A, B) {
	return p.Fst, p.Snd
}
