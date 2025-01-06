package util


type Copyable[A any] interface {
	Copy() A
}
