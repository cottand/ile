package ir

type Type interface {
	Node
	typeNode()
}

type TypeLit struct {
	Range
	Package string
	Name    string
}

func (TypeLit) typeNode() {}
