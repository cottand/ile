package ir

type Type interface {
	Node
	Equals(Type) bool
	typeNode()
}

type TypeLit struct {
	Range
	Package string
	Name    string
}

func (TypeLit) typeNode() {}

func (t TypeLit) Equals(other Type) bool {
	sameType, ok := other.(TypeLit)
	return ok && sameType.Name == t.Name && sameType.Package == t.Package
}

