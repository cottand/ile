package ir

import "fmt"

type Type interface {
	Node
	fmt.Stringer
	CanBinaryOp(Type) bool
	typeNode()
}

type TypeLit struct {
	Range
	Package string
	Name    string
}

func (TypeLit) typeNode() {}

func (t TypeLit) CanBinaryOp(other Type) bool {
	sameType, ok := other.(TypeLit)
	return ok && sameType.Name == t.Name && sameType.Package == t.Package
}
func (t TypeLit) String() string {
	if t.Package != "" {
		return fmt.Sprintf("%s.%s", t.Package, t.Name)
	}
	return t.Name
}
