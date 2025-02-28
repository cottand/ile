package types

import "github.com/cottand/ile/util"

type constraintSolver struct {
	cache map[util.Pair[simpleType, simpleType]]struct{}
}

// constrain constrains the types to enforce a subtyping relationship
//
//	lhs <: rhs
func (*constraintSolver) constrain(lhs, rhs simpleType) {
	panic("not implemented")
}

