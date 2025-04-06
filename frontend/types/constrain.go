package types

import "github.com/cottand/ile/frontend/ilerr"

func (ctx *TypeCtx) Constrain(
	lhs, rhs simpleType,
	prov *typeProvenance,
	onErr func(err ilerr.IleError) (terminateEarly bool),
) simpleType {
	panic("TODO implement me")
}
