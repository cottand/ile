package types

func unwrapProvenance(t SimpleType) SimpleType {
	if wrapped, isWrapped := t.(wrappingProvType); isWrapped {
		return unwrapProvenance(wrapped.SimpleType)
	}
	return t
}

func (ctx *TypeCtx) typeIsAliasOf(t SimpleType) bool {
	panic("TODO implement me")
}
