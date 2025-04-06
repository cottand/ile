package types

func unwrapProvenance(t simpleType) simpleType {
	if wrapped, isWrapped := t.(wrappingProvType); isWrapped {
		return unwrapProvenance(wrapped.simpleType)
	}
	return t
}

func (ctx *TypeCtx) typeIsAliasOf(t simpleType) bool {
	panic("TODO implement me")
}
