package types

// opsDNF corresponds to the DNF companion object in the scala reference
// We add operations between dnf and cnf here instead of as methods of those structs so that we can
// store here their constructors (like mk) and the TypeCtx.
type opsDNF struct {
	ctx *TypeCtx
}

func (o *opsDNF) and(left, right dnf) dnf {
	panic("TODO")
}
func (o *opsDNF) or(left, right dnf) dnf {
	panic("TODO")
}
func (o *opsDNF) andElem(left dnf, right conjunct) dnf {
	panic("TODO")
}
func (o *opsDNF) orElem(left dnf, right conjunct) dnf {
	panic("TODO")
}
func (o *opsDNF) makeDeep(t SimpleType, polarity bool, preserveTypeRefs bool) dnf {
	panic("TODO")
}

// opsCNF corresponds to the CNF companion object in the scala reference
type opsCNF struct {
	ctx *TypeCtx
}
