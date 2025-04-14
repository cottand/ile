package types

type unionOpts struct {
	prov    *typeProvenance
	swapped bool
}

// unionOf corresponds to the `|` operation in the scala mlstruct implementation
func unionOf(this, other SimpleType, opts unionOpts) SimpleType {
	panic("unimplemented")
}

// unionOf corresponds to the `&` operation in the scala mlstruct implementation
func intersectionOf(this, other SimpleType, opts unionOpts) SimpleType {
	panic("unimplemented")
}
