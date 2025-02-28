package types

type unionOpts struct {
	prov    *typeProvenance
	swapped bool
}

// unionOf corresponds to the `|` operation in the scala mlstruct implementation
func unionOf(this, other simpleType, opts unionOpts) simpleType {
	panic("unimplemented")
}
