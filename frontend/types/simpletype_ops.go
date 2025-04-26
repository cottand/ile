package types

import "fmt"

type unionOpts struct {
	prov    typeProvenance
	swapped bool
}

// unionOf corresponds to the `|` operation in the scala mlstruct implementation
func unionOf(this, other SimpleType, opts unionOpts) SimpleType {
	if this.Equivalent(topType) {
		return topType
	}
	if this.Equivalent(bottomType) {
		return other
	}
	if !opts.swapped {
		return unionOf(other, this, unionOpts{prov: opts.prov, swapped: true})
	}
	panic(fmt.Sprintf("union not implemented for %T | %T", this, other))
}

// intersectionOf corresponds to the `&` operation in the scala mlstruct implementation
func intersectionOf(this, other SimpleType, opts unionOpts) SimpleType {
	if this.Equivalent(bottomType) {
		return bottomType
	}
	if this.Equivalent(topType) {
		return other
	}
	if !opts.swapped {
		return intersectionOf(other, this, unionOpts{prov: opts.prov, swapped: true})
	}
	panic(fmt.Sprintf("intersection not implemented for %T & %T", this, other))
}
