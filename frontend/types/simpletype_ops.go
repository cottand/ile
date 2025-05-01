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
	if thisAsNeg, thisIsNeg := this.(negType); thisIsNeg {
		if thisAsNeg.negated.Equivalent(other) {
			// ~A | A = any
			return topType
		}
	}
	logger.Warn(fmt.Sprintf("union not implemented for %s | %s (returning plain union)", this, other))
	return unionType{lhs: this, rhs: other, withProvenance: opts.prov.embed()}

}

// intersectionOf corresponds to the `&` operation in the scala mlstruct implementation
func intersectionOf(this, other SimpleType, opts unionOpts) SimpleType {
	if this.Equivalent(bottomType) {
		return bottomType
	}
	if this.Equivalent(topType) {
		return other
	}
	if this.Equivalent(other) {
		return this
	}
	thisRecord, thisIsRecord := this.(recordType)
	if thisIsRecord && len(thisRecord.fields) == 0 {
		return other
	}
	if !opts.swapped {
		return intersectionOf(other, this, unionOpts{prov: opts.prov, swapped: true})
	}

	if thisAsNeg, thisIsNeg := this.(negType); thisIsNeg {
		if thisAsNeg.negated.Equivalent(other) {
			// ~A & A = nothing
			return bottomType
		}
	}
	logger.Warn(fmt.Sprintf("intersection not implemented for %s & %s (returning plain intersection)", this, other))
	return intersectionType{lhs: this, rhs: other, withProvenance: opts.prov.embed()}
}
