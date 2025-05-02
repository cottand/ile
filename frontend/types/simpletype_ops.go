package types

import "fmt"

type unionOpts struct {
	prov    typeProvenance
	swapped bool
}

// unionOf corresponds to the `|` operation in the scala mlstruct implementation
func unionOf(this, that SimpleType, opts unionOpts) SimpleType {
	if this.Equivalent(topType) {
		return topType
	}
	if this.Equivalent(bottomType) {
		return that
	}
	if !opts.swapped {
		return unionOf(that, this, unionOpts{prov: opts.prov, swapped: true})
	}
	if thisAsNeg, thisIsNeg := this.(negType); thisIsNeg {
		if thisAsNeg.negated.Equivalent(that) {
			// ~A | A = any
			return topType
		}
	}
	logger.Warn(fmt.Sprintf("union not implemented for %s | %s (returning plain union)", this, that))
	return unionType{lhs: this, rhs: that, withProvenance: opts.prov.embed()}

}

// intersectionOf corresponds to the `&` operation in the scala mlstruct implementation
func intersectionOf(this, that SimpleType, opts unionOpts) SimpleType {
	if this.Equivalent(bottomType) {
		return bottomType
	}
	if this.Equivalent(topType) {
		return that
	}
	if this.Equivalent(that) {
		return this
	}
	thisRecord, thisIsRecord := this.(recordType)
	if thisIsRecord && len(thisRecord.fields) == 0 {
		return that
	}
	if !opts.swapped {
		return intersectionOf(that, this, unionOpts{prov: opts.prov, swapped: true})
	}

	if thisAsNeg, thisIsNeg := this.(negType); thisIsNeg {
		if thisAsNeg.negated.Equivalent(that) {
			// ~A & A = nothing
			return bottomType
		}
	}
	logger.Warn(fmt.Sprintf("intersection not implemented for %s & %s (returning plain intersection)", this, that))
	return intersectionType{lhs: this, rhs: that, withProvenance: opts.prov.embed()}
}
