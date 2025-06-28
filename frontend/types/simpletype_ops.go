package types

type unionOpts struct {
	prov    typeProvenance
	swapped bool
}

// unionOf corresponds to the `|` operation in the scala reference
//
// it does not strictly need to do premature optimisation with simplifications (like A | A = A)
// because those will be likely covered in normalisation.
// But we can still take some shortcuts, just in case we might be able to avoid triggering normalisation
// altogether.
//
// Because of this, it is undesirable to do expensive computations here that might dramatically increase complexity,
// such as `isSubtype` being called here. We constantly use them when folding, so we want composed types to be O(1) to
// instantiate
func unionOf(this, that SimpleType, opts unionOpts) SimpleType {
	if Equal(this, topType) {
		return topType
	}
	if Equal(this, bottomType) {
		return that
	}
	if Equal(this, that) {
		return this
	}

	unwrapped := unwrapProvenance(this)

	// A | B where B contains A = B
	if tag, ok := unwrapped.(classTag); ok {
		if otherObject, ok := unwrapProvenance(that).(objectTag); ok && tag.containsParentST(otherObject.Id()) {
			return that
		}
	}

	// ~A | A = top
	if thisAsNeg, thisIsNeg := unwrapped.(negType); thisIsNeg {
		if Equal(thisAsNeg.negated, that) {
			return topType
		}
	}
	if !opts.swapped {
		return unionOf(that, this, unionOpts{prov: opts.prov, swapped: true})
	}
	return unionType{lhs: this, rhs: that, withProvenance: opts.prov.embed()}
}

// intersectionOf corresponds to the `&` operation in the scala reference
func intersectionOf(this, that SimpleType, opts unionOpts) SimpleType {
	if Equal(this, bottomType) {
		return bottomType
	}
	if Equal(this, topType) {
		return that
	}
	if Equal(this, that) {
		return this
	}
	thisRecord, thisIsRecord := this.(recordType)
	if thisIsRecord && len(thisRecord.fields) == 0 {
		return that
	}
	unwrapped := unwrapProvenance(this)

	// A & B where B contains A => A
	if tag, ok := unwrapped.(classTag); ok {
		if otherObject, ok := unwrapProvenance(that).(objectTag); ok && tag.containsParentST(otherObject.Id()) {
			return this
		}
	}

	// ~A & A = bottom
	if thisAsNeg, thisIsNeg := this.(negType); thisIsNeg {
		if Equal(thisAsNeg.negated, that) {
			return bottomType
		}
	}
	if !opts.swapped {
		return intersectionOf(that, this, unionOpts{prov: opts.prov, swapped: true})
	}
	return intersectionType{lhs: this, rhs: that, withProvenance: opts.prov.embed()}
}

func recordUnionOf(left, right []recordField) []recordField {
	rightAsMap := make(map[string]recordField, len(right))
	for _, rf := range right {
		rightAsMap[rf.name.Name] = rf
	}
	res := make([]recordField, 0, len(left)+len(right))
	for _, lf := range left {
		if rf, ok := rightAsMap[lf.name.Name]; ok {
			field := recordField{name: lf.name, type_: lf.type_.union(rf.type_, emptyProv)}
			res = append(res, field)
		} else {
			res = append(res, lf)
		}
	}
	return res
}

