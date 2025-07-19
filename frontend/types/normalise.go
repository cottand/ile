package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"log/slog"
	"slices"
	"strconv"
)

// normaliseType corresponds to normalizeTypes_! in the scala reference
func (ctx *TypeCtx) normaliseType(st SimpleType, p polarity) (res SimpleType) {
	normaliser := typeNormaliser{
		Logger:    logger.With("section", "inference.normalise"),
		TypeCtx:   ctx,
		processed: set.New[TypeVarID](1),
		DNF:       newOpsDNF(ctx, true),
	}
	defer func() {
		normaliser.Info("normalised successfully", "type", st, "result", res)
	}()
	return normaliser.processType(st, p)
}

type typeNormaliser struct {
	*slog.Logger
	*TypeCtx
	processed set.Collection[TypeVarID]
	// DNF must be an opsDNF that DOES preserveTypeRefs
	DNF *opsDNF
}

func (n typeNormaliser) processTypeVar(t *typeVariable) {
	if inserted := n.processed.Insert(t.id); inserted {
		for i, lb := range t.lowerBounds {
			t.lowerBounds[i] = n.processType(lb, positive)
		}
		for i, up := range t.upperBounds {
			t.upperBounds[i] = n.processType(up, negative)
		}
	}
}

// processType is called go in the Scala reference
func (n typeNormaliser) processType(typ SimpleType, pol polarity) (res SimpleType) {
	defer func() {
		n.Debug("processed type", "type", typ, "polarity", pol, "result", res)
	}()

	if pol != invariant {
		return n.processDNF(n.DNF.mk(typ, pol == positive), pol)
	}
	dnfNegative := n.DNF.mk(typ, false)
	dnsPositive := n.DNF.mk(typ, true)
	return n.TypeCtx.newTypeRange(typeRange{
		lowerBound: n.processDNF(dnfNegative, negative),
		upperBound: n.processDNF(dnsPositive, positive),
	})
}

// processDNF is called helper in the scala reference
func (n typeNormaliser) processDNF(d dnf, pol polarity) (res SimpleType) {
	defer func() {
		n.Debug("processed DNF", "type", d, "polarity", pol, "result", res)
	}()

	// unzip conjunction
	conjNegations, conjs := make([]conjunct, 0), make([]conjunct, 0)
	for _, conj := range d {
		if conj.lhs.isLeftNfTop() && conj.vars.Empty() && !(conj.rhs.isRhsBot() && conj.nvars.Empty()) {
			conjNegations = append(conjNegations, conj)
		} else {
			conjs = append(conjs, conj)
		}
	}

	// The goal here is to normalise things like `... | !A | !B | !C` the same as we would `... | !(A & B & C)`
	// It is fine to call processType because we made sure A, B, C, etc. do not themselves have any negative components.
	var conjNegAsType SimpleType = bottomType
	if len(conjNegations) > 0 {
		// else fold: !toType(neg1) | !toType(neg2) | ...
		var typ SimpleType = topType
		for _, negatedConj := range conjNegations {
			negatedAsType := negateType(negatedConj.toType(), emptyProv)
			typ = intersectionOf(typ, negatedAsType, unionOpts{})
		}
		conjNegAsType = negateType(n.processType(typ, pol.inverse()), emptyProv)
	}

	var conjAsType SimpleType = bottomType
	sortedConjs := slices.SortedFunc(slices.Values(conjs), func(c1, c2 conjunct) int {
		return cmp.Compare(c1.Hash(), c2.Hash())
	})
	for _, conj := range sortedConjs {
		for v := range util.ConcatIter(conj.vars.Items(), conj.nvars.Items()) {
			n.processTypeVar(v)
		}
		typed := conj.toTypeWith(
			// TODO doc for why we are inverting polarity here
			func(nf lhsNF) SimpleType { return n.lhsNFToType(pol.inverse(), nf) },
			func(nf rhsNF) SimpleType { return n.rhsNFToType(pol.inverse(), nf) },
		)
		conjAsType = unionOf(conjAsType, typed, unionOpts{})
	}
	conjAsType = n.factorise(conjAsType)
	return unionOf(conjAsType, conjNegAsType, unionOpts{})
}

// lhsNFToType processes the types in lhsNF before calling lhsNF's toType()
func (n typeNormaliser) lhsNFToType(pol polarity, lhs lhsNF) (ret SimpleType) {
	defer func() {
		n.Debug("processed normal form LHS ToType", "type", lhs, "polarity", pol, "result", ret)
	}()
	switch lhs := lhs.(type) {
	case lhsTop:
		return topType
	case *lhsRefined:
		newTypeRefs := make([]typeRef, 0, len(lhs.typeRefs))
		// traverse type refs as well as each type ref's arguments
		for _, ref := range lhs.typeRefs {
			newTypeRef := typeRef{
				defName:        ref.defName,
				typeArgs:       make([]SimpleType, 0, len(ref.typeArgs)),
				withProvenance: ref.withProvenance,
			}
			for typeRefPol, type_ := range n.typeRefTraverseTypeArguments(ref, pol) {
				newTypeRef.typeArgs = append(newTypeRef.typeArgs, n.processType(type_, typeRefPol))
			}
			newTypeRefs = append(newTypeRefs, newTypeRef)
		}
		var newFn *funcType
		if lhs.fn != nil {
			newFn = &funcType{
				withProvenance: lhs.fn.withProvenance,
				ret:            n.processType(lhs.fn.ret, pol),
			}
			for _, arg := range lhs.fn.args {
				newFn.args = append(newFn.args, n.processType(arg, pol.inverse()))
			}

		}

		var newArray arrayBase
		if lhs.arr != nil {
			switch arr := lhs.arr.(type) {
			case tupleType:
				arity := len(arr.fields)
				// Partition the record fields into component fields (those that start with "_" followed by a number)
				// and regular record fields
				componentFields := make(map[int]fieldType)
				regularFields := make([]recordField, 0)

				// Process record fields if they exist
				if len(lhs.reft.fields) > 0 {
					for _, field := range lhs.reft.fields {
						fieldName := field.name.Name
						if len(fieldName) > 1 && fieldName[0] == '_' {
							// Check if the rest of the name is a number
							indexStr := fieldName[1:]
							if index, err := strconv.Atoi(indexStr); err == nil {
								if index <= arity && index > 0 {
									// This is a component field
									componentFields[index] = field.type_
									continue
								}
							}
						}
						// This is a regular field
						regularFields = append(regularFields, field)
					}
				}

				// Create new tuple components by combining the original tuple types with the corresponding component fields
				newFields := make([]SimpleType, len(arr.fields))
				for i, field := range arr.fields {
					index := i + 1 // 1-based indexing
					if componentField, ok := componentFields[index]; ok {
						// Process the field type with the appropriate polarity
						upperBound := n.processType(componentField.upperBound, pol)
						// Also process lowerBound even though we don't use it directly
						_ = n.processType(componentField.lowerBound, pol.inverse())
						// Combine with the original field
						newFields[i] = intersectionOf(field, upperBound, unionOpts{})
					} else {
						newFields[i] = n.processType(field, pol)
					}
				}

				// Create a new tuple type with the processed components
				newArray = tupleType{
					fields:         newFields,
					withProvenance: arr.withProvenance,
				}

				// Update lhs.reft with the regular fields
				if len(regularFields) > 0 {
					lhs.reft = recordType{
						fields:         regularFields,
						withProvenance: lhs.reft.withProvenance,
					}
				} else {
					lhs.reft = recordType{} // Empty record type
				}
			default:
				n.addFailure(fmt.Sprintf("LHS ND with arr types (in this case %T) not implemented, see normalizeTypes_!()-helper()", lhs.arr), emptyProv)
				return lhs.toType()
			}
		}

		if lhs.base != nil {
			_, isClassTagName := lhs.base.id.(*ir.Var)
			if isClassTagName && !n.isBuiltinType(lhs.base.id.CanonicalSyntax()) {
				// Try to reconstruct a proper class type when a class tag is found,
				// reconstructing the corresponding class type arguments
				// and omitting field refinements that do not refine the reconstructed class type.
				n.addFailure(fmt.Sprintf("found %s, but non builtin types not implemented yet, see normalizeTypes_!()-helper()", lhs.base), emptyProv)
			}
			// if lhs.base is a known builtin OR a type literal expr (like '1' or "foo")
			// we do not need to reconstruct it
		}
		newLhs := lhsRefined{
			base:      lhs.base,
			fn:        newFn,
			arr:       newArray,
			traitTags: lhs.traitTags,
			reft:      lhs.reft, // keep in mind reference copies the record here - we don't yet because it's not implemented but look into why
			typeRefs:  newTypeRefs,
		}
		return newLhs.toType()
	}
	n.addFailure(fmt.Sprintf("unexpected LHS normal form %T for %s", lhs, lhs), emptyProv)
	return errorType()
}

// rhsNFToType processes the types in rhsNF before calling rhsNF's normalForm.toType
func (n typeNormaliser) rhsNFToType(pol polarity, rhs rhsNF) SimpleType {
	switch rhs := rhs.(type) {
	case rhsBot:
		return bottomType
	case *rhsField:
		field := rhs.ty
		newField := fieldType{
			lowerBound:     n.processType(field.lowerBound, pol.inverse()),
			upperBound:     n.processType(field.upperBound, pol),
			withProvenance: field.withProvenance,
		}
		return recordType{
			fields: []recordField{{
				name:  rhs.name,
				type_: newField,
			}},
		}
	case *rhsBases:
		var res SimpleType
		switch rest := rhs.rest.(type) {
		case *rhsField:
			newField := rhsField{
				name: rest.name,
				ty: fieldType{
					lowerBound:     n.processType(rest.ty.lowerBound, pol.inverse()),
					upperBound:     n.processType(rest.ty.upperBound, pol),
					withProvenance: rest.ty.withProvenance,
				},
			}
			res = newField.toType()
		case SimpleType:
			res = n.processType(rest, pol)
		default:
			if rest != nil {
				n.addFailure(fmt.Sprintf("unexpected case for rgsBases.rest %T", rhs.rest), emptyProv)
			}
			res = bottomType
		}
		sortedTypeRefs := slices.SortedFunc(slices.Values(rhs.typeRefs), util.ComparingHashable)
		var mergedTypeRefs SimpleType = bottomType
		for _, typeRef := range sortedTypeRefs {
			processed := n.processType(typeRef, pol)
			mergedTypeRefs = unionOf(mergedTypeRefs, processed, unionOpts{})
		}

		sortedTags := slices.SortedFunc(slices.Values(rhs.tags), util.ComparingHashable)
		for _, tag := range sortedTags {
			processed := n.processType(tag, pol)
			res = unionOf(res, processed, unionOpts{})
		}

		return unionOf(mergedTypeRefs, res, unionOpts{})
	default:
		n.addFailure(fmt.Sprintf("unexpected RHS normal form %T for %s", rhs, rhs), emptyProv)
		return rhs.toType()
	}
}

// TODO write tests for factorise once we can compile
// more complex type unions into Go
func (ctx *TypeCtx) factorise(typ SimpleType) SimpleType {
	// Convert the type to DNF
	dnfOps := newOpsDNF(ctx, true)
	d := dnfOps.mk(typ, true)

	if len(d) <= 1 {
		// If there's only one conjunct or none, no factorization needed
		return typ
	}

	// Find common factors across all conjuncts
	factors := make(map[Factorizable]int)

	// Collect all possible factors from the conjuncts
	for _, c := range d {
		// Check type variables
		for v := range c.vars.Items() {
			factors[v] = factors[v] + 1
		}

		// Check negated type variables
		for v := range c.nvars.Items() {
			negV := negVar{v: v}
			factors[negV] = factors[negV] + 1
		}

		// Check for trait tags in the right-hand side
		if bases, ok := c.rhs.(*rhsBases); ok {
			for _, tag := range bases.tags {
				if tt, ok := tag.(traitTag); ok {
					factors[tt] = factors[tt] + 1
				}
			}
		}

		// Check for class tag in the left-hand side
		if refined, ok := c.lhs.(*lhsRefined); ok && refined.base != nil {
			factors[*refined.base] = factors[*refined.base] + 1
		}
	}

	// Find the most common factor
	var bestFactor Factorizable
	maxCount := 1 // Only consider factors that appear more than once

	for factor, count := range factors {
		if count > maxCount {
			bestFactor = factor
			maxCount = count
		}
	}

	// If no common factor found, return the original type
	if maxCount <= 1 {
		return typ
	}

	// Partition the conjuncts based on whether they contain the factor
	var factored []conjunct
	var rest []conjunct

	for _, c := range d {
		containsFactor := false

		switch f := bestFactor.(type) {
		case *typeVariable:
			containsFactor = c.vars.Contains(f)
		case negVar:
			containsFactor = c.nvars.Contains(f.v)
		case traitTag:
			if bases, ok := c.rhs.(*rhsBases); ok {
				containsFactor = bases.hasTag(f)
			}
		case classTag:
			if refined, ok := c.lhs.(*lhsRefined); ok && refined.base != nil {
				containsFactor = Equal(*refined.base, f)
			}
		}

		if containsFactor {
			factored = append(factored, c)
		} else {
			rest = append(rest, c)
		}
	}

	// Create the factored part: factor & factorizeImpl(factored without the factor)
	var factoredWithoutFactor []conjunct
	for _, c := range factored {
		// Remove the factor from the conjunct
		newConjunct := c

		switch f := bestFactor.(type) {
		case *typeVariable:
			newVars := set.NewTreeSet(compareTypeVars)
			for v := range c.vars.Items() {
				if v != f {
					newVars.Insert(v)
				}
			}
			newConjunct.vars = newVars
		case negVar:
			newNVars := set.NewTreeSet(compareTypeVars)
			for v := range c.nvars.Items() {
				if v != f.v {
					newNVars.Insert(v)
				}
			}
			newConjunct.nvars = newNVars
		case traitTag:
			if bases, ok := c.rhs.(*rhsBases); ok {
				newTags := make([]objectTag, 0, len(bases.tags)-1)
				for _, tag := range bases.tags {
					if tt, ok := tag.(traitTag); !ok || !Equal(tt, f) {
						newTags = append(newTags, tag)
					}
				}
				newConjunct.rhs = &rhsBases{
					tags:     newTags,
					rest:     bases.rest,
					typeRefs: bases.typeRefs,
				}
			}
		case classTag:
			if refined, ok := c.lhs.(*lhsRefined); ok && refined.base != nil {
				newRefined := *refined
				newRefined.base = nil
				newConjunct.lhs = &newRefined
			}
		}

		factoredWithoutFactor = append(factoredWithoutFactor, newConjunct)
	}

	// Recursively factorize the parts without the factor
	factoredDNF := dnf(factoredWithoutFactor)
	factoredType := factoredDNF.toType()
	factoredFactorized := ctx.factorise(factoredType)

	// Convert the factor to a SimpleType
	var factorType SimpleType
	switch f := bestFactor.(type) {
	case *typeVariable:
		factorType = f
	case negVar:
		factorType = negateType(f.v, emptyProv)
	case traitTag:
		factorType = f
	case classTag:
		factorType = f
	}

	// Create the factored part: factor & factorizedWithoutFactor
	factoredPart := intersectionOf(factorType, factoredFactorized, unionOpts{})

	// If there are no rest conjuncts, return just the factored part
	if len(rest) == 0 {
		return factoredPart
	}

	// Recursively factorize the rest part if there are multiple factors
	restDNF := dnf(rest)
	restType := restDNF.toType()
	var restFactorized SimpleType

	if len(factors) > 1 {
		restFactorized = ctx.factorise(restType)
	} else {
		restFactorized = restType
	}

	// Return the union of the factored part and the rest part
	return unionOf(factoredPart, restFactorized, unionOpts{})
}

// Helper type for factorise
type negVar struct {
	v *typeVariable
}

// Factorizable is an interface for types that can be factored out
type Factorizable interface {
	Hash() uint64
}

// Ensure our types implement Factorizable
var (
	_ Factorizable = (*typeVariable)(nil)
	_ Factorizable = negVar{}
	_ Factorizable = traitTag{}
	_ Factorizable = classTag{}
)

// Hash implementation for negVar
func (n negVar) Hash() uint64 {
	return n.v.Hash() * 31
}
