package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set/v3"
	"log/slog"
	"slices"
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
			n.addFailure("LHS ND with arr types not implemented, see normalizeTypes_!()-helper()", emptyProv)
			return lhs.toType()
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

func (ctx *TypeCtx) factorise(typ SimpleType) SimpleType {
	// we don't add an actual failure with addFailure because this case is hit for every single type
	logger.Error("factorise not implemented")
	return typ
}
