package types

import (
	"cmp"
	"fmt"
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
		typed := conj.toTypeWith(n.lhsNFToType, n.rhsNFToType)
		conjAsType = unionOf(conjAsType, typed, unionOpts{})
	}
	conjAsType = n.factorise(conjAsType)
	return unionOf(conjAsType, conjNegAsType, unionOpts{})
}

func (n typeNormaliser) lhsNFToType(lhs lhsNF) SimpleType {
	switch lhs := lhs.(type) {
	case lhsTop:
		return topType
	case *lhsRefined:
		if len(lhs.typeRefs) > 0 {
			n.addFailure("LHS NF with type refs not implemented, see normalizeTypes_!()-helper()", emptyProv)
			return errorType()
		}
		if lhs.fn != nil {
			//lhs.fn = &funcType{
			//	args:           lhs.fn.args,
			//	ret:            nil,
			//	withProvenance: withProvenance{},
			//}
		}

	}
	n.addFailure(fmt.Sprintf("unexpected LHS normal form %T for %s", lhs, lhs), emptyProv)
	return lhs.toType()
}
func (n typeNormaliser) rhsNFToType(rhs rhsNF) SimpleType {
	switch rhs.(type) {
	case rhsBot:
		return bottomType
	case *rhsBases:
	case *rhsField:
	}
	n.addFailure(fmt.Sprintf("unexpected RHS normal form %T for %s", rhs, rhs), emptyProv)
	return rhs.toType()
}

func (ctx *TypeCtx) factorise(typ SimpleType) SimpleType {
	ctx.addFailure("factorise not implemented", typ.prov())
	return typ
}
