package types

import (
	"fmt"
	"log/slog"
)

type cleanBoundsOpts struct {
	inPlace bool // default false
}

// cleanBounds is called removeIrrelevantBounds in the scala reference
func (ctx *TypeCtx) cleanBounds(typ SimpleType, opts cleanBoundsOpts) SimpleType {
	// Initialize the context
	cbCtx := cleanBoundsCtx{
		cleanBoundsOpts: opts,
		ctx:             ctx,
		Logger:          ctx.logger.With("section", "simplify.cleanBounds"),
		renewed:         make(map[TypeVarID]*typeVariable),
		pols:            make(map[TypeVarID]polarity),
	}

	// Collect polarities for all type variables in the type
	for tv, pol := range ctx.getVarsPolFor(typ, positive) {
		cbCtx.pols[tv.id] = pol
	}

	cbCtx.Debug("polarities collected", "pols", cbCtx.pols)

	return cbCtx.process(typ, nil)
}

type cleanBoundsCtx struct {
	cleanBoundsOpts
	ctx *TypeCtx
	*slog.Logger

	renewed map[TypeVarID]*typeVariable
	pols    map[TypeVarID]polarity
}

func (cbc *cleanBoundsCtx) renew(tv *typeVariable) (ret *typeVariable) {
	existing, ok := cbc.renewed[tv.id]
	if ok {
		return existing
	}

	defer func() {
		cbc.renewed[ret.id] = ret
	}()

	if cbc.inPlace {
		return tv
	}
	fresh := cbc.ctx.fresher.newTypeVariable(tv.level(), emptyProv, tv.nameHint, nil, nil)
	cbc.Debug("renewed", "from", tv, "to", fresh)
	return fresh
}

type boolTypeVar struct {
	b  bool
	tv *typeVariable
}

func (cbc *cleanBoundsCtx) process(typ SimpleType, parent *boolTypeVar) SimpleType {
	switch t := typ.(type) {
	case *typeVariable:
		// Check if this variable is the parent
		if parent != nil && parent.tv.id == t.id {
			return extremeType{polarity: parent.b, withProvenance: emptyProv.embed()}
		}

		// Renew the type variable
		renewed, ok := cbc.renewed[t.id]
		if ok {
			return renewed
		}
		renewed = cbc.renew(t)

		// Process lower bounds if all polarities for this variable are positive
		pol, exists := cbc.pols[t.id]
		if exists && pol == positive {
			var processed SimpleType = bottomType
			for _, lb := range t.lowerBounds {
				processedLB := cbc.process(lb, &boolTypeVar{b: true, tv: t})
				processed = unionOf(processed, processedLB, unionOpts{})
			}
			if !isBottom(processed) {
				renewed.lowerBounds = []SimpleType{processed}
			}
		}

		// Process upper bounds if all polarities for this variable are negative
		pol, exists = cbc.pols[t.id]
		if exists && pol == negative {
			var processed SimpleType = topType
			for _, ub := range t.upperBounds {
				processedUB := cbc.process(ub, &boolTypeVar{b: false, tv: t})
				processed = intersectionOf(processed, processedUB, unionOpts{})
			}
			if !isTop(processed) {
				renewed.upperBounds = []SimpleType{processed}
			}
		}

		return renewed

	case unionType:
		// Process lhs and rhs with the same parent
		lhs := cbc.process(t.lhs, parent)
		rhs := cbc.process(t.rhs, parent)
		return unionOf(lhs, rhs, unionOpts{})

	case intersectionType:
		// Process lhs and rhs with the same parent
		lhs := cbc.process(t.lhs, parent)
		rhs := cbc.process(t.rhs, parent)
		return intersectionOf(lhs, rhs, unionOpts{})

	case negType:
		// Process the negated type with inverted parent polarity
		var newParent *boolTypeVar
		if parent != nil {
			newParent = &boolTypeVar{b: !parent.b, tv: parent.tv}
		}
		processed := cbc.process(t.negated, newParent)
		return negateType(processed, t.provenance)

	case wrappingProvType:
		if cbc.inPlace {
			underlying := cbc.process(t.SimpleType, parent)
			return wrappingProvType{SimpleType: underlying, proxyProvenance: t.proxyProvenance}
		}
		// If not in place, just process the underlying type
		return cbc.process(t.SimpleType, parent)

	case typeRef:
		// If it's a builtin type, expand it and process the expansion
		if cbc.ctx.isBuiltinType(t.defName) {
			return cbc.process(cbc.ctx.expand(t, expandOpts{}), parent)
		} else {
			return t.doMap(func(child SimpleType) SimpleType {
				return cbc.process(child, nil)
			})
		}
	case recordType:
		cbc.ctx.addFailure(fmt.Sprintf("cleanBounds: record types not implemented yet: %v", t), t.prov())
		return t

	default:
		// For other types, map over their children with neutral polarity
		return t.doMap(func(child SimpleType) SimpleType {
			return cbc.process(child, nil)
		})
	}
}
