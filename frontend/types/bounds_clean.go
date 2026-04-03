package types

import (
	"log/slog"
	"strings"
)

type cleanBoundsOpts struct {
	// using true avoids instantiating fresh variables when replacing bounds
	// it is unclear why the scala reference sometimes opts in to this
	inPlace bool // default false

	// preserveInvariantVars behaves unlike the scala reference and does not remove invariant bounds
	// (which is more conservative)
	preserveInvariantVars bool // default false
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
	// FIXME: divergence from scala reference
	// MLStruct's getVarsPol checks forall(_ === polarity) - TypeSimplifier.scala:19
	// but Ile's getVarsPolFor assigns single polarity per variable. This may miss mixed-polarity cases.
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
		cbc.renewed[tv.id] = ret
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
		// AND there are non-trivial lower bounds
		pol, exists := cbc.pols[t.id]
		switch {
		case exists && pol == positive && len(t.lowerBounds) > 0:
			var processed SimpleType = bottomType
			for _, lb := range t.lowerBounds {
				processedLB := cbc.process(lb, &boolTypeVar{b: true, tv: t})
				processed = unionOf(processed, processedLB, unionOpts{})
			}
			if !isBottom(processed) {
				renewed.lowerBounds = []SimpleType{processed}
			}
		case len(t.lowerBounds) > 0 && !cbc.preserveInvariantVars:
			renewed.lowerBounds = nil
		case len(t.lowerBounds) > 0 && !cbc.inPlace:
			renewed.lowerBounds = make([]SimpleType, len(t.lowerBounds))
			copy(renewed.lowerBounds, t.lowerBounds)
		}

		// Process upper bounds if all polarities for this variable are negative
		// AND there are non-trivial upper bounds
		switch {
		case exists && pol == negative && len(t.upperBounds) > 0:
			var processed SimpleType = topType
			for _, ub := range t.upperBounds {
				processedUB := cbc.process(ub, &boolTypeVar{b: false, tv: t})
				processed = intersectionOf(processed, processedUB, unionOpts{})
			}
			if !isTop(processed) {
				renewed.upperBounds = []SimpleType{processed}
			}
		case len(t.upperBounds) > 0 && !cbc.preserveInvariantVars:
			renewed.upperBounds = nil
		case len(t.upperBounds) > 0 && !cbc.inPlace:
			renewed.upperBounds = make([]SimpleType, len(t.upperBounds))
			copy(renewed.upperBounds, t.upperBounds)
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
		if cbc.ctx.isBuiltinType(t.defName) {
			return cbc.process(cbc.ctx.expand(t, expandOpts{}), parent)
		}

		return t.doMap(func(child SimpleType) SimpleType {
			return cbc.process(child, nil)
		})
	case recordType:
		mappedFields := make([]recordField, 0, len(t.fields))
		for _, field := range t.fields {
			fnme := field.name.Name
			hashIdx := strings.IndexByte(fnme, '#')
			defaultField := func() recordField {
				return recordField{
					name: field.name,
					type_: fieldType{
						lowerBound:     cbc.process(field.type_.lowerBound, nil),
						upperBound:     cbc.process(field.type_.upperBound, nil),
						withProvenance: field.type_.withProvenance,
					},
				}
			}
			if hashIdx < 0 {
				mappedFields = append(mappedFields, defaultField())
				continue
			}
			prefix := fnme[:hashIdx]
			postfix := fnme[hashIdx+1:]
			td, ok := cbc.ctx.typeDefs[prefix]
			if !ok || td.typeVarVariances == nil {
				mappedFields = append(mappedFields, defaultField())
				continue
			}
			var foundVariance varianceInfo
			found := false
			for _, tpa := range td.typeParamArgs {
				if string(tpa.Fst) == postfix {
					foundVariance = td.typeVarVariances[tpa.Snd.id]
					found = true
					break
				}
			}
			if !found {
				mappedFields = append(mappedFields, defaultField())
				continue
			}
			switch {
			case foundVariance.covariant && foundVariance.contravariant:
				mappedFields = append(mappedFields, recordField{
					name: field.name,
					type_: fieldType{
						lowerBound:     bottomType,
						upperBound:     topType,
						withProvenance: field.type_.withProvenance,
					},
				})
			case foundVariance.covariant:
				mappedFields = append(mappedFields, recordField{
					name: field.name,
					type_: fieldType{
						lowerBound:     bottomType,
						upperBound:     cbc.process(field.type_.upperBound, nil),
						withProvenance: field.type_.withProvenance,
					},
				})
			case foundVariance.contravariant:
				mappedFields = append(mappedFields, recordField{
					name: field.name,
					type_: fieldType{
						lowerBound:     cbc.process(field.type_.lowerBound, nil),
						upperBound:     topType,
						withProvenance: field.type_.withProvenance,
					},
				})
			default:
				mappedFields = append(mappedFields, defaultField())
			}
		}
		return recordType{
			fields:         mappedFields,
			withProvenance: t.withProvenance,
		}

	default:
		// For other types, map over their children with neutral polarity
		return t.doMap(func(child SimpleType) SimpleType {
			return cbc.process(child, nil)
		})
	}
}
