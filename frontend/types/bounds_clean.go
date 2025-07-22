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
			// Return extreme type based on polarity
			if parent.b {
				return extremeType{polarity: true, withProvenance: emptyProv.embed()}
			}
			return extremeType{polarity: false, withProvenance: emptyProv.embed()}
		}

		// Renew the type variable
		var isNew bool
		existing, ok := cbc.renewed[t.id]
		if !ok {
			isNew = true
			existing = cbc.renew(t)
		}

		if isNew {
			// Process lower bounds if all polarities for this variable are positive
			pol, exists := cbc.pols[t.id]
			if exists && pol == positive {
				if len(t.lowerBounds) > 0 {
					var processedLowerBounds []SimpleType
					for _, lb := range t.lowerBounds {
						processedLB := cbc.process(lb, &boolTypeVar{b: true, tv: t})
						// Skip bottom types
						if _, isBot := processedLB.(*extremeType); !isBot || !processedLB.(*extremeType).isTop() {
							processedLowerBounds = append(processedLowerBounds, processedLB)
						}
					}

					// Combine lower bounds with union
					if len(processedLowerBounds) > 0 {
						var result SimpleType = processedLowerBounds[0]
						for _, lb := range processedLowerBounds[1:] {
							result = unionType{lhs: result, rhs: lb, withProvenance: emptyProv.embed()}
						}
						existing.lowerBounds = []SimpleType{result}
					} else {
						existing.lowerBounds = nil
					}
				} else {
					existing.lowerBounds = nil
				}
			} else {
				existing.lowerBounds = nil
			}

			// Process upper bounds if all polarities for this variable are negative
			pol, exists = cbc.pols[t.id]
			if exists && pol == negative {
				if len(t.upperBounds) > 0 {
					var processedUpperBounds []SimpleType
					for _, ub := range t.upperBounds {
						processedUB := cbc.process(ub, &boolTypeVar{b: false, tv: t})
						// Skip top types
						if _, isTop := processedUB.(*extremeType); !isTop || processedUB.(*extremeType).isTop() {
							processedUpperBounds = append(processedUpperBounds, processedUB)
						}
					}

					// Combine upper bounds with intersection
					if len(processedUpperBounds) > 0 {
						var result SimpleType = processedUpperBounds[0]
						for _, ub := range processedUpperBounds[1:] {
							result = &intersectionType{lhs: result, rhs: ub, withProvenance: emptyProv.embed()}
						}
						existing.upperBounds = []SimpleType{result}
					} else {
						existing.upperBounds = nil
					}
				} else {
					existing.upperBounds = nil
				}
			} else {
				existing.upperBounds = nil
			}
		}

		return existing

	case unionType:
		// Process lhs and rhs with the same parent
		lhs := cbc.process(t.lhs, parent)
		rhs := cbc.process(t.rhs, parent)
		return unionType{lhs: lhs, rhs: rhs, withProvenance: t.withProvenance}

	case intersectionType:
		// Process lhs and rhs with the same parent
		lhs := cbc.process(t.lhs, parent)
		rhs := cbc.process(t.rhs, parent)
		return intersectionType{lhs: lhs, rhs: rhs, withProvenance: t.withProvenance}

	case negType:
		// Process the negated type with inverted parent polarity
		var newParent *boolTypeVar
		if parent != nil {
			newParent = &boolTypeVar{b: !parent.b, tv: parent.tv}
		}
		processed := cbc.process(t.negated, newParent)
		return negType{negated: processed, withProvenance: t.withProvenance}

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
			// In the Scala reference, this would expand the type reference and process the expansion
			// For now, we'll just process the type arguments
			processedArgs := make([]SimpleType, len(t.typeArgs))
			for i, arg := range t.typeArgs {
				processedArgs[i] = cbc.process(arg, parent)
			}
			return typeRef{
				defName:        t.defName,
				typeArgs:       processedArgs,
				withProvenance: t.withProvenance,
			}
		}
		return t

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
