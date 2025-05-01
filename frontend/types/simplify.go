package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/util"
	set "github.com/hashicorp/go-set/v3" // Using v3 as v2 is deprecated
	"slices"
	"strings"
)

// --- Analysis Data Structures ---

type polarVariableKey struct {
	// should always be one of positive, negative, not none
	pol polarity
	tv  TypeVarID
}

// analysis1State takes care of polarity and occurrence Counts
type analysis1State struct {
	occNums           map[polarVariableKey]int
	occursInvariantly *set.Set[TypeVarID]
	analyzed          *set.Set[polarVariableKey] // Tracks (TV -> Polarity) already processed for bounds
}

type analysis2State struct {
	coOccurrences map[polarVariableKey]set.Collection[SimpleType] // Stores atoms co-occurring with a variable at a polarity
	analyzed      set.Collection[simpleTypePolarity]              // Tracks (ST -> Polarity) already processed
	ctx           *TypeCtx
}

type simpleTypePolarity struct {
	st SimpleType
	// should always be one of positive, negative, not none
	pol polarity
}

func (p simpleTypePolarity) Hash() uint64 {
	return 31*p.st.Hash() + 47*uint64(p.pol)
}

// --- Simplification Pipeline ---

type polarity uint8

const (
	negative = polarity(iota)
	positive
	invariant
)

func (p polarity) String() string {
	switch p {
	case negative:
		return "negative"
	case positive:
		return "positive"
	case invariant:
		return "invariant"
	default:
		panic("invalid polarity: " + string(p))
	}
}

func (p polarity) inverse() polarity {
	switch p {
	case negative:
		return positive
	case positive:
		return negative
	default:
		return p
	}
}

func (p simpleTypePolarity) String() string {
	return p.st.String() + "-" + p.pol.String() + ""
}

// Config flags (mirroring Scala simplifyType parameters)
const simplifyRemovePolarVars = true
const simplifyInlineBounds = true // Controls bound inlining for non-rec, non-invariant vars

// simplifyPipeline orchestrates the type simplification process.
// Corresponds to the SimplifyPipeline class in Scala.
func (ctx *TypeCtx) simplifyPipeline(st SimpleType) SimpleType {
	logger.Info("begin simplification for", "simpleType", st, "bounds", boundsString(st))

	// Corresponds to the first simplifyType call in Scala
	cur := ctx.simplifyType(st, positive, simplifyRemovePolarVars, simplifyInlineBounds)
	//logger.Debug("⬤ Type after first simplification:", "type", cur, "bounds", boundsString(cur))

	// TODO: Implement and insert normalization step (normalizeTypes_!) here.
	// logger.Debug("⬤ Normalized:", "type", cur, "bounds", boundsString(cur))

	// TODO: Implement and insert cleanup steps (removeIrrelevantBounds, unskidTypes_!) here.
	// logger.Debug("⬤ Cleaned up:", "type", cur, "bounds", boundsString(cur))
	// logger.Debug("⬤ Unskid:", "type", cur, "bounds", boundsString(cur))

	// TODO: Implement and call simplifyType again (Resim) after normalization.
	// cur = ctx.simplifyType(cur, positive, simplifyRemovePolarVars, simplifyInlineBounds)
	// logger.Debug("⬤ Resim:", "type", cur, "bounds", boundsString(cur))

	// TODO: Implement and insert final factoring step (factorRecursiveTypes_!) here.
	// logger.Debug("⬤ Factored:", "type", cur, "bounds", boundsString(cur))

	return cur
}

// simplifyType performs the core simplification logic: analysis, substitution determination, and transformation.
// Corresponds to the simplifyType method in Scala's TypeSimplifier.
func (ctx *TypeCtx) simplifyType(st SimpleType, pol polarity, removePolarVars bool, inlineBounds bool) SimpleType {
	logger.Debug("simplifyType: starting", "type", st, "polarity", pol)

	// --- Analysis 1: Polarity and Occurrence Counts ---
	analyzer1 := &analysis1State{
		occNums:           make(map[polarVariableKey]int),
		occursInvariantly: set.New[TypeVarID](0),
		analyzed:          set.New[polarVariableKey](0),
	}
	analyzer1.analyze1(st, pol) // Start with the given polarity

	logger.Debug("simplifyType: inv", "analyzer1", analyzer1.occursInvariantly.Slice())
	occNumsStr := &strings.Builder{}
	for k, v := range analyzer1.occNums {
		_, _ = fmt.Fprintf(occNumsStr, "%s%v:%d; ", k.pol, k.tv, v)
	}

	// --- Analysis 2: Polar Co-occurrences ---
	analyzer2 := &analysis2State{
		coOccurrences: make(map[polarVariableKey]set.Collection[SimpleType]),
		analyzed:      set.NewHashSet[simpleTypePolarity](0),
		ctx:           ctx,
	}
	// Analyze both polarities regardless of the initial polarity for co-occurrence
	analyzer2.analyze2(st, positive) // Positive co-occurrences
	analyzer2.analyze2(st, negative) // Negative co-occurrences

	coOccsStr := &strings.Builder{}
	for k, v := range analyzer2.coOccurrences {
		_, _ = fmt.Fprintf(coOccsStr, "%s%v:{", k.pol.String(), k.tv)
		for item := range v.Items() {
			_, _ = fmt.Fprintf(coOccsStr, "%s,", item)
		}
		_, _ = fmt.Fprintf(coOccsStr, "}; ")
	}
	logger.Debug("simplifyType: occurrences " + coOccsStr.String())

	// --- Processing: Determine Substitutions ---
	allVars := getVariables(st)                             // getCached all unique variables
	slices.SortFunc(allVars, func(a, b *typeVariable) int { // Sort for deterministic processing
		if a.id < b.id {
			return -1
		}
		if a.id > b.id {
			return 1
		}
		return 0
	})

	// Substitution map: tv -> replacement (*typeVariable or nil for removal)
	varSubst := make(map[TypeVarID]*typeVariable)
	// Set of variables considered recursive (might change during processing)
	recVars := set.New[TypeVarID](0)
	for _, tv := range allVars {
		if tv.isRecursive() { // Assuming isRecursive exists
			recVars.Insert(tv.id)
		}
	}

	logger.Debug("simplifyType: vars", "allVars", allVars, "recVars", recVars)

	// 1. Remove polar vars occurring once (even if recursive)
	if inlineBounds { // This step is tied to inlining in Scala
		for key, num := range analyzer1.occNums {
			tv := key.tv
			pol := key.pol
			_, hasOtherPolarity := analyzer1.occNums[polarVariableKey{pol: pol.inverse(), tv: tv}]
			if num == 1 && !hasOtherPolarity {
				if _, exists := varSubst[tv]; !exists {
					logger.Debug("simplifyType: [sub] Rule 1 (Polar, Occ=1)", "var", tv, "action", "remove")
					varSubst[tv] = nil // Mark for removal
				}
			}
		}
	}

	// 2. Remove non-recursive polar vars
	if removePolarVars {
		for _, tv := range allVars {
			if recVars.Contains(tv.id) {
				continue // Skip recursive vars for this rule
			}
			if _, exists := varSubst[tv.id]; exists {
				continue // Already marked
			}

			_, hasPos := analyzer1.occNums[polarVariableKey{pol: positive, tv: tv.id}]
			_, hasNeg := analyzer1.occNums[polarVariableKey{pol: negative, tv: tv.id}]

			if hasPos != hasNeg { // If strictly polar
				logger.Debug("simplifyType: [sub] Rule 2 (Non-Rec, Polar)", "var", tv, "action", "remove")
				varSubst[tv.id] = nil // Mark for removal
			}
		}
	}

	// 3. Remove non-recursive vars dominated by another atom/variable
	for _, tv := range allVars {
		if recVars.Contains(tv.id) {
			continue
		}
		if _, exists := varSubst[tv.id]; exists {
			continue
		}

		posCoOccs, hasPosCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: positive, tv: tv.id}]
		negCoOccs, hasNegCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: negative, tv: tv.id}]

		if !hasPosCoOccs || !hasNegCoOccs {
			continue // Cannot be dominated if not present in both polarities
		}

		// Find intersection of co-occurring atoms/vars
		intersection := set.New[SimpleType](0)
		for item := range posCoOccs.Items() {
			if negCoOccs.Contains(item) {
				intersection.Insert(item)
			}
		}

		for dominator := range intersection.Items() {
			if _, exists := varSubst[tv.id]; exists {
				break // Stop if already marked
			}

			switch dom := dominator.(type) {
			case classTag, traitTag, typeRef: // Dominated by an atom
				logger.Debug("simplifyType: [sub] Rule 3a (Dominated by Atom)", "var", tv, "action", "remove", "dominator", dom)
				varSubst[tv.id] = nil
				continue // Check for other dominators? Scala seems to stop. Let's stop.
			case *typeVariable: // Dominated by another variable 'dom'
				if dom == tv {
					continue // Skip self
				}
				if _, domMarked := varSubst[dom.id]; domMarked {
					continue // Skip if dominator is already marked
				}
				logger.Debug("simplifyType: [sub] Rule 3b (Dominated by Var)", "var", tv, "action", "remove", "dominator", dom)
				varSubst[tv.id] = nil
				break // Stop after finding one dominator
			}
		}
	}

	// 4. Unify equivalent variables based on co-occurrence
	// Iterate multiple times? Scala does this implicitly via iteration order? Let's try one pass.
	for _, tv := range allVars {
		if _, exists := varSubst[tv.id]; exists {
			continue
		}

		for _, pol := range []polarity{positive, negative} {
			if _, exists := varSubst[tv.id]; exists {
				break // Stop if tv got substituted in the inner loop
			}

			tvCoOccs, hasTvCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: pol, tv: tv.id}]
			if !hasTvCoOccs {
				continue
			}

			// Iterate through potential candidates 'w'
			for item := range tvCoOccs.Items() {
				w, ok := item.(*typeVariable)
				if !ok || w == tv {
					continue // Skip non-vars and self
				}
				if _, wExists := varSubst[w.id]; wExists {
					continue // Skip if w is already substituted
				}
				// In Scala: Don't merge if name hint would be lost (v.nameHint.nonEmpty || w.nameHint.isEmpty)
				// We'll merge w into tv if tv has a hint or w doesn't.
				if !(tv.nameHint != "" || w.nameHint == "") {
					continue
				}

				// Check if tv is in w's co-occurrences for the same polarity
				wCoOccs, hasWCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: pol, tv: w.id}]
				if hasWCoOccs && wCoOccs.Contains(tv) {
					// Potential unification: w into tv
					logger.Debug("simplifyType: [sub] Rule 4 (Unify Candidate)", "from", w, "to", tv)
					varSubst[w.id] = tv // Mark w to be replaced by tv

					// Merge bounds (w's bounds into tv's)
					tv.lowerBounds = append(tv.lowerBounds, w.lowerBounds...)
					tv.upperBounds = append(tv.upperBounds, w.upperBounds...)
					// TODO: Should bounds be simplified/deduplicated here?

					// If w was recursive, tv becomes recursive
					if recVars.Contains(w.id) {
						recVars.Insert(tv.id)
					}
					recVars.Remove(w.id) // w is gone

					// Merge co-occurrences for the *other* polarity (!pol)
					wOtherCoOccs, hasWOtherCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: pol.inverse(), tv: w.id}]
					if hasWOtherCoOccs {
						tvOtherCoOccs, hasTvOtherCoOccs := analyzer2.coOccurrences[polarVariableKey{pol: pol.inverse(), tv: tv.id}]
						if hasTvOtherCoOccs {
							// Intersect tv's other co-occs with w's other co-occs
							intersection := tvOtherCoOccs.Intersect(wOtherCoOccs)
							analyzer2.coOccurrences[polarVariableKey{pol: pol.inverse(), tv: tv.id}] = intersection
						} else {
							// tv didn't have other co-occs, so it inherits w's
							analyzer2.coOccurrences[polarVariableKey{pol: pol.inverse(), tv: tv.id}] = wOtherCoOccs
						}
					}
					// Remove w from co-occurrence map (might not be strictly necessary)
					delete(analyzer2.coOccurrences, polarVariableKey{pol: pol, tv: w.id})
					delete(analyzer2.coOccurrences, polarVariableKey{pol: pol.inverse(), tv: w.id})

					break // Stop checking other candidates for tv for this polarity
				}
			} // End candidate loop
		} // End polarity loop
	} // End allVars loop

	substStr := &strings.Builder{}
	for k, v := range varSubst {
		if v == nil {
			fmt.Fprintf(substStr, "α%v->∅; ", k)
		} else {
			fmt.Fprintf(substStr, "α%v->%v; ", k, v)
		}
	}

	// --- Transformation ---
	transformer := &transformerState{
		varSubst:          varSubst,
		recVars:           recVars,
		occursInvariantly: analyzer1.occursInvariantly,
		renewals:          make(map[*typeVariable]*typeVariable),
		inlineBounds:      inlineBounds,
		ctx:               ctx, // Needed for freshVar
	}

	simplifiedType := transformer.transform(st, pol, nil) // Use initial polarity for transformation

	logger.Debug("simplify (simplifyType): done", "result", simplifiedType, "resultBounds", boundsString(st), "subst", substStr.String(), "recVars", recVars.Slice())

	return simplifiedType
}

// --- Analysis 1 Implementation ---

func (a1 *analysis1State) analyze1(st SimpleType, pol polarity) {
	// Use reflection to prevent infinite loops with recursive types handled via pointers
	st = unwrapProvenance(st) // Operate on the underlying type

	switch ty := st.(type) {
	case *typeVariable:
		key := polarVariableKey{tv: ty.id} // Polarity will be set below

		// Determine effective polarity for bound processing
		boundPol := pol
		if pol == invariant {
			a1.occursInvariantly.Insert(ty.id)
			boundPol = positive // Process bounds as if positive for analysis
		}

		// Count occurrence
		if pol != invariant {
			key.pol = pol
			a1.occNums[key]++
		} else {
			// Invariant counts as both positive and negative
			a1.occNums[polarVariableKey{pol: positive, tv: ty.id}]++
			a1.occNums[polarVariableKey{pol: negative, tv: ty.id}]++
		}

		// Recurse on bounds only once per (variable, polarity)
		// Process lower bounds if not strictly negative
		if boundPol != negative {
			key.pol = positive // Key for lower bounds analysis
			if a1.analyzed.Insert(key) {
				for _, lb := range ty.lowerBounds {
					a1.analyze1(lb, positive) // Lower bounds are positive
				}
			}
		}
		// Process upper bounds if not strictly positive
		if boundPol != positive {
			key.pol = negative // Key for upper bounds analysis
			if a1.analyzed.Insert(key) {
				for _, ub := range ty.upperBounds {
					a1.analyze1(ub, negative) // Upper bounds are negative
				}
			}
		}

	case funcType:
		// Args are contravariant, Return is covariant
		for _, arg := range ty.args {
			a1.analyze1(arg, pol.inverse())
		}
		a1.analyze1(ty.ret, pol)
	case unionType:
		// Polarity propagates directly
		a1.analyze1(ty.lhs, pol)
		a1.analyze1(ty.rhs, pol)
	case intersectionType:
		// Polarity propagates directly
		a1.analyze1(ty.lhs, pol)
		a1.analyze1(ty.rhs, pol)
	case negType:
		// Polarity flips
		a1.analyze1(ty.negated, pol.inverse())
	case typeRange:
		// Invariant position for bounds
		a1.analyze1(ty.lowerBound, invariant)
		a1.analyze1(ty.upperBound, invariant)
	case recordType:
		// Field types: upper bound is covariant, lower bound (if exists) is contravariant
		for _, field := range ty.fields {
			// Treat mutable fields (non-nil lb) as invariant for simplicity here?
			// Scala's Traverser.InvariantFields handles this. Let's approximate:
			if field.Snd.lowerBound != nil && !isBottom(field.Snd.lowerBound) {
				// If mutable, treat both bounds as invariant
				a1.analyze1(field.Snd.lowerBound, invariant)
				a1.analyze1(field.Snd.upperBound, invariant)
			} else {
				// If immutable, only upper bound matters, treat as covariant
				a1.analyze1(field.Snd.upperBound, pol)
			}
		}
	case tupleType:
		for _, field := range ty.fields {
			a1.analyze1(field, pol)
		}
	case namedTupleType:
		for _, field := range ty.fields {
			a1.analyze1(field.Snd, pol)
		}
	case arrayType:
		// Array elements are often treated as invariant if mutable, covariant if not.
		// Let's assume invariant for safety, like Scala's Traverser.InvariantFields might.
		a1.analyze1(ty.innerT, invariant)
	case typeRef:
		panic("unclear where this is in the reference implementation")
		// Need variance info for type parameters
		//variances, ok := a1.ctx.GetTypeDefinitionVariances(ty.defName) // Assumes this exists
		//if !ok {
		//	fmt.Println("Warning: Unknown type definition for variance analysis:", ty.defName)
		//	// Treat all args as invariant if variance is unknown
		//	for _, targ := range ty.typeArgs {
		//		a1.analyze1(targ, invariant)
		//	}
		//	return
		//}
		//if len(variances) != len(ty.typeArgs) {
		//	fmt.Println("Warning: Variance/TypeArg mismatch for:", ty.defName)
		//	// Treat all args as invariant
		//	for _, targ := range ty.typeArgs {
		//		a1.analyze1(targ, invariant)
		//	}
		//	return
		//}
		//for i, targ := range ty.typeArgs {
		//	argPol := combinePolarity(pol, variances[i])
		//	a1.analyze1(targ, argPol)
		//}

	case extremeType, classTag, traitTag:
		// No variables here
	case *PolymorphicType:
		// Should not happen if called on simplified types, but analyze body if it does
		a1.analyze1(ty.Body, pol)
	default:
		panic(fmt.Sprintf("analyze1: unhandled type %T", st))
	}
}

// combinePolarity calculates the resulting polarity based on outer polarity and variance.
func combinePolarity(outerPol polarity, variance Variance) polarity {
	// If outer is invariant or variance is invariant, result is invariant
	if outerPol == invariant || variance == Invariant {
		return invariant
	}
	// Outer polarity is defined (positive or negative)
	if variance == Covariant {
		return outerPol // Covariant: polarity stays the same
	}
	// Contravariant: polarity flips
	return outerPol.inverse()
}

// --- Analysis 2 Implementation ---

func (a2 *analysis2State) analyze2(st SimpleType, pol polarity) {
	a2.analyzeImpl(unwrapProvenance(st), pol)
}

// TODO might need to replace all usages with analyze2 here not analyzeImpl
func (a2 *analysis2State) analyzeImpl(st SimpleType, pol polarity) {
	key := simpleTypePolarity{st: st, pol: pol}
	if a2.analyzed.Contains(key) {
		return
	}
	a2.analyzed.Insert(key)

	switch ty := st.(type) {
	case *typeVariable:
		a2.processVarOcc(ty, pol, set.NewHashSet[SimpleType](0)) // Start with empty co-occs for this path
	case unionType:
		if pol == positive { // Union in positive position: process children with same polarity
			a2.analyzeImpl(ty.lhs, pol)
			a2.analyzeImpl(ty.rhs, pol)
		} else { // Union in negative position: process children as separate branches
			a2.processVarOcc(ty, pol, set.NewHashSet[SimpleType](0))
		}
	case intersectionType:
		if pol == negative { // Intersection in negative position: process children with same polarity
			a2.analyzeImpl(ty.lhs, pol)
			a2.analyzeImpl(ty.rhs, pol)
		} else { // Intersection in positive position: process children as separate branches
			a2.processVarOcc(ty, pol, set.NewHashSet[SimpleType](0))
		}
	case funcType:
		for _, arg := range ty.args {
			a2.analyzeImpl(arg, pol.inverse()) // Args are contravariant
		}
		a2.analyzeImpl(ty.ret, pol) // Return is covariant
	case negType:
		a2.analyzeImpl(ty.negated, pol.inverse()) // Polarity flips
	case typeRange:
		// Treat bounds as invariant for co-occurrence analysis? Scala seems to ignore ranges here.
		// Let's skip for now, similar to Scala's `analyze2`.
		break
	case recordType:
		for _, field := range ty.fields {
			if field.Snd.lowerBound != nil && !isBottom(field.Snd.lowerBound) {
				// Mutable: analyze both bounds with flipped polarity for lb
				a2.analyzeImpl(field.Snd.lowerBound, pol.inverse())
				a2.analyzeImpl(field.Snd.upperBound, pol)
			} else {
				// Immutable: analyze upper bound covariantly
				a2.analyzeImpl(field.Snd.upperBound, pol)
			}
		}
	case tupleType:
		for _, field := range ty.fields {
			a2.analyzeImpl(field, pol)
		}
	case namedTupleType:
		for _, field := range ty.fields {
			a2.analyzeImpl(field.Snd, pol)
		}
	case arrayType:
		// Treat inner type covariantly for analysis? Or invariant? Let's try covariant.
		a2.analyzeImpl(ty.innerT, pol)
	case typeRef:
		ty.forEachTypeArg(a2.ctx, pol, func(pol polarity, argType SimpleType) {
			if pol != negative {
				a2.analyzeImpl(argType, positive)
			}
			if pol != positive {
				a2.analyzeImpl(argType, negative)
			}
		})
	case extremeType, classTag, traitTag:
		// Atoms, no recursion needed
		break
	case *PolymorphicType:
		a2.analyzeImpl(ty.Body, pol)
	default:
		panic(fmt.Sprintf("analyze2: unhandled type %T", st))
	}
}

// processVarOcc finds all variables reachable from tv following the given polarity,
// accumulating the atomic types encountered along the way.
//
// pol can only be positive or negative, not invariant
//
// currentCoOccs stores hashes of types
func (a2 *analysis2State) processVarOcc(st SimpleType, pol polarity, currentCoOccs set.Collection[SimpleType]) {
	if pol == invariant {
		panic("processVarOcc: illegal argument: invariant polarity")
	}
	st = unwrapProvenance(st)

	switch ty := st.(type) {
	case *typeVariable:
		if currentCoOccs.Contains(ty) { // Cycle detected
			return
		}
		currentCoOccs.Insert(ty) // Add self to current path co-occs

		// Update global co-occurrences for this variable
		key := polarVariableKey{pol: pol, tv: ty.id}
		existingCoOccs, found := a2.coOccurrences[key]
		if found {
			// Intersect existing with current path's co-occs
			intersection := existingCoOccs.Intersect(currentCoOccs)
			a2.coOccurrences[key] = intersection
		} else {
			// First time seeing this var at this polarity, store current co-occs
			a2.coOccurrences[key] = util.CopyHashSet(currentCoOccs)
		}

		// Recurse on bounds
		bounds := ty.lowerBounds
		if pol == negative {
			bounds = ty.upperBounds
		}
		for _, b := range bounds {
			a2.processVarOcc(b, pol, util.CopyHashSet(currentCoOccs)) // Pass a copy for each branch
		}

	case unionType:
		if pol == positive { // Explore both branches
			a2.processVarOcc(ty.lhs, pol, util.CopyHashSet(currentCoOccs))
			a2.processVarOcc(ty.rhs, pol, util.CopyHashSet(currentCoOccs))
		} else { // Treat as atom? Or analyze children? Let's analyze children.
			a2.analyzeImpl(ty.lhs, pol)
			a2.analyzeImpl(ty.rhs, pol)
		}
	case intersectionType:
		if pol == negative { // Explore both branches
			a2.processVarOcc(ty.lhs, pol, util.CopyHashSet(currentCoOccs))
			a2.processVarOcc(ty.rhs, pol, util.CopyHashSet(currentCoOccs))
		} else { // Treat as atom? Or analyze children? Let's analyze children.
			a2.analyzeImpl(ty.lhs, pol)
			a2.analyzeImpl(ty.rhs, pol)
		}
	case classTag, traitTag, typeRef:
		// Found an atomic type, add it to co-occurrences but don't recurse further through it here.
		currentCoOccs.Insert(ty)
		// Also analyze its children separately if it's a typeRef
		if tr, ok := ty.(typeRef); ok {
			a2.analyzeImpl(tr, pol)
		}
	case extremeType:
		// Ignore Top/Bottom for co-occurrences
	default:
		// Other types encountered during traversal (like functions, records)
		// should be analyzed separately by analyzeImpl.
		a2.analyzeImpl(ty, pol)
	}
}

// --- Transformation Implementation ---

type transformerState struct {
	varSubst          map[TypeVarID]*typeVariable
	recVars           set.Collection[TypeVarID]
	occursInvariantly *set.Set[TypeVarID]
	renewals          map[*typeVariable]*typeVariable // Map from old TV to renewed fresh TV
	inlineBounds      bool
	ctx               *TypeCtx // For creating fresh variables
}

// transform applies the substitutions and simplifications.
func (ts *transformerState) transform(st SimpleType, pol polarity, parent *typeVariable) (ret SimpleType) {
	defer func() {
		logger.Debug("simplify: transformed", "type", st, "polarity", pol, "result", ret)
	}()
	st = unwrapProvenance(st)

	switch ty := st.(type) {
	case *typeVariable:
		// Handle recursive parent check
		if parent != nil && ty.id == parent.id {
			if pol != negative { // Positive or invariant parent
				return bottomType
			}
			return topType // Negative parent
		}

		// Apply substitution if exists
		if replacement, exists := ts.varSubst[ty.id]; exists {
			if replacement == nil { // Remove variable -> inline bounds
				boundsToInline := ty.lowerBounds
				if pol == negative { // Negative polarity
					boundsToInline = ty.upperBounds
				}
				mergedBound := mergeBounds(boundsToInline, pol == positive)
				logger.Debug("simplify: transform: Inlining bounds for removed var", "var", ty, "mergedBounds", mergedBound, "polarity", pol.String())
				// Need to pass parent variable to recursive call to handle cycles during inlining
				return ts.transform(mergedBound, pol, ty) // Pass 'ty' as parent
			}
			logger.Debug("simplify: transform: Substituting var", "from", ty, "to", replacement)
			// Replace with another variable, recurse on the replacement
			return ts.transform(replacement, pol, parent)
		}

		// No substitution, check if we need to renew or inline bounds
		isRec := ts.recVars.Contains(ty.id)
		isInv := ts.occursInvariantly.Contains(ty.id)

		// Check if we should inline bounds (non-rec, non-invariant, config enabled)
		shouldInline := ts.inlineBounds && !isRec && !isInv && pol != invariant

		// getCached or create the renewed variable
		renewedVar, wasDefined := ts.renewals[ty]
		if !wasDefined {
			// Create a fresh variable, preserving level and name hint
			// Use a temporary provenance; final provenance comes from usage context
			renewedVar = ts.ctx.fresher.newTypeVariable(ty.level_, emptyProv, ty.nameHint, nil, nil)
			ts.renewals[ty] = renewedVar
			logger.Debug("simplify: transform: renewed", "from", ty, "to", renewedVar)

			// Set bounds on the renewed variable *after* creating it to handle recursion
			// Pass the *renewedVar* as the parent for bound transformation
			// Use temporary slices to avoid modifying renewedVar while iterating
			var newLowerBounds, newUpperBounds []SimpleType
			for _, lb := range ty.lowerBounds {
				newLowerBounds = append(newLowerBounds, ts.transform(lb, positive, renewedVar))
			}
			for _, ub := range ty.upperBounds {
				newUpperBounds = append(newUpperBounds, ts.transform(ub, negative, renewedVar))
			}
			renewedVar.lowerBounds = newLowerBounds
			renewedVar.upperBounds = newUpperBounds

			logger.Debug("simplify: transform: set bounds for", "var", renewedVar, "LB", renewedVar.lowerBounds, "UB", renewedVar.upperBounds)
		}

		if shouldInline { // pol != invariant here
			// Inline bounds and include the renewed variable itself
			logger.Debug("simplify: transform: inlining non-rec bounds for", "var", ty, "renewed", renewedVar)
			var mergedTransform SimpleType
			var boundsToInline []SimpleType
			if pol == positive {
				boundsToInline = ty.lowerBounds
			} else {
				boundsToInline = ty.upperBounds
			}
			mergedTransform = ts.transform(mergeBounds(boundsToInline, pol == positive), pol, renewedVar)
			if pol == positive {
				// Positive polarity: MergedLowerBound | RenewedVar
				return unionOf(mergedTransform, renewedVar, unionOpts{})
			} else {
				// Negative polarity: MergedUpperBound & RenewedVar
				return intersectionOf(mergedTransform, renewedVar, unionOpts{})
			}
		} else {
			// Keep the variable (recursive, invariant, or inlining disabled)
			return renewedVar
		}

	// --- Recursive cases for other types ---
	case funcType:
		newArgs := make([]SimpleType, len(ty.args))
		for i, arg := range ty.args {
			newArgs[i] = ts.transform(arg, pol.inverse(), parent) // Contravariant args
		}
		newRet := ts.transform(ty.ret, pol, parent) // Covariant return
		return funcType{args: newArgs, ret: newRet, withProvenance: ty.withProvenance}
	case unionType:
		lhs := ts.transform(ty.lhs, pol, parent)
		rhs := ts.transform(ty.rhs, pol, parent)
		return unionOf(lhs, rhs, unionOpts{prov: ty.prov()}) // Use unionOf for potential simplification
	case intersectionType:
		lhs := ts.transform(ty.lhs, pol, parent)
		rhs := ts.transform(ty.rhs, pol, parent)
		return intersectionOf(lhs, rhs, unionOpts{prov: ty.prov()}) // Use intersectionOf
	case negType:
		negated := ts.transform(ty.negated, pol.inverse(), parent) // Flip polarity
		return negateType(negated, ty.prov())                      // Use helper for potential simplification
	case typeRange:
		// Treat bounds as invariant during transformation? Or follow polarity?
		// Scala seems to follow polarity.
		if pol == positive || pol == invariant { // Positive or invariant
			newUb := ts.transform(ty.upperBound, positive, parent)
			// This case was unreachable, fixed logic:
			if pol == invariant { // If invariant, transform both bounds
				newLb := ts.transform(ty.lowerBound, negative, parent)
				return ts.ctx.makeTypeRange(newLb, newUb, ty.prov()) // Reconstruct range
			}
			// If strictly positive, becomes upper bound
			return newUb
		} else { // Negative
			newLb := ts.transform(ty.lowerBound, negative, parent)
			return newLb // Negative: becomes lower bound
		}
	case recordType:
		panic("record type not implemented")
	//	newFields := make([]util.Pair[ast.Var, fieldType], len(ty.fields))
	//	for i, field := range ty.fields {
	//		newFty := ts.transformFieldType(field.Snd, pol, parent)
	//		newFields[i] = util.Pair[ast.Var, fieldType]{Fst: field.Fst, Snd: newFty}
	//	}
	//	Re-sort fields after transformation? makeRecordType does this.
	//	return makeRecordType(newFields, &ty.provenance)
	case tupleType:
		newFields := make([]SimpleType, len(ty.fields))
		for i, field := range ty.fields {
			newFields[i] = ts.transform(field, pol, parent)
		}
		return tupleType{fields: newFields, withProvenance: ty.withProvenance}
	case namedTupleType:
		newFields := make([]util.Pair[ast.Var, SimpleType], len(ty.fields))
		for i, field := range ty.fields {
			newFields[i] = util.Pair[ast.Var, SimpleType]{
				Fst: field.Fst,
				Snd: ts.transform(field.Snd, pol, parent),
			}
		}
		return namedTupleType{fields: newFields, withProvenance: ty.withProvenance}
	case arrayType:
		// Assuming invariant for transformation for safety
		newInner := ts.transform(ty.innerT, invariant, parent)
		return arrayType{innerT: newInner, withProvenance: ty.withProvenance}
	case typeRef:

		newTargs := make([]SimpleType, 0, len(ty.typeArgs))

		ty.forEachTypeArg(ts.ctx, pol, func(p polarity, argType SimpleType) {
			newTargs = append(newTargs, ts.transform(argType, p, nil))
		})

		return typeRef{defName: ty.defName, typeArgs: newTargs, withProvenance: ty.withProvenance}
	case extremeType, classTag, traitTag:
		return st // Atoms are returned as is
	case *PolymorphicType:
		// Should not happen, but transform body if it does
		newBody := ts.transform(ty.Body, pol, parent)
		return &PolymorphicType{Body: newBody, _level: ty._level, withProvenance: ty.withProvenance}
	default:
		panic(fmt.Sprintf("simplify: transform: unhandled type %T", st))
	}
}

// transformFieldType transforms the bounds of a fieldType.
func (ts *transformerState) transformFieldType(ft fieldType, pol polarity, parent *typeVariable) fieldType {
	// If field represents mutable state, treat bounds invariantly?
	// Scala's FieldType case seems complex. Let's try a simpler approach:
	// Lower bound is contravariant relative to outer polarity.
	// Upper bound is covariant relative to outer polarity.
	newLb := ts.transform(ft.lowerBound, pol.inverse(), parent)
	newUb := ts.transform(ft.upperBound, pol, parent)
	// Reconstruct, potentially simplifying if lb == ub?
	// For now, just reconstruct.
	return fieldType{lowerBound: newLb, upperBound: newUb, withProvenance: ft.withProvenance}
}

// mergeBounds combines bounds with | (for lower) or & (for upper).
// it is called mergeTransform in the scala reference.
func mergeBounds(bounds []SimpleType, lower bool) SimpleType {
	if len(bounds) == 0 {
		if lower {
			return bottomType
		}
		return topType
	}
	current := bounds[0]
	for i := 1; i < len(bounds); i++ {
		if lower {
			current = unionOf(current, bounds[i], unionOpts{})
		} else {
			current = intersectionOf(current, bounds[i], unionOpts{})
		}
	}
	return current
}

// isRecursive checks if a type variable refers to itself in its bounds.
// TODO: Implement this properly by traversing bounds and checking for `tv.id`.
// This is a placeholder.
func (tv *typeVariable) isRecursive() bool {
	// Placeholder implementation: Needs graph traversal
	visited := *set.New[TypeVarID](0)
	var check func(SimpleType) bool
	check = func(st SimpleType) bool {
		st = unwrapProvenance(st)
		switch ty := st.(type) {
		case *typeVariable:
			if ty.id == tv.id {
				return true // Found self
			}
			if visited.Contains(ty.id) {
				return false // Already checked this var
			}
			visited.Insert(ty.id)
			for _, b := range ty.lowerBounds {
				if check(b) {
					return true
				}
			}
			for _, b := range ty.upperBounds {
				if check(b) {
					return true
				}
			}
			return false
		case funcType:
			for _, arg := range ty.args {
				if check(arg) {
					return true
				}
			}
			return check(ty.ret)
		case unionType:
			return check(ty.lhs) || check(ty.rhs)
		case intersectionType:
			return check(ty.lhs) || check(ty.rhs)
		case negType:
			return check(ty.negated)
		case typeRange:
			return check(ty.lowerBound) || check(ty.upperBound)
		case recordType:
			for _, f := range ty.fields {
				if check(f.Snd.lowerBound) || check(f.Snd.upperBound) {
					return true
				}
			}
			return false
		case tupleType:
			for _, f := range ty.fields {
				if check(f) {
					return true
				}
			}
			return false
		case namedTupleType:
			for _, f := range ty.fields {
				if check(f.Snd) {
					return true
				}
			}
			return false
		case arrayType:
			return check(ty.innerT)
		case typeRef:
			for _, ta := range ty.typeArgs {
				if check(ta) {
					return true
				}
			}
			return false
		case extremeType, classTag, traitTag, *PolymorphicType:
			return false
		default:
			panic(fmt.Sprintf("isRecursive check: unhandled type %T", st))
		}
	}

	// Check both lower and upper bounds of the initial variable
	for _, b := range tv.lowerBounds {
		if check(b) {
			return true
		}
	}
	for _, b := range tv.upperBounds {
		if check(b) {
			return true
		}
	}

	return false
}

func negateType(negated SimpleType, prov typeProvenance) SimpleType {
	if negated.Equivalent(topType) {
		return bottomType
	}
	if negated.Equivalent(bottomType) {
		return topType
	}
	// Basic simplification placeholder
	if nn, ok := negated.(negType); ok {
		return nn.negated // ~(~A) = A
	}
	return negType{negated: negated, withProvenance: withProvenance{prov}}
}
