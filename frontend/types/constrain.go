package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/frontend/ir"
	"github.com/cottand/ile/util"
	set "github.com/hashicorp/go-set/v3"
	"go/token"
	"log/slog"
	"reflect"
	"slices"
)

// constraintPair holds a pair of types being constrained.
type constraintPair struct {
	lhs SimpleType
	rhs SimpleType
}

func (p *constraintPair) Hash() uint64 {
	return 31*p.lhs.Hash() ^ p.rhs.Hash()
}

// constraintContext tracks the chain of constraints for error reporting.
type constraintContext struct {
	lhsChain []SimpleType
	rhsChain []SimpleType
	hash     uint64
}

func (c *constraintContext) Hash() uint64 {
	if c.hash == 0 {
		c.hash = 33533
		for _, lhs := range c.lhsChain {
			c.hash = 31*c.hash ^ lhs.Hash()
		}
		for _, rhs := range c.rhsChain {
			c.hash = 31*c.hash ^ rhs.Hash()
		}

	}
	return c.hash
}

// constraintSolver holds the state for a single constrain call.
type constraintSolver struct {
	ctx   *TypeCtx                                       // Reference to the main typing context
	prov  typeProvenance                                 // Provenance of the top-level constraint
	onErr func(err ilerr.IleError) (terminateEarly bool) // Error callback
	level level                                          // Current polymorphism level during solving
	*slog.Logger

	cache          *set.HashSet[*constraintPair, uint64]
	extrusionCache map[polarVariableKey]*typeVariable // Cache for extrude: (original_var, polarity) -> extruded_var
	fuel           int                                // Fuel to prevent infinite loops
	depth          int                                // Current recursion depth
	stack          []constraintPair                   // Stack for tracking recursion depth and path

	// TODO: Implement Shadows for cycle detection
	// shadows shadowsState

	// TODO: Implement ExtrCtx for stashing constraints during extrusion
	// extrCtx map[TypeVarID][]*stashedConstraint

	// Counters for stats (optional)
	constrainCalls int
	annoyingCalls  int
}

const (
	defaultStartingFuel = 10000
	defaultDepthLimit   = 250
)

// newConstraintSolver initializes a solver instance.
func (ctx *TypeCtx) newConstraintSolver(
	prov typeProvenance,
	onErr func(err ilerr.IleError) (terminateEarly bool),
) *constraintSolver {
	return &constraintSolver{
		ctx:            ctx,
		prov:           prov,
		onErr:          onErr,
		level:          ctx.level, // Start at the context's current level
		cache:          set.NewHashSet[*constraintPair, uint64](0),
		extrusionCache: make(map[polarVariableKey]*typeVariable), // Initialize extrusion cache
		fuel:           defaultStartingFuel,
		depth:          0,
		stack:          make([]constraintPair, 0, defaultDepthLimit),
		Logger:         ctx.logger.With("section", "inference.constrain"),
		// Initialize shadows, extrCtx etc. if implemented
	}
}

// --- Helper methods for the solver ---

func (cs *constraintSolver) consumeFuel(currentLhs, currentRhs SimpleType) bool {
	cs.fuel--
	cs.depth++
	lhsProvDesc := ""
	if currentLhs.prov() != emptyProv {
		lhsProvDesc = " (" + currentLhs.prov().desc + ")"
	}
	rhsProvDesc := ""
	if currentRhs.prov() != emptyProv {
		rhsProvDesc = " (" + currentRhs.prov().desc + ")"
	}
	if cs.depth > defaultDepthLimit {
		// Simplified error reporting
		return cs.onErr(ilerr.New(ilerr.NewTypeMismatch{
			Positioner: cs.prov.Range,
			Actual:     fmt.Sprintf("%s%s", currentLhs, lhsProvDesc),
			Expected:   fmt.Sprintf("%s%s", currentRhs, rhsProvDesc),
			Reason:     "exceeded max depth limit",
		}))
	}
	if cs.fuel <= 0 {
		return cs.onErr(ilerr.New(ilerr.NewTypeMismatch{
			Positioner: cs.prov.Range,
			Actual:     fmt.Sprintf("%s%s", currentLhs, lhsProvDesc),
			Expected:   fmt.Sprintf("%s%s", currentRhs, rhsProvDesc),
			Reason:     "ran out of fuel",
		}))
	}
	return false // Continue
}

func (cs *constraintSolver) reportError(failureMsg string) (terminateEarly bool) {
	if len(cs.stack) == 0 {
		// there should be at least one pair in the stack if we hit an error
		cs.ctx.addFailure("failed to produce compile error for: "+failureMsg, emptyProv)
		return true
	}
	stackPop := cs.stack[len(cs.stack)-1]
	lhs, rhs := stackPop.lhs, stackPop.rhs

	// Get the first non-empty provenance from the left-hand side chain
	lhsProv := lhs.prov()
	for _, pair := range cs.stack {
		if pair.lhs.prov() != emptyProv && !pair.lhs.prov().IsOrigin() {
			lhsProv = pair.lhs.prov()
			break
		}
	}

	// Get the first non-empty provenance from the right-hand side chain
	rhsProv := rhs.prov()
	for _, pair := range slices.Backward(cs.stack) {
		if pair.rhs.prov() != emptyProv && !pair.rhs.prov().IsOrigin() {
			rhsProv = pair.rhs.prov()
			break
		}
	}

	// (a bit arbitrarily) find the last provenance from the right hand side
	var rhsProv2 typeProvenance
	provenanceHint := ""
	var provenanceHintPos ir.ExternalPositioner
	for _, pair := range slices.Backward(cs.stack) {
		if pair.rhs.prov() != emptyProv && !pair.rhs.prov().IsOrigin() {
			rhsProv2 = pair.rhs.prov()
			provenanceHint = fmt.Sprintf(" constraint arises from %s", rhsProv2.desc)
			if rhsProv2.Range != (ir.Range{}) {
				provenanceHintPos = rhsProv2
			}
			break
		}
	}

	// Use the most relevant Range - often the top-level one
	pos := cs.prov.Range
	if lhsProv.Pos() != token.NoPos {
		pos = lhsProv.Range // Or maybe lhs? Needs refinement based on Scala logic
	}

	if lhsAsTag, ok := lhs.(objectTag); ok {
		if rhsAsTag, ok := rhs.(objectTag); ok {
			failureMsg = fmt.Sprintf("%s is not a subtype of %s", lhsAsTag.Id().CanonicalSyntax(), rhsAsTag.Id().CanonicalSyntax())
		}
	}
	lhsProvDesc := lhsProv.Describe()
	if lhsProvDesc != "" {
		lhsProvDesc = " (" + lhsProvDesc + ")"
	}
	rhsProvDesc := rhsProv.Describe()
	if rhsProvDesc != "" {
		rhsProvDesc = " (" + rhsProvDesc + ")"
	}

	// Find the tightest relevant failure in the LHS chain (similar to Scala's tighestRelevantFailure)
	var flowHint string
	var flowHintPos ir.ExternalPositioner
	for _, pair := range cs.stack {
		l := pair.lhs
		// Skip if it's the same as the main error location or has no location
		if l.prov() == lhsProv || l.prov() == emptyProv || l.prov().Range == (ir.Range{}) {
			continue
		}

		// Check if this provenance is relevant (has a location that's different from the main error)
		if l.prov().Pos() != token.NoPos && l.prov().Pos() != lhsProv.Pos() {
			flowHint = fmt.Sprintf("but it flows into %s with expected type %s",
				l.prov().desc,
				ir.TypeString(cs.ctx.expandSimpleType(rhs, true)))
			flowHintPos = l.prov()
			break
		}
	}

	seenPositions := make(map[ir.Range]bool, 1)

	lhsStr := ir.TypeString(cs.ctx.expandSimpleType(lhs, true))
	rhsStr := ir.TypeString(cs.ctx.expandSimpleType(rhs, true))
	mismatchErr := ilerr.NewTypeMismatch{
		Positioner:        pos,
		Actual:            fmt.Sprintf("%s%s", lhsStr, lhsProvDesc),
		Expected:          fmt.Sprintf("%s%s", rhsStr, rhsProvDesc),
		Reason:            failureMsg,
		ProvenanceHint:    provenanceHint,
		ProvenanceHintPos: provenanceHintPos,
		FlowHint:          flowHint,
		FlowHintPos:       flowHintPos,
	}

	// Add the main error types first
	if lhs.prov() != emptyProv && lhs.prov().Range != (ir.Range{}) {
		mismatchErr.ActualStackHints = append(mismatchErr.ActualStackHints, ilerr.TypeStackHint{
			Description: lhs.prov().desc,
			Type:        ir.TypeString(cs.ctx.expandSimpleType(lhs, true)),
			Positioner:  lhs.prov(),
		})
		seenPositions[lhs.prov().Range] = true
	}

	// Always add the expected type, even if it doesn't have a position
	mismatchErr.ExpectedStackHints = append(mismatchErr.ExpectedStackHints, ilerr.TypeStackHint{
		Description: rhsProvDesc,
		Type:        ir.TypeString(cs.ctx.expandSimpleType(rhs, true)),
		Positioner:  rhs.prov(),
	})
	if rhs.prov() != emptyProv && rhs.prov().Pos() != token.NoPos {
		seenPositions[rhs.prov().Range] = true
	}

	// Add hints from RHS chain
	for _, pair := range cs.stack {
		r := pair.rhs
		if r.prov() == emptyProv || r.prov().Range == (ir.Range{}) {
			continue
		}

		// Skip duplicates
		if seenPositions[r.prov().Range] {
			continue
		}
		seenPositions[r.prov().Range] = true

		mismatchErr.ExpectedStackHints = append(mismatchErr.ExpectedStackHints, ilerr.TypeStackHint{
			Description: r.prov().desc,
			Type:        ir.TypeString(cs.ctx.expandSimpleType(r, true)),
			Positioner:  r.prov(),
		})
	}
	// Add hints from LHS chain
	for _, pair := range cs.stack {
		l := pair.lhs
		if l.prov() == emptyProv || l.prov().IsOrigin() || l.prov().Range == (ir.Range{}) {
			continue
		}

		// Skip duplicates
		if seenPositions[l.prov().Range] {
			continue
		}
		seenPositions[l.prov().Range] = true

		mismatchErr.ActualStackHints = append(mismatchErr.ActualStackHints, ilerr.TypeStackHint{
			Description: l.prov().desc,
			Type:        ir.TypeString(cs.ctx.expandSimpleType(l, true)),
			Positioner:  l.prov(),
		})

	}

	err := ilerr.New(mismatchErr)
	return cs.onErr(err)
}

// push pushes a constraint onto the stack for depth tracking.
func (cs *constraintSolver) push(lhs, rhs SimpleType) {
	if cs.stack != nil { // Avoid allocation if stack tracking is disabled
		cs.stack = append(cs.stack, constraintPair{lhs, rhs})
	}
}

// pop removes the last constraint from the stack.
func (cs *constraintSolver) pop() {
	if cs.stack != nil && len(cs.stack) > 0 {
		cs.stack = cs.stack[:len(cs.stack)-1]
		cs.depth--
	}
}

// withSubLevel creates a new solver context for a nested polymorphism level.
// This is a simplified version. The Scala version manages ExtrCtx and unstashing.
func (cs *constraintSolver) withSubLevel(action func(subSolver *constraintSolver) bool) bool {
	subSolver := &constraintSolver{
		ctx:            cs.ctx,            // Share the main context initially
		prov:           cs.prov,           // Inherit provenance
		onErr:          cs.onErr,          // Share error handler
		level:          cs.level + 1,      // Increment level
		cache:          cs.cache,          // Share cache (or create a sub-cache?)
		extrusionCache: cs.extrusionCache, // Share extrusion cache
		fuel:           cs.fuel,           // Pass remaining fuel
		depth:          cs.depth,          // Inherit depth
		stack:          cs.stack,          // Share stack
		constrainCalls: cs.constrainCalls,
		annoyingCalls:  cs.annoyingCalls,
		// TODO: Handle shadows and ExtrCtx properly for sub-levels
	}
	terminate := action(subSolver)
	// Update shared state back from subSolver if necessary (fuel, stats)
	cs.fuel = subSolver.fuel
	cs.constrainCalls = subSolver.constrainCalls
	cs.annoyingCalls = subSolver.annoyingCalls
	// TODO: Handle unstashing from subSolver.extrCtx if implemented
	return terminate
}

// makeProxy wraps a type with provenance information from the constraint chain.
// Simplified version.
func makeProxy(ty SimpleType, prov typeProvenance) SimpleType {
	if prov.Range.Pos() == token.NoPos {
		return ty // Don't wrap with empty provenance
	}
	// In Go, we might need a dedicated ProxyType or use wrappingProvType
	return wrappingProvType{
		SimpleType:      ty,
		proxyProvenance: prov,
	}
}

// addUpperBound adds rhs as an upper bound to the type variable tv.
func (cs *constraintSolver) addUpperBound(tv *typeVariable, rhs SimpleType) bool {
	cs.Debug("constrain: adding upper bound", "bound", rhs, "var", tv)
	// the reference implementation uses foldLeft so we concatenate and iterate in reverse
	newBound := rhs
	for _, pair := range cs.stack {
		c := pair.rhs
		if c.prov() != emptyProv {
			newBound = makeProxy(newBound, c.prov())
		}
	}
	for _, pair := range slices.Backward(cs.stack) {
		c := pair.lhs
		if c.prov() != emptyProv {
			newBound = makeProxy(newBound, c.prov())
		}
	}

	tv.upperBounds = append(tv.upperBounds, newBound)

	// Propagate constraints: L <: new_rhs for all L in lowerBounds
	for _, lb := range tv.lowerBounds {
		if cs.rec(lb, rhs, true, nil) {
			return true // Terminate early if propagation fails
		}
	}
	return false
}

// addLowerBound adds lhs as a lower bound to the type variable tv.
func (cs *constraintSolver) addLowerBound(tv *typeVariable, lhs SimpleType) bool {
	cs.Debug("constrain: adding lower bound", "bound", lhs, "tv", tv)
	newBound := lhs
	for _, pair := range cs.stack {
		c := pair.lhs
		if c.prov() != emptyProv {
			newBound = makeProxy(newBound, c.prov())
		}
	}
	for _, pair := range slices.Backward(cs.stack) {
		c := pair.rhs
		if c.prov() != emptyProv {
			newBound = makeProxy(newBound, c.prov())
		}
	}

	tv.lowerBounds = append(tv.lowerBounds, newBound)

	// Propagate constraints: new_lhs <: U for all U in upperBounds
	for _, ub := range tv.upperBounds {
		if cs.rec(lhs, ub, true, nil) {
			return true // Terminate early if propagation fails
		}
	}
	return false
}

// constrain enforces a subtyping relationship `lhs` <: `rhs`.
// It returns true if constraint solving should terminate early due to an error.
func (ctx *TypeCtx) constrain(
	lhs, rhs SimpleType,
	prov typeProvenance,
	onErr func(err ilerr.IleError) (terminateEarly bool),
) bool {
	solver := ctx.newConstraintSolver(prov, onErr)

	solver.Debug("constrain: begin for", "lhs", lhs, "rhs", rhs)

	initialShadows := shadowsState{} // Assuming shadowsState is defined

	return solver.rec(lhs, rhs, true, &initialShadows)
}

// rec is the main recursive function for constraining.
// It handles fuel, depth, stack, context propagation, and caching.
// Returns true if constraint solving should terminate early.
func (cs *constraintSolver) rec(lhs, rhs SimpleType, sameLevel bool, shadows *shadowsState) bool {
	cs.constrainCalls++
	_ = constraintPair{lhs, rhs}
	cs.push(lhs, rhs)
	defer cs.pop() // Ensure stack is popped on return

	if cs.consumeFuel(lhs, rhs) {
		return true // Terminate due to fuel/depth
	}

	// TODO: Update shadows state
	nextShadows := shadows // Simplified - needs proper update logic
	if !sameLevel {
		// Reset current shadows when level changes
		// nextShadows = shadows.resetCurrent()
	}

	return cs.recImpl(lhs, rhs, nextShadows)
}

func isErrorType(ty SimpleType) bool {
	return Equal(ty, errorTypeInstance)
}

// recImpl contains the core subtyping logic based on type structure.
// Returns true if constraint solving should terminate early.
func (cs *constraintSolver) recImpl(lhs, rhs SimpleType, shadows *shadowsState) bool {
	cs.Debug(fmt.Sprintf("constrain %s <: %s", lhs, rhs), "level", cs.level, "lhs", lhs, "rhs", rhs, "lhsType", reflect.TypeOf(lhs), "rhsType", reflect.TypeOf(rhs), "fuel", cs.fuel)

	// 1. Basic Equality Check (more robust check needed for recursive types)
	if cs.ctx.TypesEquivalent(lhs, rhs) {
		return false // Success
	}

	// 2. Cache Check
	pair := &constraintPair{lhs, rhs}
	if cs.cache.Contains(pair) {
		return false // Success (already processed)
	}
	// TODO: Implement proper cycle detection using shadows here.
	// if shadows.detectCycle(lhs, rhs) { ... reportError ... return true }

	// Add to cache *after* cycle check
	cs.cache.Insert(pair)
	// TODO: Update shadows state here.

	// 3. unwrap provenance wrappers so that we do this checking on the underlying types
	//lhs, rhs = unwrapProvenance(lhs), unwrapProvenance(rhs)
	// 4. Handle specific type combinations (using if statements with type assertions)

	if lhsExtreme, ok := lhs.(extremeType); ok && lhsExtreme.polarity {
		return false
	}
	if rhsExtreme, ok := rhs.(extremeType); ok && rhsExtreme.isTop() {
		return false
	}
	if leftProxy, ok := lhs.(wrappingProvType); ok {
		return cs.rec(leftProxy.SimpleType, rhs, true, shadows)
	}
	if rightProxy, ok := rhs.(wrappingProvType); ok {
		return cs.rec(lhs, rightProxy.SimpleType, true, shadows)
	}

	if rhs, ok := rhs.(recordType); ok && len(rhs.fields) == 0 {
		return false
	}
	if lhsRange, ok := lhs.(typeRange); ok {
		// L..U <: R => U <: R
		return cs.rec(lhsRange.upperBound, rhs, true, shadows)
	}
	if rhsRange, ok := rhs.(typeRange); ok {
		// R <: L..U => U <: L
		return cs.rec(lhs, rhsRange.lowerBound, true, shadows)
	}
	lhsNeg, okLhsNeg := lhs.(negType)
	if okLhsNeg {
		if r, ok := rhs.(negType); ok {
			// ~L <: ~R  =>  R <: L
			return cs.rec(r.negated, lhsNeg.negated, true, shadows)
		}
	}

	lhsFn, okLhsFn := lhs.(funcType)
	rhsIsErr := isErrorType(rhs)
	if okLhsFn {
		if rhsFn, ok := rhs.(funcType); ok {
			// (L1, L2) -> LR <: (R1, R2) -> RR   =>
			// R1 <: L1, LR <: LR
			return cs.constrainFuncFunc(lhsFn, rhsFn, shadows)
		}
		if rhsIsErr {
			for _, leftArg := range lhsFn.args {
				cs.rec(rhs, leftArg, false, shadows)
			}
			cs.rec(lhsFn.ret, rhs, false, shadows)
		}
	}

	if lhsClassTag, ok := lhs.(classTag); ok {
		if rhsObjType, ok := rhs.(objectTag); ok && lhsClassTag.containsParentST(rhsObjType.Id()) {
			return false
		}
	}

	if lhsVar, ok := lhs.(*typeVariable); ok {
		return cs.constrainTypeVarLhs(lhsVar, rhs, shadows)
	}
	if rhsVar, ok := rhs.(*typeVariable); ok {
		return cs.constrainTypeVarRhs(lhs, rhsVar, shadows)
	}
	// TODO ARRAYS/TUPLES HERE
	if lhsTuple, ok := lhs.(tupleType); ok {
		if r, ok := rhs.(tupleType); ok {
			return cs.constrainTupleTuple(lhsTuple, r, shadows)
		}
		if r, ok := rhs.(arrayType); ok {
			// Array subtyping: Tuple<T1,..Tn> <: Array<U> if (T1|...|Tn) <: U
			innerLhs := lhsTuple.inner(cs.ctx) // Needs implementation
			return cs.rec(innerLhs, r.innerT, false, shadows)
			// TODO: Handle bounds correctly (recLb in Scala)
		}
		// tupleType <: Other? -> goToWork or error

		// namedTupleType (Similar to tupleType)
	}
	if lhsNamedTuple, ok := lhs.(namedTupleType); ok {
		if r, ok := rhs.(namedTupleType); ok {
			return cs.constrainNamedTupleNamedTuple(lhsNamedTuple, r, shadows)
		}
		// namedTupleType <: Other? -> goToWork or error

		// arrayType
	}
	if lhsArray, ok := lhs.(arrayType); ok {
		if r, ok := rhs.(arrayType); ok {
			// Array<T> <: Array<U> if T <: U (covariance)
			// TODO: Handle bounds correctly (recLb in Scala for mutable arrays)
			return cs.rec(lhsArray.innerT, r.innerT, false, shadows)
		}
	}
	// TODO ARRAYS/TUPLES END

	if lhsUnion, ok := lhs.(unionType); ok {
		if cs.rec(lhsUnion.lhs, rhs, true, shadows) {
			return true
		}
		return cs.rec(lhsUnion.rhs, rhs, true, shadows)
	}

	if rhsInter, ok := rhs.(intersectionType); ok {
		if cs.rec(lhs, rhsInter.lhs, true, shadows) {
			return true
		}
		return cs.rec(lhs, rhsInter.rhs, true, shadows)
	}
	lhsIsErr := isErrorType(lhs)
	if lhsIsErr {
		rhsFn, isRhsFn := rhs.(funcType)
		if isRhsFn {
			for _, arg := range rhsFn.args {
				if cs.rec(lhs, arg, false, shadows) {
					return true
				}
			}
			return cs.rec(rhsFn.ret, lhs, false, shadows)
		}
	}

	leftRecord, okLhsRecord := lhs.(recordType)
	rightRecord, okRhsRecord := rhs.(recordType)
	if okLhsRecord && okRhsRecord {
		for _, rightField := range rightRecord.fields {
			found := false
			for _, leftField := range leftRecord.fields {
				if leftField.name.Name == rightField.name.Name {

					// note inverted constrain as this is the LB that we are constraining, so
					// polarity is as if this was a function return (ie, inverted polarity)
					if cs.rec(rightField.type_.lowerBound, leftField.type_.lowerBound, false, shadows) {
						return true
					}

					if cs.rec(leftField.type_.upperBound, rightField.type_.upperBound, false, shadows) {
						return true
					}
					found = true
					break
				}
			}
			if !found {
				// if we got this far, it means that we did not find right's field inside left
				if cs.reportError(fmt.Sprintf("could not find field %s", rightField.name.Name)) {
					return true
				}
			}
		}
		return false
	}
	// TODO
	//         err <: record
	//      record <: err
	//     typeRef <: typeRef
	//     typeRef <: _
	//           _ <: typeRef

	lhsTypeRef, okLhsTypeRef := lhs.(typeRef)
	rhsTypeRef, okRhsTypeRef := rhs.(typeRef)
	if okLhsTypeRef && okRhsTypeRef && lhsTypeRef.defName != ir.ArrayTypeName {
		if lhsTypeRef.defName == rhsTypeRef.defName {
			if len(lhsTypeRef.typeArgs) != len(rhsTypeRef.typeArgs) {
				return cs.reportError(fmt.Sprintf("type definition mismatch: %s vs %s", lhsTypeRef.defName, rhsTypeRef.defName))
			}
			def, ok := cs.ctx.typeDefs[lhsTypeRef.defName]
			if !ok {
				cs.ctx.addFailure(fmt.Sprintf("type definition not found: %s", lhsTypeRef.defName), lhs.prov())
			}
			for i, arg := range def.typeParamArgs {
				tv := arg.Snd
				leftArg := lhsTypeRef.typeArgs[i]
				rightArg := rhsTypeRef.typeArgs[i]
				variance := def.typeVarVariances[tv.id]
				if !variance.contravariant {
					cs.rec(leftArg, rightArg, false, shadows)
				}
				if !variance.covariant {
					cs.rec(rightArg, leftArg, false, shadows)
				}
			}
			return false
		}

		if lhsTag, ok := cs.ctx.classTagFrom(lhsTypeRef); ok {
			if rhsTag, ok := cs.ctx.classTagFrom(rhsTypeRef); ok && !cs.ctx.isSubtype(lhsTag, rhsTag, nil) {
				return cs.reportError("type mismatch")
			}
		}
		return cs.rec(cs.ctx.expand(lhsTypeRef, expandOpts{}), cs.ctx.expand(rhsTypeRef, expandOpts{}), true, shadows)
	}
	if okLhsTypeRef {
		return cs.rec(cs.ctx.expand(lhsTypeRef, expandOpts{}), rhs, true, shadows)
	}
	if okRhsTypeRef {
		return cs.rec(lhs, cs.ctx.expand(rhsTypeRef, expandOpts{}), true, shadows)
	}

	if lhsIsErr || rhsIsErr {
		return false
	}

	if _, ok := rhs.(unionType); ok {
		return cs.goToWork(lhs, rhs, shadows)
	}
	if _, ok := lhs.(intersectionType); ok {
		return cs.goToWork(lhs, rhs, shadows)
	}

	if okLhsNeg {
		// ~L <: _ -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, shadows)
	}
	if _, ok := rhs.(negType); ok {
		// _ <: ~~R -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, shadows)
	}

	cs.Error("constrain: no specific rule", "lhs", lhs, "rhs", rhs, "type_lhs", reflect.TypeOf(lhs), "type_rhs", reflect.TypeOf(rhs))
	return cs.reportError(fmt.Sprintf("cannot constrain %T <: %T", lhs, rhs))
}

// requiresGoToWork checks if the constraint likely needs DNF/CNF normalization.
func requiresGoToWork(lhs, rhs SimpleType, logger *slog.Logger) bool {
	// Simplified check. Scala logic is more involved.
	_, lhsIsUnion := lhs.(unionType)
	_, rhsIsInter := rhs.(intersectionType)
	_, lhsIsNeg := lhs.(negType)
	_, rhsIsNeg := rhs.(negType)
	// TODO: Check for Without type if implemented

	required := (lhsIsUnion && !isTop(rhs)) || (rhsIsInter && !isBottom(lhs)) || lhsIsNeg || rhsIsNeg

	logger.Debug(fmt.Sprintf("constrain: determined normalisation required for %s <: %s", lhs, rhs), "required", required)

	return required
}

func isTop(t SimpleType) bool {
	if et, ok := t.(extremeType); ok {
		return !et.polarity
	}
	return false
}
func isBottom(t SimpleType) bool {
	if et, ok := t.(extremeType); ok {
		return et.polarity
	}
	return false
}

// constrainTypeVarLhs handles `TypeVariable <: Rhs`
func (cs *constraintSolver) constrainTypeVarLhs(lhs *typeVariable, rhs SimpleType, shadows *shadowsState) bool {
	// Check levels
	if rhs.level() <= lhs.level() {
		return cs.addUpperBound(lhs, rhs)
	}

	// Extrusion needed for RHS
	cs.Debug("constraint: extruding RHS for type-variable LHS", "rhs_level", rhs.level(), "lhs_level", lhs.level())
	extrudedRhs := cs.extrude(rhs, lhs.level(), false) // Needs extrude implementation
	if extrudedRhs == nil {                            // Extrusion failed or reported error
		cs.reportError(fmt.Sprintf("cannot extrude RHS for type-variable LHS: %s", rhs))
		return true
	}
	return cs.rec(lhs, extrudedRhs, true, shadows) // Retry with extruded RHS
}

// constrainTypeVarRhs handles LHS <: TypeVariable
func (cs *constraintSolver) constrainTypeVarRhs(lhs SimpleType, rhs *typeVariable, shadows *shadowsState) bool {
	// Levels match or LHS is lower: Add lower bound
	if lhs.level() <= rhs.level() {
		return cs.addLowerBound(rhs, lhs)
	}

	cs.Debug("constraint: extruding LHS for type-variable RHS", "rhs_level", rhs.level(), "lhs_level", lhs.level())
	extrudedLhs := cs.extrude(lhs, rhs.level(), true) // Needs extrude implementation
	if extrudedLhs == nil {                           // Extrusion failed or reported error
		return true
	}
	return cs.rec(extrudedLhs, rhs, true, shadows) // Retry with extruded LHS

}

// constrainFuncFunc handles `FunctionType <: FunctionType`
func (cs *constraintSolver) constrainFuncFunc(lhs, rhs funcType, shadows *shadowsState) bool {
	if len(lhs.args) != len(rhs.args) {
		return cs.reportError(fmt.Sprintf("function arity mismatch: %d vs %d", len(lhs.args), len(rhs.args)))
	}
	// constrain arguments contravariantly: rhs.arg <: lhs.arg
	for i := range lhs.args {
		if cs.rec(rhs.args[i], lhs.args[i], false, shadows) {
			return true
		}
	}
	// constrain return type covariantly: lhs.ret <: rhs.ret
	return cs.rec(lhs.ret, rhs.ret, false, shadows)
}

// constrainTupleTuple handles `TupleType <: TupleType`
func (cs *constraintSolver) constrainTupleTuple(lhs, rhs tupleType, shadows *shadowsState) bool {
	if len(lhs.fields) != len(rhs.fields) {
		return cs.reportError(fmt.Sprintf("tuple size mismatch: %d vs %d", len(lhs.fields), len(rhs.fields)))
	}

	// constrain fields covariantly: lhs.field <: rhs.field
	// TODO: Handle named tuples correctly if names differ.
	// TODO: Handle bounds correctly (recLb in Scala).
	for i := range lhs.fields {
		if cs.rec(lhs.fields[i], rhs.fields[i], false, shadows) { // Note: sameLevel = false
			return true // Terminate early
		}
	}
	return false
}

// constrainNamedTupleNamedTuple handles `NamedTupleType <: NamedTupleType`
func (cs *constraintSolver) constrainNamedTupleNamedTuple(lhs, rhs namedTupleType, shadows *shadowsState) bool {
	if len(lhs.fields) != len(rhs.fields) {
		return cs.reportError(fmt.Sprintf("named tuple size mismatch: %d vs %d", len(lhs.fields), len(rhs.fields)))
	}

	// Check names and constrain fields covariantly
	// TODO: Handle bounds correctly (recLb in Scala).
	for i := range lhs.fields {
		// Assuming fields are ordered or using a map lookup
		if lhs.fields[i].Fst.Name != rhs.fields[i].Fst.Name {
			return cs.reportError(fmt.Sprintf("named tuple field name mismatch: '%s' vs '%s'", lhs.fields[i].Fst.Name, rhs.fields[i].Fst.Name))
		}
		if cs.rec(lhs.fields[i].Snd, rhs.fields[i].Snd, false, shadows) { // Note: sameLevel = false
			return true // Terminate early
		}
	}
	return false
}

// constrainTypeRefTypeRef handles `TypeRef <: TypeRef`
func (cs *constraintSolver) constrainTypeRefTypeRef(
	lhs, rhs typeRef,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	if lhs.defName != rhs.defName {
		// Different definitions, try expanding both
		// Need cycle detection for expansion
		expandedLhs := cs.ctx.expand(lhs, expandOpts{})
		expandedRhs := cs.ctx.expand(rhs, expandOpts{})
		if Equal(expandedLhs, lhs) && Equal(expandedRhs, rhs) { // Avoid infinite loop
			// Check structural subtyping via tags if possible (Scala: mkClsTag)
			// Or report error
			return cs.reportError(fmt.Sprintf("type definition mismatch: %s vs %s", lhs.defName, rhs.defName))
		}
		return cs.rec(expandedLhs, expandedRhs, true, shadows)
	}

	// Same definition, check type arguments based on variance
	if len(lhs.typeArgs) != len(rhs.typeArgs) {
		// Should not happen if defName is the same and definitions are consistent
		return cs.reportError("type argument count mismatch")
	}

	// Fetch variance info for the definition (needs Ctx support)
	variances, ok := cs.ctx.getTypeDefinitionVariances(lhs.defName) // Needs implementation
	if !ok {
		return cs.reportError(fmt.Sprintf("unknown type definition %s", lhs.defName))
	}
	if len(variances) != len(lhs.typeArgs) {
		return cs.reportError(fmt.Sprintf("variance info mismatch for %s", lhs.defName))
	}

	for i, variance := range variances {
		targLhs := lhs.typeArgs[i]
		targRhs := rhs.typeArgs[i]
		switch variance {
		case Covariant: // targLhs <: targRhs
			if cs.rec(targLhs, targRhs, false, shadows) {
				return true
			}
		case Contravariant: // targRhs <: targLhs
			if cs.rec(targRhs, targLhs, false, shadows) {
				return true
			}
		case Invariant: // targLhs =:= targRhs (constrain both ways)
			if cs.rec(targLhs, targRhs, false, shadows) {
				return true
			}
			if cs.rec(targRhs, targLhs, false, shadows) {
				return true
			}
		}
	}
	return false
}

// constrainClassRecord handles ClassTag <: RecordType
func (cs *constraintSolver) constrainClassRecord(
	lhs classTag,
	rhs recordType, // Assuming recordType exists
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	// For each field (fldName, fldTy) in rhs, lookup the corresponding
	// field type `fty` in lhs and constrain `fty.ub <: fldTy.ub`
	// and `fldTy.lb <: fty.lb` (recLb).
	className := lhs.id.CanonicalSyntax() // Assuming id gives the name
	for _, field := range rhs.fields {    // Assuming recordType has fields []Pair[Var, FieldType]
		fldName := field.name
		fldTy := field.type_ // Assuming FieldType exists with lb, ub

		// Lookup field in the class definition (needs Ctx support)
		// This involves getting the class info, handling type parameters, freshening etc.
		// Simplified lookup:
		memberTy, err := cs.ctx.LookupField(lhs, fldName) // Needs implementation
		if err != nil {
			return cs.reportError(fmt.Sprintf("class %s has no field %s required by record", className, fldName.Name))
		}

		// constrain upper bounds: memberTy.ub <: fldTy.ub
		if cs.rec(memberTy.ub, fldTy.upperBound, false, shadows) {
			return true
		}

		// constrain lower bounds: fldTy.lb <: memberTy.lb (recLb)
		if fldTy.lowerBound != nil {
			if memberTy.lb == nil {
				// Trying to assign to a non-mutable field
				return cs.reportError(fmt.Sprintf("field %s is not mutable", fldName.Name))
			}
			if cs.rec(fldTy.lowerBound, memberTy.lb, false, shadows) {
				return true
			}
		}
	}
	return false
}

// --- Placeholders for Complex Logic ---

// goToWork handles complex constraints using DNF/CNF normalization.
// This is a major piece requiring its own implementation based on NormalForms.scala.
func (cs *constraintSolver) goToWork(lhs, rhs SimpleType, shadows *shadowsState) bool {
	opsDnf := &opsDNF{
		ctx:    cs.ctx,
		Logger: cs.Logger.With("section", "inference.constraintSolver"),
	}
	cs.constrainDNF(opsDnf, opsDnf.mkDeep(lhs, true), opsDnf.mkDeep(rhs, false), shadows)
	return false
}

// constrainDNF handles constraining when types have been converted to DNF.
// This corresponds to the logic within goToWork inside constraining in the scala reference.
func (cs *constraintSolver) constrainDNF(ops *opsDNF, lhs, rhs dnf, shadows *shadowsState) {
	ops.Debug("constrain: for DNF", "lhs", lhs, "rhs", rhs)
	cs.annoyingCalls++

	// Iterate through each conjunct C in the LHS DNF (LHS = C1 | C2 | ...)
	// We need to ensure C <: RHS for all C.
eachConj:
	for _, conj := range lhs {
		if !conj.vars.Empty() {
			// Case 1: The conjunct has positive variables (TV & L & ~R & ~N <: RHS)
			// Strategy: Extract one variable TV and constrain TV <: (RHS | ~(L & ~R & ~N))
			first := util.IterFirstOrPanic(conj.vars.Items())
			single := set.TreeSetFrom[*typeVariable]([]*typeVariable{first}, compareTypeVars)
			newC := newConjunct(
				conj.lhs,
				conj.rhs,
				conj.vars.Difference(single),
				conj.nvars,
			)
			newRhs := unionOf(rhs.toType(), negateType(newC.toType(), emptyProv), unionOpts{})
			cs.rec(first, newRhs, true, shadows)
			break eachConj
		}
		// Case 2: The conjunct C has no positive variables (L & ~R & ~N <: RHS)

		nvarsAsDNF := util.MapIter(conj.nvars.Items(), func(nvar *typeVariable) dnf {
			return ops.mkDeep(nvar, true)
		})
		fullRhs := ops.or(rhs, ops.mkDeep(conj.rhs.toType(), false))
		for nvar := range nvarsAsDNF {
			fullRhs = ops.or(fullRhs, nvar)
		}

		ops.Debug(fmt.Sprintf("constrainDNF: considering %s <: %s", conj.lhs, fullRhs))

		lnf := conj.lhs

		possibleConjuncts := make([]conjunct, 0, len(fullRhs))
		for _, rConj := range fullRhs {
			_, isBot := rConj.rhs.(rhsBot)
			// Early exit check (corresponds to Scala's `if ((r.rnf is RhsBot)...)`)
			if isBot && rConj.vars.Empty() && rConj.nvars.Empty() {
				// If rConj is just an LHS part (rConj.lhs)
				if lnf.lessThanOrEqual(rConj.lhs) { // Check if lnf <: rConj.lhs
					ops.Debug("constrainDNF: Early exit", "lnf", lnf, "rConj.lhs", rConj.lhs)
					break eachConj // Skip to the next conjunct in the outer loop (lhs)
				}
			}

			// Filtering logic (corresponds to Scala's `filter` conditions)
			// 1. `!vars.exists(r.nvars)` - Always true here as conj.vars is empty.
			// 2. `((lnf & r.lnf)).isDefined` - Check if intersection is possible.
			_, ok := lnf.and(rConj.lhs, cs.ctx)

			if !ok {
				ops.Debug("constrainDNF: Filtered (Lnf intersection failed)", "lnf", lnf, "rConj.lhs", rConj.lhs.String())
				continue
			}

			// 3. Tag checks (simplified version)
			if ok := checkTagCompatibility(lnf, rConj.rhs); !ok {
				ops.Debug("constrainDNF: Filtered (Tag incompatibility) !<:", "lnf", lnf, "rConj.rhs", rConj.rhs.String())
				continue // Skip this rConj
			}

			// If all checks pass, add to possible conjuncts
			possibleConjuncts = append(possibleConjuncts, rConj)
		}
		ops.Debug("constrainDNF: possible conjuncts", "possibleConjuncts", possibleConjuncts)
		possibleAsTypes := make([]SimpleType, 0, len(possibleConjuncts))
		for _, rConj := range possibleConjuncts {
			possibleAsTypes = append(possibleAsTypes, rConj.toType())
		}
		cs.annoying(nil, lnf, possibleAsTypes, &rhsBot{})
	}
}

// annoyingHandleEmptyTypes handles the case where both leftTypes and rightTypes are empty.
// This is extracted to a separate function to make the annoying() function more left-aligned.
func (cs *constraintSolver) annoyingHandleEmptyTypes(doneLeft lhsNF, doneRight rhsNF) {
	if doneLeft.isLeftNfTop() {
		cs.reportError("Type constraint cannot be satisfied")
		return
	}

	if doneRight.isRhsBot() {
		cs.reportError("Type constraint cannot be satisfied")
		return
	}

	// Check for trait tag conflicts
	lhsRefined, okLhs := doneLeft.(*lhsRefined)
	rhsBases, okRhs := doneRight.(*rhsBases)

	if okLhs && okRhs {
		// Check if there are trait tag conflicts
		if lhsRefined.traitTags != nil {
			for tag := range lhsRefined.traitTags.Items() {
				if rhsBases.hasTag(tag) {
					cs.Debug("annoying: trait tag conflict", "tag", tag)
					return // Trait tag conflict, constraint is unsatisfiable
				}
			}
		}

		// Handle function types
		if lhsRefined.fn != nil {
			if rhsRest, ok := rhsBases.rest.(funcType); ok {
				// Constrain function types
				cs.rec(lhsRefined.fn, &rhsRest, true, nil)
				return
			}
		}

		// Handle class tags
		if lhsRefined.base != nil {
			for _, tag := range rhsBases.tags {
				if ct, ok := tag.(classTag); ok {
					// Check if lhsRefined.base is a subtype of ct
					if lhsRefined.base.containsParentST(ct.id) || Equal(*lhsRefined.base, ct) {
						cs.Debug("annoying: class tag conflict", "lhsBase", lhsRefined.base, "rhsTag", ct)
						return // Class tag conflict, constraint is unsatisfiable
					}
				}
			}
		}
	}

	// Handle record fields
	if okLhs {
		if rhsField, ok := doneRight.(*rhsField); ok {
			// Find matching field in lhsRefined
			for _, field := range lhsRefined.reft.fields {
				if field.name.Name == rhsField.name.Name {
					// Constrain field types
					cs.rec(field.type_.lowerBound, rhsField.ty.lowerBound, false, nil)
					cs.rec(field.type_.upperBound, rhsField.ty.upperBound, false, nil)
					return
				}
			}
			// Field not found in lhsRefined
			cs.reportError(fmt.Sprintf("Field '%s' not found", rhsField.name.Name))
			return
		}
	}

	// If we've reached here, we need to check if the constraint is satisfiable
	if !checkTagCompatibility(doneLeft, doneRight) {
		cs.reportError("Type constraint cannot be satisfied due to tag incompatibility")
		return
	}
}

// Solve annoying constraints,
// which are those that involve either unions and intersections at the wrong polarities or negations.
// This works by constructing all pairs of "conjunct <: disjunct" implied by the conceptual
// "DNF <: CNF" form of the constraint.
func (cs *constraintSolver) annoying(leftTypes []SimpleType, doneLeft lhsNF, rightTypes []SimpleType, doneRight rhsNF) {
	cs.Warn("constrain: annoying", "leftTypes", leftTypes, "doneLeft", doneLeft, "rightTypes", rightTypes, "doneRight", doneRight)
	cs.annoyingCalls++

	// Handle the case where both leftTypes and rightTypes are empty at the start
	if len(leftTypes) == 0 && len(rightTypes) == 0 {
		cs.annoyingHandleEmptyTypes(doneLeft, doneRight)
		return
	}

	// Handle type variables on the left side
	if len(leftTypes) > 0 {
		if tv, ok := leftTypes[0].(*typeVariable); ok {
			// Create a type that represents the right-hand side of the constraint
			var rhs SimpleType

			// Start with the negation of the remaining left types
			remainingLeftTypes := leftTypes[1:]

			// Collect all types for the right-hand side
			var types []SimpleType

			// Add negated left types
			for _, lt := range remainingLeftTypes {
				types = append(types, negateType(lt, emptyProv))
			}

			// Add right types
			types = append(types, rightTypes...)

			// Add doneRight types
			if !doneRight.isRhsBot() {
				types = append(types, doneRight.toType())
			}

			// Combine all types with union
			if len(types) == 0 {
				rhs = bottomType
			} else {
				rhs = types[0]
				for i := 1; i < len(types); i++ {
					rhs = unionOf(rhs, types[i], unionOpts{})
				}
			}

			// Check if all remaining left types are top
			allTop := doneLeft.isLeftNfTop()
			for _, lt := range remainingLeftTypes {
				if !isTop(lt) {
					allTop = false
					break
				}
			}

			// Add constraint: tv <: rhs
			cs.rec(tv, rhs, allTop, nil)
			return
		}
	}

	// Handle type variables on the right side
	if len(rightTypes) > 0 {
		if tv, ok := rightTypes[0].(*typeVariable); ok {
			// Create a type that represents the left-hand side of the constraint
			var lhs SimpleType

			// Start with the left types
			remainingRightTypes := rightTypes[1:]

			// Collect all types for the left-hand side
			var types []SimpleType

			// Add left types
			types = append(types, leftTypes...)

			// Add doneLeft types
			if !doneLeft.isLeftNfTop() {
				types = append(types, doneLeft.toType())
			}

			// Add negated right types
			for _, rt := range remainingRightTypes {
				types = append(types, negateType(rt, emptyProv))
			}

			// Combine all types with intersection
			if len(types) == 0 {
				lhs = topType
			} else {
				lhs = types[0]
				for i := 1; i < len(types); i++ {
					lhs = intersectionOf(lhs, types[i], unionOpts{})
				}
			}

			// Check if all remaining right types are bottom
			isBot := doneRight.isRhsBot()
			for _, rt := range remainingRightTypes {
				if !isBottom(rt) {
					isBot = false
					break
				}
			}

			// Add constraint: lhs <: tv
			cs.rec(lhs, tv, isBot, nil)
			return
		}
	}

	// Handle type ranges
	if len(leftTypes) > 0 {
		if tr, ok := leftTypes[0].(typeRange); ok {
			// For type ranges on the left, use the upper bound
			newLeftTypes := append([]SimpleType{tr.upperBound}, leftTypes[1:]...)
			cs.annoying(newLeftTypes, doneLeft, rightTypes, doneRight)
			return
		}
	}

	if len(rightTypes) > 0 {
		if tr, ok := rightTypes[0].(typeRange); ok {
			// For type ranges on the right, use the lower bound
			newRightTypes := append([]SimpleType{tr.lowerBound}, rightTypes[1:]...)
			cs.annoying(leftTypes, doneLeft, newRightTypes, doneRight)
			return
		}
	}

	// Handle union types on the left
	if len(leftTypes) > 0 {
		if ut, ok := leftTypes[0].(unionType); ok {
			// For unions on the left, split into two separate constraints
			newLeftTypes1 := append([]SimpleType{ut.lhs}, leftTypes[1:]...)
			cs.annoying(newLeftTypes1, doneLeft, rightTypes, doneRight)

			newLeftTypes2 := append([]SimpleType{ut.rhs}, leftTypes[1:]...)
			cs.annoying(newLeftTypes2, doneLeft, rightTypes, doneRight)
			return
		}
	}

	// Handle intersection types on the right
	if len(rightTypes) > 0 {
		if it, ok := rightTypes[0].(intersectionType); ok {
			// For intersections on the right, split into two separate constraints
			newRightTypes1 := append([]SimpleType{it.lhs}, rightTypes[1:]...)
			cs.annoying(leftTypes, doneLeft, newRightTypes1, doneRight)

			newRightTypes2 := append([]SimpleType{it.rhs}, rightTypes[1:]...)
			cs.annoying(leftTypes, doneLeft, newRightTypes2, doneRight)
			return
		}
	}

	// Handle intersection types on the left
	if len(leftTypes) > 0 {
		if it, ok := leftTypes[0].(intersectionType); ok {
			// For intersections on the left, combine both parts
			newLeftTypes := append([]SimpleType{it.lhs, it.rhs}, leftTypes[1:]...)
			cs.annoying(newLeftTypes, doneLeft, rightTypes, doneRight)
			return
		}
	}

	// Handle union types on the right
	if len(rightTypes) > 0 {
		if ut, ok := rightTypes[0].(unionType); ok {
			// For unions on the right, combine both parts
			newRightTypes := append([]SimpleType{ut.lhs, ut.rhs}, rightTypes[1:]...)
			cs.annoying(leftTypes, doneLeft, newRightTypes, doneRight)
			return
		}
	}

	// Handle negation types on the left
	if len(leftTypes) > 0 {
		if nt, ok := leftTypes[0].(negType); ok {
			// For negations on the left, move the negated type to the right
			newLeftTypes := leftTypes[1:]
			newRightTypes := append([]SimpleType{nt.negated}, rightTypes...)
			cs.annoying(newLeftTypes, doneLeft, newRightTypes, doneRight)
			return
		}
	}

	// Handle negation types on the right
	if len(rightTypes) > 0 {
		if nt, ok := rightTypes[0].(negType); ok {
			// For negations on the right, move the negated type to the left
			newRightTypes := rightTypes[1:]
			newLeftTypes := append([]SimpleType{nt.negated}, leftTypes...)
			cs.annoying(newLeftTypes, doneLeft, newRightTypes, doneRight)
			return
		}
	}

	// Handle bottom type on the left (makes constraint trivial)
	if len(leftTypes) > 0 && isBottom(leftTypes[0]) {
		return // Constraint is trivially satisfied
	}

	// Handle top type on the right (makes constraint trivial)
	if len(rightTypes) > 0 && isTop(rightTypes[0]) {
		return // Constraint is trivially satisfied
	}

	// Handle top type on the left (can be skipped)
	if len(leftTypes) > 0 && isTop(leftTypes[0]) {
		cs.annoying(leftTypes[1:], doneLeft, rightTypes, doneRight)
		return
	}

	// Handle bottom type on the right (can be skipped)
	if len(rightTypes) > 0 && isBottom(rightTypes[0]) {
		cs.annoying(leftTypes, doneLeft, rightTypes[1:], doneRight)
		return
	}

	// Handle proxy types
	if len(leftTypes) > 0 {
		if pt, ok := leftTypes[0].(wrappingProvType); ok {
			// For proxy types on the left, use the underlying type
			newLeftTypes := append([]SimpleType{pt.underlying()}, leftTypes[1:]...)
			cs.annoying(newLeftTypes, doneLeft, rightTypes, doneRight)
			return
		}
	}

	if len(rightTypes) > 0 {
		if pt, ok := rightTypes[0].(wrappingProvType); ok {
			// For proxy types on the right, use the underlying type
			newRightTypes := append([]SimpleType{pt.underlying()}, rightTypes[1:]...)
			cs.annoying(leftTypes, doneLeft, newRightTypes, doneRight)
			return
		}
	}

	// Handle type references and basic types on the left
	if len(leftTypes) > 0 {
		// Convert SimpleType to lhsNF
		var lnf lhsNF

		// Unwrap provenance
		ty := leftTypes[0]
		ty = unwrapProvenance(ty)

		// Convert based on type
		switch t := ty.(type) {
		case funcType:
			lnf = &lhsRefined{fn: &t, reft: emptyRecord, traitTags: set.NewTreeSet(compareTraitTags)}
		case arrayBase: // Covers tupleType, arrayType, namedTupleType
			lnf = &lhsRefined{arr: t, reft: emptyRecord, traitTags: set.NewTreeSet(compareTraitTags)}
		case classTag:
			lnf = &lhsRefined{base: &t, reft: emptyRecord, traitTags: set.NewTreeSet(compareTraitTags)}
		case traitTag:
			traitSet := set.NewTreeSet(compareTraitTags)
			traitSet.Insert(t)
			lnf = &lhsRefined{reft: emptyRecord, traitTags: traitSet}
		case recordType:
			lnf = &lhsRefined{reft: t, traitTags: set.NewTreeSet(compareTraitTags)}
		case extremeType:
			if !t.polarity { // Top
				lnf = lhsTop{}
			} else { // Bottom
				// Bottom on the left makes the constraint trivial
				return
			}
		case typeRef:
			refs := []typeRef{t}
			lnf = &lhsRefined{typeRefs: refs, reft: emptyRecord, traitTags: set.NewTreeSet(compareTraitTags)}
		default:
			// For other types, we can't easily convert to lhsNF
			cs.Debug("annoying: unsupported left type", "type", leftTypes[0])
			return
		}

		// Try to add the converted lhsNF to doneLeft
		newDoneLeft, ok := doneLeft.and(lnf, cs.ctx)
		if ok {
			cs.annoying(leftTypes[1:], newDoneLeft, rightTypes, doneRight)
			return
		} else {
			// If we can't add it to doneLeft, the constraint is unsatisfiable
			cs.Debug("annoying: incompatible left type", "type", leftTypes[0], "doneLeft", doneLeft)
			return
		}
	}

}

// checkTagCompatibility performs tag compatibility checks similar to the Scala filter.
func checkTagCompatibility(lnf lhsNF, rnf rhsNF) bool {
	lhsRefined, okLhs := lnf.(*lhsRefined)
	if !okLhs {
		return true // LhsTop is compatible with anything
	}

	rhsBases, okRhs := rnf.(*rhsBases)
	if !okRhs {
		return true // RhsBot or RhsField don't have tags to conflict
	}

	// Check 1: Trait tag conflict
	// If LHS has trait T and RHS has ~T, there's a conflict
	for _, rhsTag := range rhsBases.tags {
		if tt, ok := rhsTag.(traitTag); ok {
			if lhsRefined.traitTags.Contains(tt) {
				return false // Conflict: LHS has trait T, RHS has ~T
			}
		}
	}

	// Check 2: Class tag conflict
	// If LHS has class C and RHS has ~C, there's a conflict
	if lhsRefined.base != nil { // If LHS has a class tag
		for _, rhsTag := range rhsBases.tags {
			// Check if RHS contains the same class tag
			if ct, ok := rhsTag.(classTag); ok && Equal(ct, *(lhsRefined.base)) {
				return false // Conflict: LHS has class C, RHS has ~C
			}

			// Check inheritance relationship: if C <: D and RHS has ~D, there's a conflict
			if ct, ok := rhsTag.(classTag); ok && lhsRefined.base != nil {
				// Check if LHS class is a subtype of RHS class (using parent info)
				clsTag := *(lhsRefined.base)
				if clsTag.parents.Contains(ct.id.CanonicalSyntax()) {
					return false // Conflict: LHS has class C, RHS has ~D, and C <: D
				}
			}
		}
	}

	return true // No conflicts found
}

// ... (rest of the file, including extrude, helpers, etc.) ...

// extrude copies a type up to its type variables of wrong level (and their extruded bounds).
// It returns the extruded type, or nil if an error occurred during extrusion.
func (cs *constraintSolver) extrude(ty SimpleType, targetLvl level, pol bool) SimpleType {
	if ty.level() <= targetLvl {
		return ty
	}

	switch t := ty.(type) {
	case typeRange:
		if pol { // Positive polarity
			return cs.extrude(t.upperBound, targetLvl, true)
		}
		return cs.extrude(t.lowerBound, targetLvl, false) // Negative polarity

	case funcType:
		extrudedLhs := cs.extrude(t.args[0], targetLvl, !pol) // Contravariant
		if extrudedLhs == nil {
			return nil
		}
		extrudedRhs := cs.extrude(t.ret, targetLvl, pol) // Covariant
		if extrudedRhs == nil {
			return nil
		}
		// Assuming single argument function for now, like Scala's FunctionType(l, r)
		return funcType{args: []SimpleType{extrudedLhs}, ret: extrudedRhs, withProvenance: t.withProvenance}

	case unionType: // ComposedType(true, ...)
		extrudedLhs := cs.extrude(t.lhs, targetLvl, pol)
		if extrudedLhs == nil {
			return nil
		}
		extrudedRhs := cs.extrude(t.rhs, targetLvl, pol)
		if extrudedRhs == nil {
			return nil
		}
		// Use unionOf for potential simplification
		return unionOf(extrudedLhs, extrudedRhs, unionOpts{prov: t.prov()})

	case intersectionType: // ComposedType(false, ...)
		extrudedLhs := cs.extrude(t.lhs, targetLvl, pol)
		if extrudedLhs == nil {
			return nil
		}
		extrudedRhs := cs.extrude(t.rhs, targetLvl, pol)
		if extrudedRhs == nil {
			return nil
		}
		// Use intersectionOf for potential simplification
		return intersectionOf(extrudedLhs, extrudedRhs, unionOpts{prov: t.prov()})

	case recordType:
		newFields := make([]recordField, len(t.fields))
		for i, field := range t.fields {
			// Extrude field bounds: lb is contravariant (!pol), ub is covariant (pol)
			extrudedLb := cs.extrude(field.type_.lowerBound, targetLvl, !pol)
			if extrudedLb == nil {
				return nil
			}
			extrudedUb := cs.extrude(field.type_.upperBound, targetLvl, pol)
			if extrudedUb == nil {
				return nil
			}
			newFields[i] = recordField{
				name:  field.name,
				type_: fieldType{lowerBound: extrudedLb, upperBound: extrudedUb, withProvenance: withProvenance{field.type_.prov()}},
			}
		}
		// Use makeRecordType for potential sorting/simplification
		panic("extrude: implement makeRecordType")
		//return makeRecordType(newFields, &t.provenance)

	case tupleType:
		newFields := make([]SimpleType, len(t.fields))
		for i, field := range t.fields {
			extrudedField := cs.extrude(field, targetLvl, pol)
			if extrudedField == nil {
				return nil
			}
			newFields[i] = extrudedField
		}
		return tupleType{fields: newFields, withProvenance: t.withProvenance}

	case namedTupleType: // Similar to tupleType
		newFields := make([]util.Pair[ir.Var, SimpleType], len(t.fields))
		for i, field := range t.fields {
			extrudedField := cs.extrude(field.Snd, targetLvl, pol)
			if extrudedField == nil {
				return nil
			}
			newFields[i] = util.Pair[ir.Var, SimpleType]{Fst: field.Fst, Snd: extrudedField}
		}
		return namedTupleType{fields: newFields, withProvenance: t.withProvenance}

	case arrayType:
		extrudedInner := cs.extrude(t.innerT, targetLvl, pol) // Assuming covariant for now
		if extrudedInner == nil {
			return nil
		}
		return arrayType{innerT: extrudedInner, withProvenance: t.withProvenance}

	case *typeVariable:
		key := polarVariableKey{tv: t.id, pol: polarityFromBool(pol)}
		if cachedVar, ok := cs.extrusionCache[key]; ok {
			return cachedVar
		}

		// Create a new variable at the target level
		nv := cs.ctx.fresher.newTypeVariable(targetLvl, t.prov(), t.nameHint, nil, nil)
		cs.extrusionCache[key] = nv // Cache it immediately

		if pol { // Positive polarity
			// Add nv as an upper bound to the original variable t
			t.upperBounds = append(t.upperBounds, nv)
			// Set lower bounds of nv by extruding lower bounds of t
			nv.lowerBounds = make([]SimpleType, 0, len(t.lowerBounds))
			for _, lb := range t.lowerBounds {
				extrudedLb := cs.extrude(lb, targetLvl, pol)
				if extrudedLb == nil {
					return nil // Propagate failure
				}
				nv.lowerBounds = append(nv.lowerBounds, extrudedLb)
			}
		} else { // Negative polarity
			// Add nv as a lower bound to the original variable t
			t.lowerBounds = append(t.lowerBounds, nv)
			// Set upper bounds of nv by extruding upper bounds of t
			nv.upperBounds = make([]SimpleType, 0, len(t.upperBounds))
			for _, ub := range t.upperBounds {
				extrudedUb := cs.extrude(ub, targetLvl, pol)
				if extrudedUb == nil {
					return nil // Propagate failure
				}
				nv.upperBounds = append(nv.upperBounds, extrudedUb)
			}
		}
		return nv

	case negType:
		extrudedNegated := cs.extrude(t.negated, targetLvl, pol) // Polarity stays the same for negation itself? Check Scala. Yes.
		if extrudedNegated == nil {
			return nil
		}
		// Use negateType for potential simplification
		return negateType(extrudedNegated, t.prov())

	case extremeType:
		return t // Level 0

	case wrappingProvType: // ProvType
		extrudedUnderlying := cs.extrude(t.underlying(), targetLvl, pol)
		if extrudedUnderlying == nil {
			return nil
		}
		// Re-wrap with the original provenance
		return wrappingProvType{SimpleType: extrudedUnderlying, proxyProvenance: t.prov()}

	// case ProxyType: // Scala has this, Go doesn't seem to have a direct equivalent yet
	// 	return cs.extrude(t.underlying, targetLvl, pol)

	case classTag, traitTag:
		return t // Level 0

	case typeRef:
		// Need variance info to extrude type arguments correctly
		variances, ok := cs.ctx.getTypeDefinitionVariances(t.defName)
		if !ok {
			// Cannot extrude if definition is unknown, treat as opaque? Or error?
			// Scala seems to return the original typeRef here. Let's do that.
			// This might be incorrect if the typeRef *contains* higher-level variables.
			// A safer approach might be to report an error.
			cs.Warn("extrude: unknown type definition, cannot extrude TypeRef arguments", "typeRef", t)
			return t // Or return nil and report error?
		}
		if len(variances) != len(t.typeArgs) {
			cs.Warn("extrude: variance/type argument mismatch", "typeRef", t)
			return t // Or return nil and report error?
		}

		newTargs := make([]SimpleType, len(t.typeArgs))
		for i, targ := range t.typeArgs {
			argPol := combinePolarity(polarityFromBool(pol), variances[i])
			var extrudedArg SimpleType
			if argPol == invariant {
				// Extrude as a TypeRange (like Scala's mapTargs N case)
				extrudedLb := cs.extrude(targ, targetLvl, false)
				if extrudedLb == nil {
					return nil
				}
				extrudedUb := cs.extrude(targ, targetLvl, true)
				if extrudedUb == nil {
					return nil
				}
				// Use makeTypeRange for potential simplification
				extrudedArg = cs.ctx.makeTypeRange(extrudedLb, extrudedUb, targ.prov())
			} else {
				// Extrude with the calculated polarity
				extrudedArg = cs.extrude(targ, targetLvl, argPol == positive)
				if extrudedArg == nil {
					return nil
				}
			}
			newTargs[i] = extrudedArg
		}
		return typeRef{defName: t.defName, typeArgs: newTargs, withProvenance: t.withProvenance}

	default:
		panic(fmt.Sprintf("extrude: unhandled type %T", ty))
	}
}

// polarityFromBool converts bool (true=positive, false=negative) to polarity enum.
// Assumes invariant case is handled separately where needed.
func polarityFromBool(pol bool) polarity {
	if pol {
		return positive
	}
	return negative
}

// --- Type Definition Lookup Helpers (Need implementation in TypeCtx) ---

// IsSubtypeTag checks if tag1 is a subtype of tag2 (class/trait inheritance).
func (ctx *TypeCtx) IsSubtypeTag(tag1, tag2 objectTag) bool {
	if Equal(tag1, tag2) {
		return true
	}
	asClassTag, ok := tag1.(classTag)
	if !ok {
		panic("IsSubtypeTag called with a trait not supported yet")
	}
	return asClassTag.parents.Contains(tag2.Id().CanonicalSyntax())
}

// LookupField finds the type of a field within a class or trait context.
// This needs to handle inheritance, type parameter substitution, etc.
func (ctx *TypeCtx) LookupField(classType classTag, fieldName ir.Var) (*FieldType, error) {
	// TODO: Implement based on Scala's lookupField/getFieldType.
	// Needs access to type definitions (simplifier.tyDefs/tyDefs2).
	// Needs to handle freshening of type parameters based on classType's arguments (if any).
	fmt.Printf("WARN: LookupField not implemented for %s.%s\n", classType.id.CanonicalSyntax(), fieldName.Name)
	// Placeholder: Return a dummy field type or error
	return nil, fmt.Errorf("LookupField not implemented")
}

// Variance enum
type Variance int

const (
	Invariant Variance = iota
	Covariant
	Contravariant
)

// FieldType represents the type of a record field, potentially mutable.
// Corresponds to FieldType in Scala.
type FieldType struct {
	lb   SimpleType // Lower bound (for mutable fields, otherwise nil)
	ub   SimpleType // Upper bound
	prov typeProvenance
}

// --- Shadow State for Cycle Detection (Placeholder) ---
type shadowPair struct {
	lhsShadow uintptr // Using address as a simple shadow ID
	rhsShadow uintptr
}
type shadowsState struct {
	current  map[constraintPair]struct{} // Current constraints in this path
	previous map[shadowPair]struct{}     // Shadows seen in the entire run
}

// TODO: Implement methods for shadowsState: detectCycle, addConstraint, resetCurrent etc.
