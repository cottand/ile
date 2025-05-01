package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"github.com/cottand/ile/util"
	set "github.com/hashicorp/go-set/v3"
	"go/token"
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
		// Initialize shadows, extrCtx etc. if implemented
	}
}

// --- Helper methods for the solver ---

func (cs *constraintSolver) consumeFuel(currentLhs, currentRhs SimpleType, _ constraintContext) bool {
	cs.fuel--
	cs.depth++
	if cs.depth > defaultDepthLimit {
		// Simplified error reporting
		return cs.onErr(ilerr.New(ilerr.NewTypeMismatch{
			Positioner: cs.prov.Range,
			First:      fmt.Sprintf("%s (%s)", currentLhs, currentLhs.prov().desc),
			Second:     fmt.Sprintf("%s (%s)", currentRhs, currentRhs.prov().desc),
			Reason:     "exceeded max depth limit",
		})) // TODO Check if we should terminate
	}
	if cs.fuel <= 0 {
		return cs.onErr(ilerr.New(ilerr.NewTypeMismatch{
			Positioner: cs.prov.Range,
			First:      fmt.Sprintf("%s (%s)", currentLhs, currentLhs.prov().desc),
			Second:     fmt.Sprintf("%s (%s)", currentRhs, currentRhs.prov().desc),
			Reason:     "ran out of fuel",
		})) // TODO Check if we should terminate
	}
	return false // Continue
}

func (cs *constraintSolver) reportError(failureMsg string, lhs, rhs SimpleType, cctx constraintContext) bool {
	// Simplified error reporting. A full implementation would mirror the Scala version's
	// detailed provenance tracking and message generation.
	lhsProv := lhs.prov()
	if len(cctx.lhsChain) > 0 {
		lhsProv = cctx.lhsChain[0].prov() // Use the start of the chain
	}
	rhsProv := rhs.prov()
	if len(cctx.rhsChain) > 0 {
		rhsProv = cctx.rhsChain[0].prov() // Use the start of the chain
	}

	// Use the most relevant Range - often the top-level one
	pos := cs.prov.Range
	if lhsProv.Pos() != token.NoPos {
		pos = lhsProv.Range // Or maybe lhs? Needs refinement based on Scala logic
	}

	err := ilerr.New(ilerr.NewTypeMismatch{
		Positioner: pos,
		First:      fmt.Sprintf("%s (%s)", lhs.String(), lhsProv.desc),
		Second:     fmt.Sprintf("%s (%s)", rhs.String(), rhsProv.desc),
		Reason:     failureMsg,
	})
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
func (cs *constraintSolver) addUpperBound(tv *typeVariable, rhs SimpleType, cctx constraintContext) bool {
	logger.Debug("constrain: adding upper bound", "bound", rhs, "var", tv)
	// Simplified: Add bound and propagate. Scala version uses mkProxy.
	// Need to handle potential duplicates and normalization.
	newBound := rhs // TODO: Apply makeProxy based on cctx
	tv.upperBounds = append(tv.upperBounds, newBound)

	// Propagate constraints: L <: new_rhs for all L in lowerBounds
	for _, lb := range tv.lowerBounds {
		if cs.rec(lb, newBound, true, cctx, nil /* TODO: shadows */) {
			return true // Terminate early if propagation fails
		}
	}
	return false
}

// addLowerBound adds lhs as a lower bound to the type variable tv.
func (cs *constraintSolver) addLowerBound(tv *typeVariable, lhs SimpleType, cctx constraintContext) bool {
	logger.Debug("adding lower bound", "lhs", lhs, "tv", tv)
	newBound := lhs
	for c := range util.ConcatIter(slices.Values(cctx.lhsChain), util.Reverse(cctx.rhsChain)) {
		if c.prov() != emptyProv {
			newBound = makeProxy(newBound, c.prov())
		}
	}

	tv.lowerBounds = append(tv.lowerBounds, newBound)

	// Propagate constraints: new_lhs <: U for all U in upperBounds
	for _, ub := range tv.upperBounds {
		if cs.rec(newBound, ub, true, cctx, nil /* TODO: shadows */) {
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

	logger.Debug("constrain: begin for", "lhs", lhs, "rhs", rhs)

	// fmt.Printf("  where %s\n", functionType{args: []SimpleType{lhs}, ret: rhs}.String()) // Assuming functionType exists

	// Start the recursive constraining process
	// Initial context and previous contexts are empty
	initialCtx := constraintContext{lhsChain: nil, rhsChain: nil}
	// initialPrevCctxs := []constraintContext{} // If needed for extrusion reasons
	initialShadows := shadowsState{} // Assuming shadowsState is defined

	return solver.rec(lhs, rhs, true, initialCtx, &initialShadows)
}

// rec is the main recursive function for constraining.
// It handles fuel, depth, stack, context propagation, and caching.
// Returns true if constraint solving should terminate early.
func (cs *constraintSolver) rec(
	lhs, rhs SimpleType,
	sameLevel bool, // Indicates if we are in a nested position (affecting context/shadows)
	cctx constraintContext,
	shadows *shadowsState, // Pointer to allow modification
// prevCctxs []constraintContext, // If needed for extrusion reasons
) bool {
	cs.constrainCalls++
	_ = constraintPair{lhs, rhs}
	cs.push(lhs, rhs)
	defer cs.pop() // Ensure stack is popped on return

	if cs.consumeFuel(lhs, rhs, cctx) {
		return true // Terminate due to fuel/depth
	}

	// Update context for the recursive call
	var nextCctx constraintContext
	if sameLevel {
		nextCctx = cctx
		// Prepend without reallocating if possible (optimization)
		if len(cctx.lhsChain) == 0 || !cctx.lhsChain[0].Equivalent(lhs) { // Avoid duplicates
			nextCctx.lhsChain = slices.Insert(cctx.lhsChain, 0, lhs)
		}
		if len(cctx.rhsChain) == 0 || !cctx.rhsChain[0].Equivalent(rhs) { // Avoid duplicates
			nextCctx.rhsChain = slices.Insert(cctx.rhsChain, 0, rhs)
		}
	} else {
		nextCctx = constraintContext{
			lhsChain: []SimpleType{lhs},
			rhsChain: []SimpleType{rhs},
		}
	}

	// TODO: Update prevCctxs if needed

	// TODO: Update shadows state
	nextShadows := shadows // Simplified - needs proper update logic
	if !sameLevel {
		// Reset current shadows when level changes
		// nextShadows = shadows.resetCurrent()
	}

	return cs.recImpl(lhs, rhs, nextCctx, nextShadows)
}

func isErrorType(ty SimpleType) bool {
	return ty.Equivalent(errorTypeInstance)
}

// recImpl contains the core subtyping logic based on type structure.
// Returns true if constraint solving should terminate early.
func (cs *constraintSolver) recImpl(
	lhs, rhs SimpleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	logger.Debug("constrain", "level", cs.level, "lhs", lhs, "rhs", rhs, "fuel", cs.fuel)

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

	// 3. Unwrap Provenance Wrappers (like ProvType in Scala)
	if lhsWrap, ok := lhs.(wrappingProvType); ok {
		return cs.rec(lhsWrap.underlying(), rhs, true, cctx, shadows)
	}
	if rhsWrap, ok := rhs.(wrappingProvType); ok {
		return cs.rec(lhs, rhsWrap.underlying(), true, cctx, shadows)
	}

	// 4. Handle specific type combinations (Switch statement mirroring Scala's match)
	switch lhs := lhs.(type) {
	case extremeType: // Top or Bottom
		if lhs.polarity == true { // Bottom <: Anything
			return false // Success
		}
		// Top <: RHS only if RHS is Top
		if r, ok := rhs.(extremeType); ok && !r.polarity {
			return false // Top <: Top
		}
		// Fall through to report error or handle Top <: X via goToWork

	case *typeVariable:
		return cs.constrainTypeVarLhs(lhs, rhs, cctx, shadows)

	case funcType:
		if r, ok := rhs.(funcType); ok {
			return cs.constrainFuncFunc(lhs, r, cctx, shadows)
		}
		// funcType <: Other? -> goToWork or error

	case tupleType:
		if r, ok := rhs.(tupleType); ok {
			return cs.constrainTupleTuple(lhs, r, cctx, shadows)
		}
		if r, ok := rhs.(arrayType); ok {
			// Array subtyping: Tuple<T1,..Tn> <: Array<U> if (T1|...|Tn) <: U
			innerLhs := lhs.inner() // Needs implementation
			return cs.rec(innerLhs, r.innerT, false, cctx, shadows)
			// TODO: Handle bounds correctly (recLb in Scala)
		}
		// tupleType <: Other? -> goToWork or error

	case namedTupleType: // Similar to tupleType
		if r, ok := rhs.(namedTupleType); ok {
			return cs.constrainNamedTupleNamedTuple(lhs, r, cctx, shadows)
		}
		// namedTupleType <: Other? -> goToWork or error

	case arrayType:
		if r, ok := rhs.(arrayType); ok {
			// Array<T> <: Array<U> if T <: U (covariance)
			// TODO: Handle bounds correctly (recLb in Scala for mutable arrays)
			return cs.rec(lhs.innerT, r.innerT, false, cctx, shadows)
		}
		// arrayType <: Other? -> goToWork or error

	case intersectionType:
		// (L1 & L2) <: R  =>  L1 <: R AND L2 <: R
		if cs.rec(lhs.lhs, rhs, true, cctx, shadows) {
			return true
		}
		return cs.rec(lhs.rhs, rhs, true, cctx, shadows)

	case unionType:
		// L <: (R1 | R2) -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case negType:
		if r, ok := rhs.(negType); ok {
			// ~L <: ~R  =>  R <: L
			return cs.rec(r.negated, lhs.negated, true, cctx, shadows)
		}
		// ~L <: R -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case typeRef:
		// Expand LHS and retry
		// Need a mechanism to prevent infinite expansion
		expandedLhs := cs.ctx.expand(lhs) // Assuming expand exists
		// TODO this might need to be a Equiv rather than actual comparison
		if expandedLhs.Equivalent(lhs) { // Avoid infinite loop if expansion didn't change anything
			// Fall through to handle TypeRef vs RHS
		} else {
			return cs.rec(expandedLhs, rhs, true, cctx, shadows)
		}
		// Handle TypeRef vs TypeRef (variance check) or TypeRef vs Other
		if r, ok := rhs.(typeRef); ok {
			return cs.constrainTypeRefTypeRef(lhs, r, cctx, shadows)
		}
		// Fall through

	case *PolymorphicType:
		// Instantiate LHS and retry
		// Need to manage levels correctly
		instantiatedLhs := lhs.instantiate(cs.ctx.fresher, cs.level) // Instantiate at current solver level
		// TODO: Pass previous contexts (prevCctxs) if needed for extrusion reasons
		return cs.rec(instantiatedLhs, rhs, true, cctx, shadows)

	case typeRange: // Equivalent to TypeBounds in Scala
		// L..U <: R => U <: R
		return cs.rec(lhs.upperBound, rhs, true, cctx, shadows)

	case classTag:
		switch rhs := rhs.(type) {
		// Handle ClassTag <: RecordType (member checking)
		case objectTag:
			// Check inheritance/equality
			if cs.ctx.IsSubtypeTag(lhs, rhs) {
				return false // Success
			}
		// Handle ClassTag <: RHS
		case recordType:
			return cs.constrainClassRecord(lhs, rhs, cctx, shadows)
		}

		// special cases where lhs is the error type - we continue constraining so that we can get
		// some info on the other types
		if isErrorType(lhs) {
			err := lhs
			if rhs, rhsIsFunction := rhs.(funcType); rhsIsFunction {
				for _, arg := range rhs.args {
					cs.rec(arg, err, false, cctx, shadows)
				}
				cs.rec(err, rhs.ret, false, cctx, shadows)
			}
			if rhs, rhsIsRecordType := rhs.(recordType); rhsIsRecordType {
				for _, field := range rhs.fields {
					cs.rec(err, field.Snd.upperBound, false, cctx, shadows)
				}
			}
			return false
		}
		// Fall through

	case traitTag:
		// Handle TraitTag <: RHS
		if rTag, ok := rhs.(objectTag); ok {
			// Check inheritance/equality
			if cs.ctx.IsSubtypeTag(lhs, rTag) { // Needs implementation
				return false // Success
			}
		}
		// Fall through

		// TODO: Add cases for RecordType, other specific types as needed
	}

	// Handle RHS structure (if LHS didn't match a specific case)
	switch r := rhs.(type) {
	case extremeType: // Bottom or Top
		if r.polarity == false { // Anything <: Top
			return false // Success
		}
		// L <: Bottom only if L is Bottom
		if l, ok := lhs.(extremeType); ok && l.polarity {
			return false // Bottom <: Bottom
		}
		// Fall through to report error or handle X <: Bottom via goToWork

	case *typeVariable:
		return cs.constrainTypeVarRhs(lhs, r, cctx, shadows)

	case unionType:
		// L <: (R1 | R2) => L <: R1 AND L <: R2
		if cs.rec(lhs, r.lhs, true, cctx, shadows) {
			return true
		}
		return cs.rec(lhs, r.rhs, true, cctx, shadows)

	case intersectionType:
		// L <: (R1 & R2) -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case negType:
		// L <: ~R -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case typeRef:
		// Expand RHS and retry
		expandedRhs := cs.ctx.expand(r) // Assuming expand exists
		if expandedRhs.Equivalent(rhs) {
			// Fall through if expansion didn't change anything
		} else {
			return cs.rec(lhs, expandedRhs, true, cctx, shadows)
		}
		// Fall through

	case *PolymorphicType:
		// L <: forall V. R => Enter new level, rigidify R, and constrain L <: rigid R
		return cs.withSubLevel(func(subSolver *constraintSolver) bool {
			rigidRhs := r.rigidify(cs.ctx.fresher, subSolver.level) // Rigidify at the *new* level
			fmt.Printf(" -> Rigidified RHS: %s\n", rigidRhs)
			// constrain LHS against the rigidified RHS in the sub-level
			return subSolver.rec(lhs, rigidRhs, true, cctx, shadows) // Pass original cctx? Scala passes `true` for sameLevel
			// Unstashing happens automatically when withSubLevel returns (if implemented)
		})

	case typeRange: // Equivalent to TypeBounds in Scala
		// L <: L'..U' => L <: L'
		return cs.rec(lhs, r.lowerBound, true, cctx, shadows)

	case classTag:
		// special cases where rhs is the error type - we continue constraining so that we can get
		// some info on the other types
		if isErrorType(rhs) {
			err := rhs
			if lhs, lhsIsFunction := lhs.(funcType); lhsIsFunction {
				for _, arg := range lhs.args {
					cs.rec(err, arg, false, cctx, shadows)
				}
				cs.rec(lhs.ret, err, false, cctx, shadows)
			}
			if lhs, lhsIsRecordType := lhs.(recordType); lhsIsRecordType {
				for _, field := range lhs.fields {
					cs.rec(field.Snd.upperBound, err, false, cctx, shadows)
				}
			}
			return false
		}

		// TODO: Add cases for RecordType on RHS, etc.
	}

	// 6. If no specific rule matched, report error or try complex solving
	logger.Debug("constrain: no specific rule", "lhs", lhs, "rhs", rhs, "type_lhs", reflect.TypeOf(lhs), "type_rhs", reflect.TypeOf(rhs))
	// Attempt goToWork for complex cases involving unions/intersections/negations
	if requiresGoToWork(lhs, rhs) {
		return cs.goToWork(lhs, rhs, cctx, shadows)
	}

	// 7. Default: Report Error
	return cs.reportError(fmt.Sprintf("cannot constrain %T <: %T", lhs, rhs), lhs, rhs, cctx)
}

// requiresGoToWork checks if the constraint likely needs DNF/CNF normalization.
func requiresGoToWork(lhs, rhs SimpleType) bool {
	// Simplified check. Scala logic is more involved.
	_, lhsIsUnion := lhs.(unionType)
	_, rhsIsInter := rhs.(intersectionType)
	_, lhsIsNeg := lhs.(negType)
	_, rhsIsNeg := rhs.(negType)
	// TODO: Check for Without type if implemented

	return (lhsIsUnion && !isTop(rhs)) || (rhsIsInter && !isBottom(lhs)) || lhsIsNeg || rhsIsNeg
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
func (cs *constraintSolver) constrainTypeVarLhs(
	lhs *typeVariable,
	rhs SimpleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	// Check levels
	if rhs.level() <= lhs.level() {
		return cs.addUpperBound(lhs, rhs, cctx)
	}

	// Extrusion needed for RHS
	logger.Debug("constraint: extruding RHS for type-variable LHS", "rhs_level", rhs.level(), "lhs_level", lhs.level())
	extrudedRhs := cs.extrude(rhs, lhs.level(), false) // Needs extrude implementation
	if extrudedRhs == nil {                            // Extrusion failed or reported error
		return true
	}
	return cs.rec(lhs, extrudedRhs, true, cctx, shadows) // Retry with extruded RHS

	// Levels match or RHS is lower: Add upper bound
}

// constrainTypeVarRhs handles LHS <: TypeVariable
func (cs *constraintSolver) constrainTypeVarRhs(
	lhs SimpleType,
	rhs *typeVariable,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	// Levels match or LHS is lower: Add lower bound
	if lhs.level() <= rhs.level() {
		return cs.addLowerBound(rhs, lhs, cctx)
	}
	// Extrusion needed for LHS
	fmt.Printf(" -> Extruding LHS for TV RHS (%d > %d)\n", lhs.level(), rhs.level())
	extrudedLhs := cs.extrude(lhs, rhs.level(), true) // Needs extrude implementation
	if extrudedLhs == nil {                           // Extrusion failed or reported error
		return true
	}
	return cs.rec(extrudedLhs, rhs, true, cctx, shadows) // Retry with extruded LHS

}

// constrainFuncFunc handles `FunctionType <: FunctionType`
func (cs *constraintSolver) constrainFuncFunc(
	lhs, rhs funcType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	// Check arity
	if len(lhs.args) != len(rhs.args) {
		return cs.reportError(fmt.Sprintf("function arity mismatch: %d vs %d", len(lhs.args), len(rhs.args)), lhs, rhs, cctx)
	}

	// constrain arguments contravariantly: rhs.arg <: lhs.arg
	for i := range lhs.args {
		if cs.rec(rhs.args[i], lhs.args[i], false, cctx, shadows) { // Note: sameLevel = false
			return true // Terminate early
		}
	}

	// constrain return type covariantly: lhs.ret <: rhs.ret
	return cs.rec(lhs.ret, rhs.ret, false, cctx, shadows) // Note: sameLevel = false
}

// constrainTupleTuple handles `TupleType <: TupleType`
func (cs *constraintSolver) constrainTupleTuple(
	lhs, rhs tupleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	if len(lhs.fields) != len(rhs.fields) {
		return cs.reportError(fmt.Sprintf("tuple size mismatch: %d vs %d", len(lhs.fields), len(rhs.fields)), lhs, rhs, cctx)
	}

	// constrain fields covariantly: lhs.field <: rhs.field
	// TODO: Handle named tuples correctly if names differ.
	// TODO: Handle bounds correctly (recLb in Scala).
	for i := range lhs.fields {
		if cs.rec(lhs.fields[i], rhs.fields[i], false, cctx, shadows) { // Note: sameLevel = false
			return true // Terminate early
		}
	}
	return false
}

// constrainNamedTupleNamedTuple handles `NamedTupleType <: NamedTupleType`
func (cs *constraintSolver) constrainNamedTupleNamedTuple(
	lhs, rhs namedTupleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	if len(lhs.fields) != len(rhs.fields) {
		return cs.reportError(fmt.Sprintf("named tuple size mismatch: %d vs %d", len(lhs.fields), len(rhs.fields)), lhs, rhs, cctx)
	}

	// Check names and constrain fields covariantly
	// TODO: Handle bounds correctly (recLb in Scala).
	for i := range lhs.fields {
		// Assuming fields are ordered or using a map lookup
		if lhs.fields[i].Fst.Name != rhs.fields[i].Fst.Name {
			return cs.reportError(fmt.Sprintf("named tuple field name mismatch: '%s' vs '%s'", lhs.fields[i].Fst.Name, rhs.fields[i].Fst.Name), lhs, rhs, cctx)
		}
		if cs.rec(lhs.fields[i].Snd, rhs.fields[i].Snd, false, cctx, shadows) { // Note: sameLevel = false
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
		expandedLhs := cs.ctx.expand(lhs)
		expandedRhs := cs.ctx.expand(rhs)
		if expandedLhs.Equivalent(lhs) && expandedRhs.Equivalent(rhs) { // Avoid infinite loop
			// Check structural subtyping via tags if possible (Scala: mkClsTag)
			// Or report error
			return cs.reportError(fmt.Sprintf("type definition mismatch: %s vs %s", lhs.defName, rhs.defName), lhs, rhs, cctx)
		}
		return cs.rec(expandedLhs, expandedRhs, true, cctx, shadows)
	}

	// Same definition, check type arguments based on variance
	if len(lhs.typeArgs) != len(rhs.typeArgs) {
		// Should not happen if defName is the same and definitions are consistent
		return cs.reportError("type argument count mismatch", lhs, rhs, cctx)
	}

	// Fetch variance info for the definition (needs Ctx support)
	variances, ok := cs.ctx.getTypeDefinitionVariances(lhs.defName) // Needs implementation
	if !ok {
		return cs.reportError(fmt.Sprintf("unknown type definition %s", lhs.defName), lhs, rhs, cctx)
	}
	if len(variances) != len(lhs.typeArgs) {
		return cs.reportError(fmt.Sprintf("variance info mismatch for %s", lhs.defName), lhs, rhs, cctx)
	}

	for i, variance := range variances {
		targLhs := lhs.typeArgs[i]
		targRhs := rhs.typeArgs[i]
		switch variance {
		case Covariant: // targLhs <: targRhs
			if cs.rec(targLhs, targRhs, false, cctx, shadows) {
				return true
			}
		case Contravariant: // targRhs <: targLhs
			if cs.rec(targRhs, targLhs, false, cctx, shadows) {
				return true
			}
		case Invariant: // targLhs =:= targRhs (constrain both ways)
			if cs.rec(targLhs, targRhs, false, cctx, shadows) {
				return true
			}
			if cs.rec(targRhs, targLhs, false, cctx, shadows) {
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
		fldName := field.Fst
		fldTy := field.Snd // Assuming FieldType exists with lb, ub

		// Lookup field in the class definition (needs Ctx support)
		// This involves getting the class info, handling type parameters, freshening etc.
		// Simplified lookup:
		memberTy, err := cs.ctx.LookupField(lhs, fldName) // Needs implementation
		if err != nil {
			return cs.reportError(fmt.Sprintf("class %s has no field %s required by record", className, fldName.Name), lhs, rhs, cctx)
		}

		// constrain upper bounds: memberTy.ub <: fldTy.ub
		if cs.rec(memberTy.ub, fldTy.upperBound, false, cctx, shadows) {
			return true
		}

		// constrain lower bounds: fldTy.lb <: memberTy.lb (recLb)
		if fldTy.lowerBound != nil {
			if memberTy.lb == nil {
				// Trying to assign to a non-mutable field
				return cs.reportError(fmt.Sprintf("field %s is not mutable", fldName.Name), lhs, rhs, cctx)
			}
			if cs.rec(fldTy.lowerBound, memberTy.lb, false, cctx, shadows) {
				return true
			}
		}
	}
	return false
}

// --- Placeholders for Complex Logic ---

// goToWork handles complex constraints using DNF/CNF normalization.
// This is a major piece requiring its own implementation based on NormalForms.scala.
func (cs *constraintSolver) goToWork(
	lhs, rhs SimpleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	opsDnf := &opsDNF{ctx: cs.ctx}
	cs.constrainDNF(opsDnf, opsDnf.mkDeep(lhs, true), opsDnf.mkDeep(rhs, false), cctx, shadows)
	return false
}

// constrainDNF handles constraining when types have been converted to DNF.
// This corresponds to the logic within goToWork after normalization in the scala reference.
func (cs *constraintSolver) constrainDNF(ops *opsDNF, lhs, rhs dnf, cctx constraintContext, shadows *shadowsState) {
	logger.Debug("constrain: for DNF", "lhs", lhs, "rhs", rhs)
	cs.annoyingCalls++

	// Iterate through each conjunct C in the LHS DNF (LHS = C1 | C2 | ...)
	// We need to ensure C <: RHS for all C.
	for _, conj := range lhs {
		if !conj.vars.Empty() {
			// Case 1: The conjunct has positive variables (TV & L & ~R & ~N <: RHS)
			// Strategy: Extract one variable TV and constrain TV <: (RHS | ~(L & ~R & ~N))
			first := util.IterFirstOrPanic(conj.vars.Items())
			single := set.TreeSetFrom[*typeVariable]([]*typeVariable{first}, compareTypeVars)
			newC := conjunct{
				lhs:   conj.lhs,
				rhs:   conj.rhs,
				vars:  conj.vars.Difference(single),
				nvars: conj.nvars,
			}
			newRhs := unionOf(rhs.toType(), negateType(newC.toType(), emptyProv), unionOpts{})
			cs.rec(first, newRhs, true, cctx, shadows)
		} else {
			// Case 2: The conjunct C has no positive variables (L & ~R & ~N <: RHS)

			nvarsAsDNF := util.MapIter(conj.nvars.Items(), func(nvar *typeVariable) dnf {
				return ops.mkDeep(nvar, true)
			})
			fullRhs := ops.or(rhs, ops.mkDeep(conj.rhs.toType(), false))
			for nvar := range nvarsAsDNF {
				fullRhs = ops.or(fullRhs, nvar)
			}

			logger.Debug(fmt.Sprintf("constrainDNF: considering %s <: %s", conj.lhs, fullRhs))

			lnf := conj.lhs

			possibleConjuncts := make([]conjunct, 0, len(fullRhs))
			for _, rConj := range fullRhs {
				_, isBot := rConj.rhs.(rhsBot)
				// Early exit check (corresponds to Scala's `if ((r.rnf is RhsBot)...)`)
				if isBot && rConj.vars.Empty() && rConj.nvars.Empty() {
					// If rConj is just an LHS part (rConj.lhs)
					if lnf.lessThanOrEqual(rConj.lhs) { // Check if lnf <: rConj.lhs
						logger.Debug("constrainDNF: Early exit", "lnf", lnf, "rConj.lhs", rConj.lhs)
						goto nextLhsConjunct // Skip to the next conjunct in the outer loop (lhs)
					}
				}

				// Filtering logic (corresponds to Scala's `filter` conditions)
				// 1. `!vars.exists(r.nvars)` - Always true here as conj.vars is empty.
				// 2. `((lnf & r.lnf)).isDefined` - Check if intersection is possible.
				_, ok := lnf.and(rConj.lhs, cs.ctx)

				if !ok {
					logger.Debug("constrainDNF: Filtered (Lnf intersection failed)", "lnf", lnf, "rConj.lhs", rConj.lhs.String())
					continue
				}

				// 3. Tag checks (simplified version)
				if ok := checkTagCompatibility(lnf, rConj.rhs); !ok {
					logger.Debug("constrainDNF: Filtered (Tag incompatibility) !<:", "lnf", lnf, "rConj.rhs", rConj.rhs.String())
					continue // Skip this rConj
				}

				// If all checks pass, add to possible conjuncts
				possibleConjuncts = append(possibleConjuncts, rConj)
			}
			logger.Debug("constrainDNF: possible conjuncts", "possibleConjuncts", possibleConjuncts)
			possibleAsTypes := make([]SimpleType, 0, len(possibleConjuncts))
			for _, rConj := range possibleConjuncts {
				possibleAsTypes = append(possibleAsTypes, rConj.toType())
			}
			cs.annoying(cctx, nil, lnf, possibleAsTypes, &rhsBot{})
			panic("TODO implement rest of constrainDNF")

		} // End of else block (Case 2)

	nextLhsConjunct: // Label for the early exit goto
	} // End of loop through lhs conjuncts
}

func (cs *constraintSolver) annoying(cctx constraintContext, leftTypes []SimpleType, doneLeft lhsNF, rightTypes []SimpleType, doneRight rhsNF) {
	logger.Debug("constrain: annoying", "leftTypes", leftTypes, "doneLeft", doneLeft, "rightTypes", rightTypes, "doneRight", doneRight)
	cs.annoyingCalls++

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
			if ct, ok := rhsTag.(classTag); ok && ct.Equivalent(lhsRefined.base) {
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
		newFields := make([]util.Pair[ast.Var, fieldType], len(t.fields))
		for i, field := range t.fields {
			// Extrude field bounds: lb is contravariant (!pol), ub is covariant (pol)
			extrudedLb := cs.extrude(field.Snd.lowerBound, targetLvl, !pol)
			if extrudedLb == nil {
				return nil
			}
			extrudedUb := cs.extrude(field.Snd.upperBound, targetLvl, pol)
			if extrudedUb == nil {
				return nil
			}
			newFields[i] = util.Pair[ast.Var, fieldType]{
				Fst: field.Fst,
				Snd: fieldType{lowerBound: extrudedLb, upperBound: extrudedUb, withProvenance: withProvenance{field.Snd.prov()}},
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
		newFields := make([]util.Pair[ast.Var, SimpleType], len(t.fields))
		for i, field := range t.fields {
			extrudedField := cs.extrude(field.Snd, targetLvl, pol)
			if extrudedField == nil {
				return nil
			}
			newFields[i] = util.Pair[ast.Var, SimpleType]{Fst: field.Fst, Snd: extrudedField}
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
			logger.Warn("extrude: unknown type definition, cannot extrude TypeRef arguments", "typeRef", t)
			return t // Or return nil and report error?
		}
		if len(variances) != len(t.typeArgs) {
			logger.Warn("extrude: variance/type argument mismatch", "typeRef", t)
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
	if tag1.Equivalent(tag2) {
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
func (ctx *TypeCtx) LookupField(classType classTag, fieldName ast.Var) (*FieldType, error) {
	// TODO: Implement based on Scala's lookupField/getFieldType.
	// Needs access to type definitions (ctx.tyDefs/tyDefs2).
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
