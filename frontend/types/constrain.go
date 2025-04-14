package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	"github.com/cottand/ile/frontend/ilerr"
	"go/token"
	"slices"
)

// constraintPair holds a pair of types being constrained.
type constraintPair struct {
	lhs SimpleType
	rhs SimpleType
}

// constraintContext tracks the chain of constraints for error reporting.
type constraintContext struct {
	lhsChain []SimpleType
	rhsChain []SimpleType
}

// constraintSolver holds the state for a single constrain call.
type constraintSolver struct {
	ctx   *TypeCtx                                       // Reference to the main typing context
	prov  *typeProvenance                                // Provenance of the top-level constraint
	onErr func(err ilerr.IleError) (terminateEarly bool) // Error callback
	level level                                          // Current polymorphism level during solving

	cache map[constraintPair]struct{} // Cache for subtyping checks
	fuel  int                         // Fuel to prevent infinite loops
	depth int                         // Current recursion depth
	stack []constraintPair            // Stack for tracking recursion depth and path

	// TODO: Implement Shadows for cycle detection
	// shadows shadowsState

	// TODO: Implement ExtrCtx for stashing constraints during extrusion
	// extrCtx map[typeVariableID][]*stashedConstraint

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
	prov *typeProvenance,
	onErr func(err ilerr.IleError) (terminateEarly bool),
) *constraintSolver {
	return &constraintSolver{
		ctx:   ctx,
		prov:  prov,
		onErr: onErr,
		level: ctx.level, // Start at the context's current level
		cache: make(map[constraintPair]struct{}),
		fuel:  defaultStartingFuel,
		depth: 0,
		stack: make([]constraintPair, 0, defaultDepthLimit),
		// Initialize shadows, extrCtx etc. if implemented
	}
}

// --- Helper methods for the solver ---

func (cs *constraintSolver) consumeFuel(currentLhs, currentRhs SimpleType, cctx constraintContext) bool {
	cs.fuel--
	cs.depth++
	if cs.depth > defaultDepthLimit {
		// Simplified error reporting
		err := ilerr.NewTypeError(
			fmt.Sprintf("Subtyping constraint %s <: %s exceeded recursion depth limit (%d)", currentLhs, currentRhs, defaultDepthLimit),
			cs.prov.positioner,
			nil, // TODO: Add more context like the stack trace
		)
		return cs.onErr(err) // Check if we should terminate
	}
	if cs.fuel <= 0 {
		err := ilerr.NewTypeError(
			fmt.Sprintf("Subtyping constraint %s <: %s ran out of fuel (%d)", currentLhs, currentRhs, defaultStartingFuel),
			cs.prov.positioner,
			nil, // TODO: Add context
		)
		return cs.onErr(err) // Check if we should terminate
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

	// Use the most relevant positioner - often the top-level one
	pos := cs.prov.positioner
	if lhsProv.positioner != nil && lhsProv.positioner.Pos() != token.NoPos {
		pos = lhsProv.positioner // Or maybe lhs? Needs refinement based on Scala logic
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
		ctx:            cs.ctx,       // Share the main context initially
		prov:           cs.prov,      // Inherit provenance
		onErr:          cs.onErr,     // Share error handler
		level:          cs.level + 1, // Increment level
		cache:          cs.cache,     // Share cache (or create a sub-cache?)
		fuel:           cs.fuel,      // Pass remaining fuel
		depth:          cs.depth,     // Inherit depth
		stack:          cs.stack,     // Share stack
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
func makeProxy(ty SimpleType, prov *typeProvenance) SimpleType {
	if prov == nil || prov.positioner == nil || prov.positioner.Pos() == token.NoPos {
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
	fmt.Printf("Adding UB %s to %s\n", rhs, tv)
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
	fmt.Printf("Adding LB %s to %s\n", lhs, tv)
	// Simplified: Add bound and propagate. Scala version uses mkProxy.
	// Need to handle potential duplicates and normalization.
	newBound := lhs // TODO: Apply makeProxy based on cctx
	tv.lowerBounds = append(tv.lowerBounds, newBound)

	// Propagate constraints: new_lhs <: U for all U in upperBounds
	for _, ub := range tv.upperBounds {
		if cs.rec(newBound, ub, true, cctx, nil /* TODO: shadows */) {
			return true // Terminate early if propagation fails
		}
	}
	return false
}

// Constrain enforces a subtyping relationship `lhs` <: `rhs`.
// It returns true if constraint solving should terminate early due to an error.
func (ctx *TypeCtx) Constrain(
	lhs, rhs SimpleType,
	prov *typeProvenance,
	onErr func(err ilerr.IleError) (terminateEarly bool),
) bool {
	solver := ctx.newConstraintSolver(prov, onErr)

	fmt.Printf("CONSTRAIN %s <! %s\n", lhs, rhs)
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
	pair := constraintPair{lhs, rhs}
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
		if len(cctx.lhsChain) == 0 || cctx.lhsChain[0] != lhs { // Avoid duplicates
			nextCctx.lhsChain = slices.Insert(cctx.lhsChain, 0, lhs)
		}
		if len(cctx.rhsChain) == 0 || cctx.rhsChain[0] != rhs { // Avoid duplicates
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

// recImpl contains the core subtyping logic based on type structure.
// Returns true if constraint solving should terminate early.
func (cs *constraintSolver) recImpl(
	lhs, rhs SimpleType,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	fmt.Printf("%d. C %s <! %s (fuel: %d)\n", cs.level, lhs, rhs, cs.fuel)

	// 1. Basic Equality Check (more robust check needed for recursive types)
	if cs.ctx.TypesEquivalent(lhs, rhs) {
		fmt.Println(" -> Trivial: Equivalent types")
		return false // Success
	}

	// 2. Cache Check
	pair := constraintPair{lhs, rhs}
	if _, found := cs.cache[pair]; found {
		fmt.Println(" -> Cached")
		return false // Success (already processed)
	}
	// TODO: Implement proper cycle detection using shadows here.
	// if shadows.detectCycle(lhs, rhs) { ... reportError ... return true }

	// Add to cache *after* cycle check
	cs.cache[pair] = struct{}{}
	// TODO: Update shadows state here.

	// 3. Unwrap Provenance Wrappers (like ProvType in Scala)
	if lhsWrap, ok := lhs.(wrappingProvType); ok {
		return cs.rec(lhsWrap.underlying(), rhs, true, cctx, shadows)
	}
	if rhsWrap, ok := rhs.(wrappingProvType); ok {
		return cs.rec(lhs, rhsWrap.underlying(), true, cctx, shadows)
	}

	// 4. Handle specific type combinations (Switch statement mirroring Scala's match)
	switch l := lhs.(type) {
	case extremeType: // Top or Bottom
		if l.polarity == true { // Bottom <: Anything
			return false // Success
		}
		// Top <: RHS only if RHS is Top
		if r, ok := rhs.(extremeType); ok && !r.polarity {
			return false // Top <: Top
		}
		// Fall through to report error or handle Top <: X via goToWork

	case *typeVariable:
		return cs.constrainTypeVarLhs(l, rhs, cctx, shadows)

	case funcType:
		if r, ok := rhs.(funcType); ok {
			return cs.constrainFuncFunc(l, r, cctx, shadows)
		}
		// funcType <: Other? -> goToWork or error

	case tupleType:
		if r, ok := rhs.(tupleType); ok {
			return cs.constrainTupleTuple(l, r, cctx, shadows)
		}
		if r, ok := rhs.(arrayType); ok {
			// Array subtyping: Tuple<T1,..Tn> <: Array<U> if (T1|...|Tn) <: U
			innerLhs := l.inner() // Needs implementation
			return cs.rec(innerLhs, r.innerT, false, cctx, shadows)
			// TODO: Handle bounds correctly (recLb in Scala)
		}
		// tupleType <: Other? -> goToWork or error

	case namedTupleType: // Similar to tupleType
		if r, ok := rhs.(namedTupleType); ok {
			return cs.constrainNamedTupleNamedTuple(l, r, cctx, shadows)
		}
		// namedTupleType <: Other? -> goToWork or error

	case arrayType:
		if r, ok := rhs.(arrayType); ok {
			// Array<T> <: Array<U> if T <: U (covariance)
			// TODO: Handle bounds correctly (recLb in Scala for mutable arrays)
			return cs.rec(l.innerT, r.innerT, false, cctx, shadows)
		}
		// arrayType <: Other? -> goToWork or error

	case intersectionType:
		// (L1 & L2) <: R  =>  L1 <: R AND L2 <: R
		if cs.rec(l.lhs, rhs, true, cctx, shadows) {
			return true
		}
		return cs.rec(l.rhs, rhs, true, cctx, shadows)

	case unionType:
		// L <: (R1 | R2) -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case negType:
		if r, ok := rhs.(negType); ok {
			// ~L <: ~R  =>  R <: L
			return cs.rec(r.negated, l.negated, true, cctx, shadows)
		}
		// ~L <: R -> Requires DNF/CNF (goToWork)
		return cs.goToWork(lhs, rhs, cctx, shadows)

	case typeRef:
		// Expand LHS and retry
		// Need a mechanism to prevent infinite expansion
		expandedLhs := cs.ctx.expand(l) // Assuming expand exists
		if expandedLhs == lhs {         // Avoid infinite loop if expansion didn't change anything
			// Fall through to handle TypeRef vs RHS
		} else {
			return cs.rec(expandedLhs, rhs, true, cctx, shadows)
		}
		// Handle TypeRef vs TypeRef (variance check) or TypeRef vs Other
		if r, ok := rhs.(typeRef); ok {
			return cs.constrainTypeRefTypeRef(l, r, cctx, shadows)
		}
		// Fall through

	case *PolymorphicType:
		// Instantiate LHS and retry
		// Need to manage levels correctly
		instantiatedLhs := l.instantiate(cs.level) // Instantiate at current solver level
		// TODO: Pass previous contexts (prevCctxs) if needed for extrusion reasons
		return cs.rec(instantiatedLhs, rhs, true, cctx, shadows)

	case typeRange: // Equivalent to TypeBounds in Scala
		// L..U <: R => U <: R
		return cs.rec(l.upperBound, rhs, true, cctx, shadows)

	case classTag:
		// Handle ClassTag <: RHS
		if rTag, ok := rhs.(objectTag); ok {
			// Check inheritance/equality
			if cs.ctx.IsSubtypeTag(l, rTag) { // Needs implementation
				return false // Success
			}
		}
		// Handle ClassTag <: RecordType (member checking)
		if rRec, ok := rhs.(recordType); ok { // Assuming recordType exists
			return cs.constrainClassRecord(l, rRec, cctx, shadows)
		}
		// Fall through

	case traitTag:
		// Handle TraitTag <: RHS
		if rTag, ok := rhs.(objectTag); ok {
			// Check inheritance/equality
			if cs.ctx.IsSubtypeTag(l, rTag) { // Needs implementation
				return false // Success
			}
		}
		// Fall through

		// TODO: Add cases for RecordType, other specific types as needed
	}

	// 5. Handle RHS structure (if LHS didn't match a specific case)
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
		if expandedRhs == rhs {
			// Fall through if expansion didn't change anything
		} else {
			return cs.rec(lhs, expandedRhs, true, cctx, shadows)
		}
		// Fall through

	case *PolymorphicType:
		// L <: forall V. R => Enter new level, rigidify R, and constrain L <: rigid R
		return cs.withSubLevel(func(subSolver *constraintSolver) bool {
			rigidRhs := r.rigidify(subSolver.level) // Rigidify at the *new* level
			fmt.Printf(" -> Rigidified RHS: %s\n", rigidRhs)
			// Constrain LHS against the rigidified RHS in the sub-level
			return subSolver.rec(lhs, rigidRhs, true, cctx, shadows) // Pass original cctx? Scala passes `true` for sameLevel
			// Unstashing happens automatically when withSubLevel returns (if implemented)
		})

	case typeRange: // Equivalent to TypeBounds in Scala
		// L <: L'..U' => L <: L'
		return cs.rec(lhs, r.lowerBound, true, cctx, shadows)

		// TODO: Add cases for RecordType on RHS, etc.
	}

	// 6. If no specific rule matched, report error or try complex solving
	fmt.Printf(" -> No specific rule for %T <: %T\n", lhs, rhs)
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
	if rhs.level() > lhs.level() {
		// Extrusion needed for RHS
		fmt.Printf(" -> Extruding RHS for TV LHS (%d > %d)\n", rhs.level(), lhs.level())
		extrudedRhs := cs.extrude(rhs, lhs.level(), false, cs.level /*?*/, cctx) // Needs extrude implementation
		if extrudedRhs == nil {                                                  // Extrusion failed or reported error
			return true
		}
		return cs.rec(lhs, extrudedRhs, true, cctx, shadows) // Retry with extruded RHS
	}

	// Levels match or RHS is lower: Add upper bound
	return cs.addUpperBound(lhs, rhs, cctx)
}

// constrainTypeVarRhs handles `Lhs <: TypeVariable`
func (cs *constraintSolver) constrainTypeVarRhs(
	lhs SimpleType,
	rhs *typeVariable,
	cctx constraintContext,
	shadows *shadowsState,
) bool {
	// Check levels
	if lhs.level() > rhs.level() {
		// Extrusion needed for LHS
		fmt.Printf(" -> Extruding LHS for TV RHS (%d > %d)\n", lhs.level(), rhs.level())
		extrudedLhs := cs.extrude(lhs, rhs.level(), true, cs.level /*?*/, cctx) // Needs extrude implementation
		if extrudedLhs == nil {                                                 // Extrusion failed or reported error
			return true
		}
		return cs.rec(extrudedLhs, rhs, true, cctx, shadows) // Retry with extruded LHS
	}

	// Levels match or LHS is lower: Add lower bound
	return cs.addLowerBound(rhs, lhs, cctx)
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

	// Constrain arguments contravariantly: rhs.arg <: lhs.arg
	for i := range lhs.args {
		if cs.rec(rhs.args[i], lhs.args[i], false, cctx, shadows) { // Note: sameLevel = false
			return true // Terminate early
		}
	}

	// Constrain return type covariantly: lhs.ret <: rhs.ret
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

	// Constrain fields covariantly: lhs.field <: rhs.field
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
		if expandedLhs == lhs && expandedRhs == rhs { // Avoid infinite loop
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
	variances, ok := cs.ctx.GetTypeDefinitionVariances(lhs.defName) // Needs implementation
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

		// Constrain upper bounds: memberTy.ub <: fldTy.ub
		if cs.rec(memberTy.ub, fldTy.ub, false, cctx, shadows) {
			return true
		}

		// Constrain lower bounds: fldTy.lb <: memberTy.lb (recLb)
		if fldTy.lb != nil {
			if memberTy.lb == nil {
				// Trying to assign to a non-mutable field
				return cs.reportError(fmt.Sprintf("field %s is not mutable", fldName.Name), lhs, rhs, cctx)
			}
			if cs.rec(fldTy.lb, memberTy.lb, false, cctx, shadows) {
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
	cs.annoyingCalls++
	fmt.Printf(" -> goToWork: %s <! %s (NOT IMPLEMENTED)\n", lhs, rhs)
	// 1. Convert lhs to DNF (Disjunctive Normal Form: union of conjunctions)
	//    lhsDNF := cs.normalize(lhs, true) // Needs implementation
	// 2. Convert rhs to CNF (Conjunctive Normal Form: intersection of disjunctions)
	//    or DNF with polarity false.
	//    rhsDNF := cs.normalize(rhs, false) // Needs implementation
	// 3. Handle polymorphism in rhsDNF (rigidification) if necessary.
	// 4. Call constrainDNF(lhsDNF, rhsDNF)
	// return cs.constrainDNF(lhsDNF, rhsDNF, cctx, shadows)

	// Placeholder: report error
	return cs.reportError("complex constraint solving (DNF/CNF) not implemented", lhs, rhs, cctx)
}

// extrude handles type variable extrusion. Complex logic based on levels and polarity.
// Needs careful implementation matching Scala's `extrude`.
func (cs *constraintSolver) extrude(
	ty SimpleType,
	lowerLvl int,
	pol bool,               // Polarity: true for positive, false for negative
	upperLvl level,         // Upper level limit for extrusion
	cctx constraintContext, // Used for provenance/reason in Extruded type
) SimpleType {
	fmt.Printf(" -> extrude: %s (level %d) below %d, pol: %t (NOT IMPLEMENTED)\n", ty, ty.level(), lowerLvl, pol)
	// 1. Check base case: ty.level <= lowerLvl -> return ty
	// 2. Handle recursion using a cache (map[typeVariableID][bool]typeVariableID ?)
	// 3. Recursively extrude children based on type structure and polarity.
	// 4. Handle TypeVariable:
	//    - If tv.level > upperLvl: Copy the variable (freshen).
	//    - If tv.level > lowerLvl: Extrude the variable:
	//        - Create a new variable `nv` at `lowerLvl`.
	//        - Add `nv` to the bounds of the original `tv`.
	//        - Extrude the bounds of `tv` and add them to `nv`.
	//        - Return `nv`.
	// 5. Handle SkolemTag/RigidVar extrusion (widen to Top/Bottom or use Extruded type).
	// 6. Return the extruded type.

	// Placeholder: return original type (incorrect)
	// A real implementation needs to handle level changes and bound updates.
	// Returning nil could signal an error during extrusion.
	return ty
}

// --- Type Definition Lookup Helpers (Need implementation in TypeCtx) ---

// GetTypeDefinitionVariances retrieves variance information for type parameters.
func (ctx *TypeCtx) GetTypeDefinitionVariances(name typeName) ([]Variance, bool) {
	// TODO: Implement lookup in ctx.tyDefs or ctx.tyDefs2
	fmt.Printf("WARN: GetTypeDefinitionVariances not implemented for %s\n", name)
	// Placeholder: Assume invariant for now
	def, ok := ctx.typeDefs[name] // Assuming tyDefs stores this info
	if !ok {
		return nil, false
	}
	variances := make([]Variance, len(def.tparams))
	for i := range variances {
		variances[i] = Invariant // Default to invariant
		// TODO: Read actual variance from TypeDef
	}
	return variances, true
}

// IsSubtypeTag checks if tag1 is a subtype of tag2 (class/trait inheritance).
func (ctx *TypeCtx) IsSubtypeTag(tag1, tag2 objectTag) bool {
	// TODO: Implement based on class/trait definitions and parent information.
	fmt.Printf("WARN: IsSubtypeTag not implemented for %s <: %s\n", tag1, tag2)
	if tag1.Equivalent(tag2) {
		return true
	}
	// Check parent hierarchy
	if c1, ok := tag1.(classTag); ok {
		// Check c1.parents against tag2.id
		// Recursively check parents of parents
	}
	// Similar logic if tag1 is traitTag
	return false // Placeholder
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
	prov *typeProvenance
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
