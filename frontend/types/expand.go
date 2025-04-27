package types

import (
	"fmt"
	"github.com/cottand/ile/frontend/ast"
	set "github.com/hashicorp/go-set/v3"
	"go/token"
	"slices"
)

// expanderState holds the state during the recursive expansion of a SimpleType to an ast.Type.
type expanderState struct {
	ctx          *TypeCtx
	bounds       map[TypeVarID]ast.TypeBounds // Stores non-trivial bounds discovered for type variables.
	seenVars     *set.Set[TypeVarID]          // Tracks visited type variables to prevent infinite loops during bound expansion.
	stopAtTyVars bool                         // If true, stops expansion at type variables.
	// Store the created ast.TypeVar nodes to easily associate bounds later
	createdVars map[TypeVarID]*ast.TypeVar
}

// GetAstTypeFor converts a SimpleType into its corresponding ast.Type representation,
// simplifying it first.
func (ctx *TypeCtx) GetAstTypeFor(t SimpleType) ast.Type {
	// Note: The Scala version calls uninstantiatedBody() first.
	uninstantiated := t.uninstantiatedBody()
	simple := ctx.simplifyPipeline(uninstantiated) // Simplify first
	expanded := ctx.expandSimpleType(simple, false)
	logger.Info("expanded type", "simpleType", t, "simplifiedBounds", boundsString(simple), "expanded", expanded.ShowIn(ast.DumbShowCtx, 0))
	return expanded
}

// expandSimpleType converts a simplified SimpleType into its ast.Type representation.
// It handles type variables, bounds, and recursive structures.
func (ctx *TypeCtx) expandSimpleType(t SimpleType, stopAtTyVars bool) ast.Type {
	state := &expanderState{
		ctx:          ctx,
		bounds:       make(map[TypeVarID]ast.TypeBounds),
		seenVars:     set.New[TypeVarID](0), // Initialize the set
		stopAtTyVars: stopAtTyVars,
		createdVars:  make(map[TypeVarID]*ast.TypeVar),
	}

	res := state.expandRec(t)

	// If any bounds were collected, wrap the result in ast.ConstrainedType
	if len(state.bounds) > 0 {
		constraints := make([]ast.ConstrainedEntry, 0, len(state.bounds))

		// Use the createdVars map to associate bounds with the correct ast.TypeVar nodes
		for id, bnds := range state.bounds {
			tv, found := state.createdVars[id]
			if !found {
				// This shouldn't happen if createdVars is populated correctly
				panic(fmt.Sprintf("internal error: could not find ast.TypeVar for bound variable ID %d", id))
			}
			// Ensure bnds is a pointer if ConstrainedEntry expects *TypeBounds
			boundsPtr := bnds
			constraints = append(constraints, ast.ConstrainedEntry{Var: tv, Bounds: &boundsPtr})
		}

		// Sort constraints for deterministic output
		slices.SortFunc(constraints, func(a, b ast.ConstrainedEntry) int {
			// Sort by TypeVar UID for consistency
			// Assuming ast.TypeVar has a comparable UID field (using Identifier for now)
			if a.Var.Identifier < b.Var.Identifier {
				return -1
			}
			if a.Var.Identifier > b.Var.Identifier {
				return 1
			}
			return 0
		})

		// Use the range of the original type for the constrained type
		// If the result has a valid range, use that, otherwise fallback to original
		resRange := ast.RangeOf(res)
		if resRange.Pos() == token.NoPos {
			resRange = t.prov().Range // Fallback to original SimpleType range
		}

		return &ast.ConstrainedType{ // Assuming ast.ConstrainedType exists
			Base:  res,
			Where: constraints,
			Range: resRange,
		}
	}

	return res
}

// expandRec is the recursive helper for expandSimpleType.
func (st *expanderState) expandRec(t SimpleType) ast.Type {
	// Unwrap provenance wrappers transparently
	if pt, ok := t.(wrappingProvType); ok {
		// Keep the outer provenance range if the inner one is invalid
		innerProv := pt.underlying().prov()
		res := st.expandRec(pt.underlying())
		resRange := res // getCached current range

		// If result range is invalid, try using the wrapper's range
		if resRange.Pos() == token.NoPos && pt.proxyProvenance.Pos() != token.NoPos {
			//ast.SetRange(res, pt.prov().Range) // TODO: Need a SetRange equivalent for ast.Type
		} else if innerProv.Pos() == token.NoPos && pt.proxyProvenance.Pos() != token.NoPos {
			// If inner type had no provenance but wrapper did, use wrapper's
			//ast.SetRange(res, pt.prov().Range)
		}
		return res
	}

	prov := t.prov()
	rng := prov.Range

	switch ty := t.(type) {
	case *typeVariable:
		// Check if we already created the AST node for this var
		if existingTv, found := st.createdVars[ty.id]; found {
			// If we've seen it but haven't processed bounds yet (recursive case), return existing node
			// This handles recursive types where a variable might appear in its own bounds.
			if !st.seenVars.Contains(ty.id) {
				return existingTv
			}
			// If bounds already processed, just return the node
			return existingTv
		}

		// Create the AST node
		tv := &ast.TypeVar{
			Identifier: ty.String(), // Use the string representation as identifier? Or just UID?
			NameHint:   ty.nameHint, // Use hint if available
			// UID:   ty.id, // Assuming ast.TypeVar has UID
			Range: rng,
		}
		st.createdVars[ty.id] = tv // Store it

		if st.stopAtTyVars {
			return tv
		}

		// Check for recursion before processing bounds
		if st.seenVars.Contains(ty.id) {
			return tv // Already processing this var's bounds up the stack
		}
		st.seenVars.Insert(ty.id)       // Mark as seen *before* recursing on bounds
		defer st.seenVars.Remove(ty.id) // Remove after processing this branch

		// Expand lower bounds (union)
		var lb ast.Type = &ast.NothingType{Positioner: rng} // Default to Bot
		if len(ty.lowerBounds) > 0 {
			expandedLbs := make([]ast.Type, len(ty.lowerBounds))
			for i, bound := range ty.lowerBounds {
				expandedLbs[i] = st.expandRec(bound)
			}
			// Union of lower bounds
			currentLb := expandedLbs[0]
			for i := 1; i < len(expandedLbs); i++ {
				unionRange := ast.RangeBetween(currentLb, expandedLbs[i])
				currentLb = &ast.UnionType{Left: currentLb, Right: expandedLbs[i], Positioner: unionRange}
			}
			lb = currentLb
		}

		// Expand upper bounds (intersection)
		var ub ast.Type = &ast.AnyType{Positioner: rng} // Default to Top
		if len(ty.upperBounds) > 0 {
			expandedUbs := make([]ast.Type, len(ty.upperBounds))
			for i, bound := range ty.upperBounds {
				expandedUbs[i] = st.expandRec(bound)
			}
			// Intersection of upper bounds
			currentUb := expandedUbs[0]
			for i := 1; i < len(expandedUbs); i++ {
				interRange := ast.RangeBetween(currentUb, expandedUbs[i])
				currentUb = &ast.IntersectionType{Left: currentUb, Right: expandedUbs[i], Positioner: interRange}
			}
			ub = currentUb
		}

		// Check if bounds are non-trivial
		_, isBot := lb.(*ast.NothingType)
		_, isTop := ub.(*ast.AnyType)
		if !isBot || !isTop {
			boundsRange := ast.RangeBetween(lb, ub)
			st.bounds[ty.id] = ast.TypeBounds{Lower: lb, Upper: ub, Range: boundsRange}
		}
		return tv

	case funcType:
		args := make([]ast.Type, len(ty.args))
		for i, arg := range ty.args {
			args[i] = st.expandRec(arg)
		}
		ret := st.expandRec(ty.ret)
		funcRange := rng
		return &ast.FnType{
			Args:   args,
			Return: ret,
			Range:  funcRange,
		}
	//
	case unionType:
		lhs := st.expandRec(ty.lhs)
		rhs := st.expandRec(ty.rhs)
		return &ast.UnionType{
			Left:       lhs,
			Right:      rhs,
			Positioner: ast.RangeBetween(lhs, rhs),
		}

	case intersectionType:
		lhs := st.expandRec(ty.lhs)
		rhs := st.expandRec(ty.rhs)
		return &ast.IntersectionType{
			Left:       lhs,
			Right:      rhs,
			Positioner: ast.RangeBetween(lhs, rhs),
		}

	case recordType:
		panic("TODO: Implement expandRec for recordType")

	case tupleType:
		// Assuming ast.Tuple exists
		panic("TODO: Implement expandRec for tupleType - requires ast.Tuple")
		// fields := make([]ast.Type, len(ty.fields))
		// var firstFieldRange, lastFieldRange ast.Range
		// for i, f := range ty.fields {
		// 	fields[i] = st.expandRec(f)
		// 	// ... range calculation ...
		// }
		// tupRange := rng // Default
		// if len(fields) > 0 {
		// 	tupRange = ast.MergeRanges(firstFieldRange, lastFieldRange)
		// }
		// return &ast.Tuple{
		// 	Fields: fields,
		// 	Range:  tupRange,
		// }

	case namedTupleType:
		// Assuming ast.NamedTuple exists
		panic("TODO: Implement expandRec for namedTupleType - requires ast.NamedTuple")
		// fields := make([]util.Pair[ast.Var, ast.Type], len(ty.fields))
		// var firstFieldRange, lastFieldRange ast.Range
		// for i, f := range ty.fields {
		// 	fieldType := st.expandRec(f.Snd)
		// 	fields[i] = util.Pair[ast.Var, ast.Type]{
		// 		Fst: f.Fst,
		// 		Snd: fieldType,
		// 	}
		// 	// ... range calculation ...
		// }
		// tupRange := rng // Default
		// if len(fields) > 0 {
		// 	tupRange = ast.MergeRanges(firstFieldRange, lastFieldRange)
		// }
		// return &ast.NamedTuple{
		// 	Fields: fields,
		// 	Range:  tupRange,
		// }

	case arrayType:
		inner := st.expandRec(ty.innerT)
		// Create a synthetic TypeName for "Array"
		// Use original range as approximation, or maybe inner range?
		arrayTypeName := &ast.TypeName{Name: "Array", Positioner: rng}
		return &ast.AppliedType{
			Base:       *arrayTypeName,
			Args:       []ast.Type{inner},
			Positioner: ast.RangeBetween(arrayTypeName, inner),
		}

	case negType:
		// Assuming ast.Neg exists
		panic("TODO: Implement expandRec for negType - requires ast.Neg")
		// inner := st.expandRec(ty.negated)
		// return &ast.Neg{
		// 	Inner: inner,
		// 	Range: ast.MergeRanges(rng, inner.RangeOf()),
		// }

	case extremeType:
		if ty.polarity { // Bottom
			return &ast.NothingType{Positioner: rng}
		}
		// Top
		return &ast.AnyType{Positioner: rng}

	case classTag:
		// Use the range from the original tag ID
		tagRange := ast.RangeOf(ty.id)
		if tagRange.Pos() == token.NoPos {
			tagRange = rng // Fallback
		}

		switch id := ty.id.(type) {
		case *ast.Var:
			// Check if it's a primitive type name
			if st.ctx.IsPrimitive(id.Name) {
				return &ast.TypeName{Name: id.Name, Positioner: tagRange}
			}
			// It's a user-defined class tag
			return &ast.TypeTag{Name: id.Name, Positioner: tagRange} // Assuming ast.TypeTag has Name field
		case *ast.Literal: // Handle literal types
			// Convert the literal expression to ast.Literal type node
			copied := *id
			copied.Range = tagRange
			return &copied
		default:
			// Should not happen for valid classTags
			panic(fmt.Sprintf("unexpected id type in classTag: %T", ty.id))
		}

	case traitTag: // Similar to classTag but always results in TypeTag for Vars
		var tagRange ast.Positioner = ty.id
		if tagRange.Pos() == token.NoPos {
			tagRange = rng // Fallback
		}
		switch id := ty.id.(type) {
		case *ast.Var:
			return &ast.TypeTag{Name: id.Name, Positioner: tagRange} // Assuming ast.TypeTag has Name field
		case *ast.Literal:
			copied := *id
			copied.Range = ast.RangeOf(tagRange)
			return &copied
		default:
			panic(fmt.Sprintf("unexpected id type in traitTag: %T", ty.id))
		}

	case typeRef:
		targs := make([]ast.Type, len(ty.typeArgs))
		var firstArgRange, lastArgRange ast.Range
		for i, ta := range ty.typeArgs {
			// Check for wildcard case: Bounds(Bot, Top) -> ast.TypeBounds{Lb: Bot, Ub: Top}
			if tr, ok := ta.(typeRange); ok && isBottom(tr.lowerBound) && isTop(tr.upperBound) {
				wildcardRange := tr.prov().Range
				targs[i] = &ast.TypeBounds{ // Represent wildcard as Bounds(Bot, Top) in AST
					Lower: &ast.NothingType{Positioner: wildcardRange}, // Approx range
					Upper: &ast.AnyType{Positioner: wildcardRange},     // Approx range
					Range: wildcardRange,
				}
			} else {
				targs[i] = st.expandRec(ta)
			}

			if i == 0 {
				firstArgRange = ast.RangeOf(targs[i])
			}
			if i == len(ty.typeArgs)-1 {
				lastArgRange = ast.RangeOf(targs[i])
			}
		}
		// Create base TypeName using the TypeRef's range
		base := &ast.TypeName{Name: ty.defName, Positioner: rng}
		if len(targs) == 0 {
			return base // Just the type name if no arguments
		}
		// Calculate range for AppliedType
		appliedRange := ast.RangeBetween(base, firstArgRange)
		appliedRange = ast.RangeBetween(appliedRange, lastArgRange)

		return &ast.AppliedType{
			Base:       *base,
			Args:       targs,
			Positioner: appliedRange,
		}

	case typeRange:
		lb := st.expandRec(ty.lowerBound)
		ub := st.expandRec(ty.upperBound)
		return &ast.TypeBounds{
			Lower: lb,
			Upper: ub,
			Range: ast.Range{
				PosStart: lb.Pos(),
				PosEnd:   ub.Pos(),
			},
		}

	case *PolymorphicType:
		// This case should ideally not be hit if simplifyPipeline/uninstantiateBody was called first.
		// If it occurs, expand the body, but the level information is lost in the AST.
		fmt.Printf("Warning: Expanding PolymorphicType directly: %s\n", ty)
		// Return the expanded body, potentially losing the polymorphic nature in the AST representation.
		return st.expandRec(ty.Body)

	default:
		panic(fmt.Sprintf("unhandled SimpleType case in expandRec: %T", t))
	}
}

// expandFieldType converts a simple fieldType to an ast.Field.
func (st *expanderState) expandFieldType(ft fieldType) ast.Field {
	panic("TODO: Implement expandFieldType for record fields")
	// // Note: In Scala, FieldType's lb defaults to BotType. Here, it might be nil or bottomType.
	// var lb ast.Type
	// var lbRange ast.Range
	// // Check if lower bound is non-trivial (not nil and not bottomType)
	// if ft.lowerBound != nil && !isBottom(ft.lowerBound) {
	// 	lbExpanded := st.expandRec(ft.lowerBound)
	// 	lb = lbExpanded        // Assign the expanded type
	// 	lbRange = lb.RangeOf() // getCached its range
	// } else {
	// 	// If lb is nil or Bot, use field's provenance range as approximation for merging
	// 	lbRange = ft.prov().Range
	// 	// lb remains nil for ast.Field.In
	// }

	// ub := st.expandRec(ft.upperBound)
	// ubRange := ub.RangeOf()

	// fieldRange := ast.MergeRanges(lbRange, ubRange) // Calculate overall field range

	// // Create the ast.Field - Assuming ast.Field has In *ast.Type and Out ast.Type
	// var lbPtr *ast.Type
	// if lb != nil {
	// 	lbPtr = &lb // Take address only if lb is not nil (and not Bot)
	// }

	// return ast.Field{In: lbPtr, Out: ub, Positioner: fieldRange}
}

// IsPrimitive checks if a type name corresponds to a built-in primitive.
// TODO: Move this to TypeCtx and use the actual list of primitives.
func (ctx *TypeCtx) IsPrimitive(name string) bool {
	// Placeholder implementation - adapt based on your language's primitives
	switch name {
	case "Int", "Number", "Bool", "String", "Unit", // Base types
		ast.AnyTypeName, ast.NothingTypeName, // Top/Bottom
		"True", "False", // Bool literals treated as types
		"Undefined", "Null", // Common JS/TS-like concepts
		"Object", "Array", "Error": // Common structural/runtime types
		return true
	default:
		return false
	}
}
