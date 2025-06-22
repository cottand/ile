package types

import (
	"cmp"
	"fmt"
	"github.com/cottand/ile/frontend/ir"
	set "github.com/hashicorp/go-set/v3"
	"go/token"
	"slices"
)

// expanderState holds the state during the recursive expansion of a SimpleType to an ast.Type.
type expanderState struct {
	ctx          *TypeCtx
	bounds       map[TypeVarID]ir.TypeBounds // Stores non-trivial bounds discovered for type variables.
	seenVars     *set.Set[TypeVarID]         // Tracks visited type variables to prevent infinite loops during bound expansion.
	stopAtTyVars bool                        // If true, stops expansion at type variables.
	// Store the created ast.TypeVar nodes to easily associate bounds later
	createdVars map[TypeVarID]*ir.TypeVar
}

// GetAstTypeFor converts a SimpleType into its corresponding ast.Type representation,
// simplifying it first.
func (ctx *TypeCtx) GetAstTypeFor(t SimpleType) ir.Type {
	// Note: The Scala version calls uninstantiatedBody() first.
	uninstantiated := t.uninstantiatedBody()
	simple := ctx.simplifyPipeline(uninstantiated) // Simplify first
	expanded := ctx.expandSimpleType(simple, false)
	logger.Info("expanded type", "simpleType", t, "simplifiedBounds", boundsString(simple), "expanded", expanded.ShowIn(ir.DumbShowCtx, 0))
	return expanded
}

// expandSimpleType converts a simplified SimpleType into its ast.Type representation.
// It handles type variables, bounds, and recursive structures.
func (ctx *TypeCtx) expandSimpleType(t SimpleType, stopAtTyVars bool) ir.Type {
	state := &expanderState{
		ctx:          ctx,
		bounds:       make(map[TypeVarID]ir.TypeBounds),
		seenVars:     set.New[TypeVarID](0), // Initialize the set
		stopAtTyVars: stopAtTyVars,
		createdVars:  make(map[TypeVarID]*ir.TypeVar),
	}

	res := state.expandRec(t)

	// If any bounds were collected, wrap the result in ast.ConstrainedType
	if len(state.bounds) > 0 {
		constraints := make([]ir.ConstrainedEntry, 0, len(state.bounds))

		// Use the createdVars map to associate bounds with the correct ast.TypeVar nodes
		for id, bnds := range state.bounds {
			tv, found := state.createdVars[id]
			if !found {
				// This shouldn't happen if createdVars is populated correctly
				panic(fmt.Sprintf("internal error: could not find ast.TypeVar for bound variable ID %d", id))
			}
			// Ensure bnds is a pointer if ConstrainedEntry expects *TypeBounds
			boundsPtr := bnds
			constraints = append(constraints, ir.ConstrainedEntry{Var: tv, Bounds: &boundsPtr})
		}

		// Sort constraints for deterministic output
		slices.SortFunc(constraints, func(a, b ir.ConstrainedEntry) int {
			if a.Var.Identifier == b.Var.Identifier {
				return cmp.Compare(a.Bounds.Hash(), b.Bounds.Hash())
			}
			return cmp.Compare(a.Var.Identifier, b.Var.Identifier)
		})

		// Use the range of the original type for the constrained type
		// If the result has a valid range, use that, otherwise fallback to original
		resRange := ir.RangeOf(res)
		if resRange.Pos() == token.NoPos {
			resRange = t.prov().Range // Fallback to original SimpleType range
		}

		return &ir.ConstrainedType{ // Assuming ast.ConstrainedType exists
			Base:  res,
			Where: constraints,
			Range: resRange,
		}
	}

	return res
}

// expandRec is the recursive helper for expandSimpleType.
func (st *expanderState) expandRec(t SimpleType) ir.Type {
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
		tv := &ir.TypeVar{
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
		var lb ir.Type = &ir.NothingType{Positioner: rng} // Default to Bot
		if len(ty.lowerBounds) > 0 {
			expandedLbs := make([]ir.Type, len(ty.lowerBounds))
			for i, bound := range ty.lowerBounds {
				expandedLbs[i] = st.expandRec(bound)
			}
			// Union of lower bounds
			currentLb := expandedLbs[0]
			for i := 1; i < len(expandedLbs); i++ {
				unionRange := ir.RangeBetween(currentLb, expandedLbs[i])
				currentLb = &ir.UnionType{Left: currentLb, Right: expandedLbs[i], Positioner: unionRange}
			}
			lb = currentLb
		}

		// Expand upper bounds (intersection)
		var ub ir.Type = &ir.AnyType{Positioner: rng} // Default to Top
		if len(ty.upperBounds) > 0 {
			expandedUbs := make([]ir.Type, len(ty.upperBounds))
			for i, bound := range ty.upperBounds {
				expandedUbs[i] = st.expandRec(bound)
			}
			// Intersection of upper bounds
			currentUb := expandedUbs[0]
			for i := 1; i < len(expandedUbs); i++ {
				interRange := ir.RangeBetween(currentUb, expandedUbs[i])
				currentUb = &ir.IntersectionType{Left: currentUb, Right: expandedUbs[i], Positioner: interRange}
			}
			ub = currentUb
		}

		// Check if bounds are non-trivial
		_, isBot := lb.(*ir.NothingType)
		_, isTop := ub.(*ir.AnyType)
		if !isBot || !isTop {
			boundsRange := ir.RangeBetween(lb, ub)
			st.bounds[ty.id] = ir.TypeBounds{Lower: lb, Upper: ub, Range: boundsRange}
		}
		return tv

	case funcType:
		args := make([]ir.Type, len(ty.args))
		for i, arg := range ty.args {
			args[i] = st.expandRec(arg)
		}
		ret := st.expandRec(ty.ret)
		funcRange := rng
		return &ir.FnType{
			Args:   args,
			Return: ret,
			Range:  funcRange,
		}
	//
	case unionType:
		lhs := st.expandRec(ty.lhs)
		rhs := st.expandRec(ty.rhs)
		// type should be normalised so True | False should happen but never False | True
		if Equal(lhs, ir.TrueType) && Equal(rhs, ir.FalseType) {
			return &ir.TypeName{
				Name:  ir.BoolTypeName,
				Range: ty.provenance.Range,
			}
		}
		return &ir.UnionType{
			Left:       lhs,
			Right:      rhs,
			Positioner: ir.RangeBetween(lhs, rhs),
		}

	case intersectionType:
		lhs := st.expandRec(ty.lhs)
		rhs := st.expandRec(ty.rhs)
		return &ir.IntersectionType{
			Left:       lhs,
			Right:      rhs,
			Positioner: ir.RangeBetween(lhs, rhs),
		}

	case recordType:
		record := &ir.RecordType{
			Fields: make([]ir.RecordField, 0, len(ty.fields)),
		}
		for _, field := range ty.fields {
			var in, out ir.Type
			expandedTypeVar := false
			if LBTypeVar, isLBTypeVar := field.type_.lowerBound.(*typeVariable); isLBTypeVar {
				if UPTypeVar, isUBTypeVar := field.type_.upperBound.(*typeVariable); isUBTypeVar && LBTypeVar.id == UPTypeVar.id {
					expandedTypeVar = true
					expanded := st.expandRec(LBTypeVar)
					in = expanded
					out = expanded
				}
			}
			// else
			if !expandedTypeVar {
				in = st.expandRec(field.type_.lowerBound)
				out = st.expandRec(field.type_.upperBound)
			}
			record.Fields = append(record.Fields, ir.RecordField{
				Name:  field.name,
				Type:  ir.FieldType{In: in, Out: out, Range: ty.prov().Range},
				Range: ty.prov().Range,
			})
		}
		return record
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
		// 	fieldType := st.expandRec(f.type_)
		// 	fields[i] = util.Pair[ast.Var, ast.Type]{
		// 		name: f.name,
		// 		type_: fieldType,
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
		arrayTypeName := &ir.TypeName{Name: "Array", Range: rng}
		return &ir.AppliedType{
			Base:       *arrayTypeName,
			Args:       []ir.Type{inner},
			Positioner: ir.RangeBetween(arrayTypeName, inner),
		}

	case negType:
		st.ctx.addFailure(fmt.Sprintf("failed to expand negated type %s", ty), ty.provenance)
		// Assuming ast.Neg exists
		panic("TODO: Implement expandRec for negType - requires ast.Neg")
		// inner := st.expandRec(ty.negated)
		// return &ast.Neg{
		// 	Inner: inner,
		// 	Range: ast.MergeRanges(rng, inner.RangeOf()),
		// }

	case extremeType:
		if ty.polarity { // Bottom
			return &ir.NothingType{Positioner: rng}
		}
		// Top
		return &ir.AnyType{Positioner: rng}

	case classTag:
		// Use the range from the original tag ID
		tagRange := ir.RangeOf(ty.id)
		if tagRange.Pos() == token.NoPos {
			tagRange = rng // Fallback
		}

		switch id := ty.id.(type) {
		case *ir.Var:
			// note - scala reference differentiates TypeName and TypeTag - we treat these
			// as interchangeable here.
			return &ir.TypeName{Name: id.Name, Range: tagRange}
		case *ir.Literal: // Handle literal types
			// Convert the literal expression to ast.Literal type node
			copied := *id
			copied.Range = tagRange
			return &copied
		default:
			// Should not happen for valid classTags
			panic(fmt.Sprintf("unexpected id type in classTag: %T", ty.id))
		}

	case traitTag: // Similar to classTag but always results in TypeTag for Vars
		var tagRange ir.Positioner = ty.id
		if tagRange.Pos() == token.NoPos {
			tagRange = rng // Fallback
		}
		switch id := ty.id.(type) {
		case *ir.Var:
			return &ir.TypeName{Name: id.Name, Range: ir.RangeOf(tagRange)} // Assuming ast.TypeTag has Name field
		case *ir.Literal:
			copied := *id
			copied.Range = ir.RangeOf(tagRange)
			return &copied
		default:
			panic(fmt.Sprintf("unexpected id type in traitTag: %T", ty.id))
		}

	case typeRef:
		targs := make([]ir.Type, len(ty.typeArgs))
		var firstArgRange, lastArgRange ir.Range
		for i, ta := range ty.typeArgs {
			// Check for wildcard case: Bounds(Bot, Top) -> ast.TypeBounds{Lb: Bot, Ub: Top}
			if tr, ok := ta.(typeRange); ok && isBottom(tr.lowerBound) && isTop(tr.upperBound) {
				wildcardRange := tr.prov().Range
				targs[i] = &ir.TypeBounds{ // Represent wildcard as Bounds(Bot, Top) in AST
					Lower: &ir.NothingType{Positioner: wildcardRange}, // Approx range
					Upper: &ir.AnyType{Positioner: wildcardRange},     // Approx range
					Range: wildcardRange,
				}
			} else {
				targs[i] = st.expandRec(ta)
			}

			if i == 0 {
				firstArgRange = ir.RangeOf(targs[i])
			}
			if i == len(ty.typeArgs)-1 {
				lastArgRange = ir.RangeOf(targs[i])
			}
		}
		// Create base TypeName using the TypeRef's range
		base := &ir.TypeName{Name: ty.defName, Range: rng}
		if len(targs) == 0 {
			return base // Just the type name if no arguments
		}
		// Calculate range for AppliedType
		appliedRange := ir.RangeBetween(base, firstArgRange)
		appliedRange = ir.RangeBetween(appliedRange, lastArgRange)

		return &ir.AppliedType{
			Base:       *base,
			Args:       targs,
			Positioner: appliedRange,
		}

	case typeRange:
		lb := st.expandRec(ty.lowerBound)
		ub := st.expandRec(ty.upperBound)
		return &ir.TypeBounds{
			Lower: lb,
			Upper: ub,
			Range: ir.Range{
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
func (st *expanderState) expandFieldType(ft fieldType) ir.FieldType {
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
	// 	// lb remains nil for ast.FieldType.In
	// }

	// ub := st.expandRec(ft.upperBound)
	// ubRange := ub.RangeOf()

	// fieldRange := ast.MergeRanges(lbRange, ubRange) // Calculate overall field range

	// // Create the ast.FieldType - Assuming ast.FieldType has In *ast.Type and Out ast.Type
	// var lbPtr *ast.Type
	// if lb != nil {
	// 	lbPtr = &lb // Take address only if lb is not nil (and not Bot)
	// }

	// return ast.FieldType{In: lbPtr, Out: ub, Range: fieldRange}
}
