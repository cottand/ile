package ir


func hasTypeVars(t Type) bool {
	switch ty := t.(type) {
	case *TypeVar:
		return true
	case *FnType:
		for _, arg := range ty.Args {
			if arg != nil && hasTypeVars(arg) {
				return true
			}
		}
		return ty.Return != nil && hasTypeVars(ty.Return)
	case *UnionType:
		return hasTypeVars(ty.Left) || hasTypeVars(ty.Right)
	case *IntersectionType:
		return hasTypeVars(ty.Left) || hasTypeVars(ty.Right)
	case *ConstrainedType:
		return hasTypeVars(ty.Base)
	case *AppliedType:
		for _, arg := range ty.Args {
			if hasTypeVars(arg) {
				return true
			}
		}
		return false
	case *ListLiteralType:
		for _, elem := range ty.ElementTypes {
			if hasTypeVars(elem) {
				return true
			}
		}
		return false
	case *NegType:
		return hasTypeVars(ty.Underlying)
	case *TypeBounds:
		return hasTypeVars(ty.Lower) || hasTypeVars(ty.Upper)
	default:
		return false
	}
}
