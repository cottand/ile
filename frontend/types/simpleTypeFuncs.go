package types

import (
	"github.com/cottand/ile/util"
	"maps"
	"slices"
	"strings"
)

func unwrapProvenance(t SimpleType) SimpleType {
	if wrapped, isWrapped := t.(wrappingProvType); isWrapped {
		return unwrapProvenance(wrapped.SimpleType)
	}
	return t
}

func (ctx *TypeCtx) typeIsAliasOf(t SimpleType) bool {
	panic("TODO implement me")
}

func getVariables(t SimpleType) []*typeVariable {
	//found := set.New[*typeVariable](1)
	found := make(map[TypeVarID]*typeVariable)
	remaining := []SimpleType{t}
	for {
		if len(remaining) == 0 {
			break
		}
		first := remaining[0]
		rest := remaining[1:]

		typeVar, ok := first.(*typeVariable)
		if ok {
			if _, ok := found[typeVar.id]; ok {
				remaining = rest
				continue
			}
			found[typeVar.id] = typeVar
			remaining = append(slices.Collect(typeVar.children(true)), rest...)
			continue
		}
		remaining = append(slices.Collect(first.children(true)), rest...)
	}
	return slices.Collect(maps.Values(found))
}

func boundsString(t SimpleType) string {
	if t == nil {
		return "<nil>"
	}
	vars := getVariables(t)
	sb := strings.Builder{}
	for i, variable := range vars {
		if len(variable.lowerBounds) == 0 && len(variable.upperBounds) == 0 {
			continue
		}
		if len(variable.lowerBounds) > 0 {
			sb.WriteString(util.JoinString(variable.lowerBounds, " | "))
			sb.WriteString(" <: ")
		}
		sb.WriteString(variable.String())
		if len(variable.upperBounds) > 0 {
			sb.WriteString(" <: ")
			sb.WriteString(util.JoinString(variable.upperBounds, " & "))
		}
		if len(vars) > 1 && i < len(vars)-1 {
			sb.WriteString("; ")
		}
	}
	return sb.String()
}
