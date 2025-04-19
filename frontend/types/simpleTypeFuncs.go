package types

import (
	"github.com/cottand/ile/util"
	"github.com/hashicorp/go-set"
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
	found := set.New[*typeVariable](1)
	remaining := []SimpleType{t}
	for {
		if len(remaining) == 0 {
			break
		}
		first := remaining[0]
		rest := remaining[1:]

		typeVar, ok := first.(*typeVariable)
		if ok {
			if found.Contains(typeVar) {
				continue
			}
			found.Insert(typeVar)
			remaining = append(slices.Collect(typeVar.children(true)), rest...)
			continue
		}
		remaining = append(slices.Collect(first.children(true)), rest...)
	}
	return found.Slice()
}

func boundsString(t SimpleType) string {
	vars := getVariables(t)
	sb := strings.Builder{}
	for _, variable := range vars {
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
	}
	return sb.String()
}
