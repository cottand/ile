package types

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestGetVarsPolFor(t *testing.T) {
	ctx := NewEmptyTypeCtx()

	// Create type variables for testing
	tv1 := ctx.newTypeVariable(typeProvenance{}, "tv1", nil, nil)
	tv2 := ctx.newTypeVariable(typeProvenance{}, "tv2", nil, nil)

	// Test cases
	testCases := []struct {
		name     string
		typ      SimpleType
		pol      polarity
		expected map[TypeVarID]polarity
	}{
		{
			name: "Single type variable with positive polarity",
			typ:  tv1,
			pol:  positive,
			expected: map[TypeVarID]polarity{
				tv1.id: positive,
			},
		},
		{
			name: "Single type variable with negative polarity",
			typ:  tv1,
			pol:  negative,
			expected: map[TypeVarID]polarity{
				tv1.id: negative,
			},
		},
		{
			name: "Function type with type variables",
			typ: funcType{
				args: []SimpleType{tv1},
				ret:  tv2,
			},
			pol: positive,
			expected: map[TypeVarID]polarity{
				tv1.id: negative, // Argument is contravariant
				tv2.id: positive, // Return type is covariant
			},
		},
		{
			name: "Function type with type variables and negative polarity",
			typ: funcType{
				args: []SimpleType{tv1},
				ret:  tv2,
			},
			pol: negative,
			expected: map[TypeVarID]polarity{
				tv1.id: positive, // Argument is contravariant, but outer polarity is negative
				tv2.id: negative, // Return type is covariant, but outer polarity is negative
			},
		},
		{
			name: "Union type with type variables",
			typ: unionType{
				lhs: tv1,
				rhs: tv2,
			},
			pol: positive,
			expected: map[TypeVarID]polarity{
				tv1.id: positive,
				tv2.id: positive,
			},
		},
		{
			name: "Intersection type with type variables",
			typ: intersectionType{
				lhs: tv1,
				rhs: tv2,
			},
			pol: positive,
			expected: map[TypeVarID]polarity{
				tv1.id: positive,
				tv2.id: positive,
			},
		},
		{
			name: "Negation type with type variable",
			typ: negType{
				negated: tv1,
			},
			pol: positive,
			expected: map[TypeVarID]polarity{
				tv1.id: negative, // Negation inverts polarity
			},
		},
		{
			name: "Type variable appearing in both positive and negative positions",
			typ: funcType{
				args: []SimpleType{tv1},
				ret:  tv1,
			},
			pol: positive,
			expected: map[TypeVarID]polarity{
				tv1.id: invariant, // Appears in both positions, so it's invariant
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Convert the iterator result to a map for comparison with expected values
			result := make(map[TypeVarID]polarity)
			for tv, pol := range ctx.getVarsPolFor(tc.typ, tc.pol) {
				result[tv.id] = pol
			}
			assert.Equal(t, tc.expected, result, "getVarsPolFor returned incorrect result")
		})
	}
}
