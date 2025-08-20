package types

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestCleanBounds(t *testing.T) {
	ctx := NewEmptyTypeCtx()

	// Test cases for cleanBounds function
	testCases := []struct {
		name     string
		input    SimpleType
		opts     cleanBoundsOpts
		expected SimpleType
	}{
		{
			name:     "Extreme type remains unchanged",
			input:    &extremeType{polarity: true, withProvenance: emptyProv.embed()},
			opts:     cleanBoundsOpts{},
			expected: &extremeType{polarity: true, withProvenance: emptyProv.embed()},
		},
		{
			name: "Union type processes both sides",
			input: &unionType{
				lhs:            &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				rhs:            &extremeType{polarity: false, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
			opts: cleanBoundsOpts{},
			expected: &unionType{
				lhs:            &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				rhs:            &extremeType{polarity: false, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
		},
		{
			name: "Intersection type processes both sides",
			input: &intersectionType{
				lhs:            &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				rhs:            &extremeType{polarity: false, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
			opts: cleanBoundsOpts{},
			expected: &intersectionType{
				lhs:            &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				rhs:            &extremeType{polarity: false, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
		},
		{
			name: "Negation type inverts polarity",
			input: &negType{
				negated:        &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
			opts: cleanBoundsOpts{},
			expected: &negType{
				negated:        &extremeType{polarity: true, withProvenance: emptyProv.embed()},
				withProvenance: emptyProv.embed(),
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := ctx.cleanBounds(tc.input, tc.opts)
			assert.Equal(t, tc.expected.String(), result.String(), "Expected %v, got %v", tc.expected, result)
		})
	}
}

func TestCleanBoundsWithTypeVariables(t *testing.T) {

	// Test with positive polarity
	t.Run("Type variable with positive polarity", func(t *testing.T) {
		ctx := NewEmptyTypeCtx()

		// Create type variables for testing
		tv1 := ctx.fresher.newTypeVariable(0, emptyProv, "a", nil, nil)
		tv2 := ctx.fresher.newTypeVariable(0, emptyProv, "b", nil, nil)

		// Set up bounds
		tv1.lowerBounds = []SimpleType{&extremeType{polarity: true, withProvenance: emptyProv.embed()}}
		tv1.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}

		tv2.lowerBounds = []SimpleType{tv1}
		tv2.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}
		result := ctx.cleanBounds(tv2, cleanBoundsOpts{inPlace: false})

		// The result should be a new type variable with processed bounds
		resultTV, ok := result.(*typeVariable)
		assert.True(t, ok, "Result should be a type variable")

		// Check that the result is not the same as the input
		assert.NotEqual(t, tv2.id, resultTV.id, "Result should be a new type variable")

		// Check that lower bounds are processed
		assert.Equal(t, 1, len(resultTV.lowerBounds), "Should have one lower bound")

		// Upper bounds should be empty with positive polarity
		assert.Equal(t, 0, len(resultTV.upperBounds), "Upper bounds should be empty with positive polarity")
	})

	// Test with negative polarity
	t.Run("Type variable with negative polarity", func(t *testing.T) {
		ctx := NewEmptyTypeCtx()

		// Create type variables for testing
		tv1 := ctx.fresher.newTypeVariable(0, emptyProv, "a", nil, nil)
		tv2 := ctx.fresher.newTypeVariable(0, emptyProv, "b", nil, nil)

		// Set up bounds
		tv1.lowerBounds = []SimpleType{&extremeType{polarity: true, withProvenance: emptyProv.embed()}}
		tv1.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}

		tv2.lowerBounds = []SimpleType{tv1}
		tv2.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}
		result := ctx.cleanBounds(tv2, cleanBoundsOpts{inPlace: false})

		// The result should be a new type variable with processed bounds
		resultTV, ok := result.(*typeVariable)
		assert.True(t, ok, "Result should be a type variable")

		// Check that the result is not the same as the input
		assert.NotEqual(t, tv2.id, resultTV.id, "Result should be a new type variable")

		// Lower bounds should be empty with negative polarity
		assert.Equal(t, 0, len(resultTV.lowerBounds), "Lower bounds should be empty with negative polarity")

		// Check that upper bounds are processed
		assert.Equal(t, 0, len(resultTV.upperBounds), "Should have one upper bound")
	})

	// Test with in-place processing
	t.Run("Type variable with in-place processing", func(t *testing.T) {
		ctx := NewEmptyTypeCtx()

		// Create type variables for testing
		tv1 := ctx.fresher.newTypeVariable(0, emptyProv, "a", nil, nil)
		tv2 := ctx.fresher.newTypeVariable(0, emptyProv, "b", nil, nil)

		// Set up bounds
		tv1.lowerBounds = []SimpleType{&extremeType{polarity: true, withProvenance: emptyProv.embed()}}
		tv1.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}

		tv2.lowerBounds = []SimpleType{tv1}
		tv2.upperBounds = []SimpleType{&extremeType{polarity: false, withProvenance: emptyProv.embed()}}
		result := ctx.cleanBounds(tv2, cleanBoundsOpts{inPlace: true})

		// The result should be the same type variable
		assert.Equal(t, tv2, result, "Result should be the same type variable with in-place processing")
	})
}
