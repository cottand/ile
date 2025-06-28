package types

import (
	"github.com/cottand/ile/frontend/ir"
	"github.com/hashicorp/go-set/v3"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestDNFMergeUnion(t *testing.T) {
	ctx := NewEmptyTypeCtx()
	DNF := newOpsDNF(ctx, true)
	testCases := []struct {
		left, right conjunct
		expected    conjunct
		expectedOk  bool
	}{{
		left: newConjunct(
			&lhsRefined{
				typeRefs: []typeRef{{
					defName: "False",
				}},
			},
			rhsBot{},
			nil, nil),
		right: newConjunct(
			&lhsRefined{
				typeRefs: []typeRef{{
					defName: "True",
				}},
			},
			&rhsBases{
				typeRefs: []typeRef{{
					defName: "False",
				}},
			},
			nil, nil),
		expectedOk: false,
	}}

	for _, testCase := range testCases {
		t.Run(testCase.left.String()+"V"+testCase.left.String(), func(t *testing.T) {
			res, ok := DNF.tryMergeUnion(testCase.left, testCase.right)
			assert.Equal(t, testCase.expectedOk, ok, "for (%s V %s) expected not OK, got %s", testCase.left, testCase.right, res)
			if testCase.expectedOk {
				assert.Equal(t, testCase.expected, res)
			}
		})
	}

}

func TestDNFAndConjuct(t *testing.T) {
	ctx := NewEmptyTypeCtx()
	DNF := newOpsDNF(ctx, true)
	testCases := []struct {
		left     dnf
		right    conjunct
		expected dnf
	}{{
		left: dnf{
			newConjunct(
				&lhsRefined{
					typeRefs: []typeRef{{
						defName: "False",
					}},
				},
				rhsBot{},
				nil, nil),
		},
		right: newConjunct(
			lhsTop{},
			&rhsBases{
				typeRefs: []typeRef{{
					defName: "True",
				}},
			},
			nil, nil),
		expected: dnf{
			newConjunct(
				&lhsRefined{
					typeRefs: []typeRef{{
						defName: "False",
					}},
				},
				rhsBot{}, nil, nil,
			),
		}}}

	for _, testCase := range testCases {
		t.Run(testCase.left.String()+"&"+testCase.left.String(), func(t *testing.T) {
			res := DNF.andConjunct(testCase.left, testCase.right)
			assert.Equal(t, testCase.expected, res, "for (%s & %s) expected %s, got %s", testCase.left, testCase.right, testCase.expected, res)
		})
	}
}

func TestDNFAndDNF(t *testing.T) {
	ctx := NewEmptyTypeCtx()
	DNF := newOpsDNF(ctx, true)
	testCases := []struct {
		left, right dnf
		expected    dnf
	}{{
		left: dnf{
			newConjunct(
				&lhsRefined{
					typeRefs: []typeRef{{
						defName: "False",
					}},
				},
				rhsBot{},
				nil, nil),
		},
		right: dnf{newConjunct(
			lhsTop{},
			&rhsBases{
				typeRefs: []typeRef{{
					defName: "True",
				}},
			},
			nil, nil)},
		expected: dnf{
			newConjunct(
				&lhsRefined{
					typeRefs: []typeRef{{
						defName: "False",
					}},
				},
				rhsBot{}, nil, nil,
			),
		}}}

	for _, testCase := range testCases {
		t.Run(testCase.left.String()+"&"+testCase.left.String(), func(t *testing.T) {
			res := DNF.and(testCase.left, testCase.right)
			assert.Equal(t, testCase.expected, res, "for (%s & %s) expected %s, got %s", testCase.left, testCase.right, testCase.expected, res)
		})
	}

}

func TestLeftNFAndType(t *testing.T) {
	ctx := NewEmptyTypeCtx()
	ops := newOpsDNF(ctx, true)

	// Helper function to create ir.Var
	createVar := func(name string) *ir.Var {
		return &ir.Var{Name: name}
	}

	// Test cases
	testCases := []struct {
		name     string
		left     lhsNF
		right    basicType
		expected lhsNF
		ok       bool
	}{
		{
			name: "lhsTop and tupleType",
			left: lhsTop{},
			right: tupleType{
				fields: []SimpleType{topType, topType},
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsTop and arrayType",
			left: lhsTop{},
			right: arrayType{
				innerT: topType,
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr: arrayType{
					innerT: topType,
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsTop and classTag",
			left: lhsTop{},
			right: &classTag{
				id: createVar("MyClass"),
				parents: set.NewTreeSet(func(a, b string) int {
					if a < b {
						return -1
					} else if a > b {
						return 1
					}
					return 0
				}),
			},
			expected: &lhsRefined{
				base: &classTag{
					id: createVar("MyClass"),
					parents: set.NewTreeSet(func(a, b string) int {
						if a < b {
							return -1
						} else if a > b {
							return 1
						}
						return 0
					}),
				},
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsTop and traitTag",
			left: lhsTop{},
			right: traitTag{
				id: createVar("MyTrait"),
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: func() set.Collection[traitTag] {
					s := set.NewTreeSet(compareTraitTags)
					s.Insert(traitTag{id: createVar("MyTrait")})
					return s
				}(),
				reft:     recordType{},
				typeRefs: nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined and traitTag",
			left: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: traitTag{
				id: createVar("MyTrait"),
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: func() set.Collection[traitTag] {
					s := set.NewTreeSet(compareTraitTags)
					s.Insert(traitTag{id: createVar("MyTrait")})
					return s
				}(),
				reft:     recordType{},
				typeRefs: nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with no base and classTag",
			left: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: &classTag{
				id: createVar("MyClass"),
				parents: set.NewTreeSet(func(a, b string) int {
					if a < b {
						return -1
					} else if a > b {
						return 1
					}
					return 0
				}),
			},
			expected: &lhsRefined{
				base: &classTag{
					id: createVar("MyClass"),
					parents: set.NewTreeSet(func(a, b string) int {
						if a < b {
							return -1
						} else if a > b {
							return 1
						}
						return 0
					}),
				},
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with no array and tupleType",
			left: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: tupleType{
				fields: []SimpleType{topType, topType},
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with no array and arrayType",
			left: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr:       nil,
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: arrayType{
				innerT: topType,
			},
			expected: &lhsRefined{
				base:      nil,
				fn:        nil,
				arr: arrayType{
					innerT: topType,
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with tupleType and tupleType of same size",
			left: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: tupleType{
				fields: []SimpleType{topType, topType},
			},
			expected: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with tupleType and tupleType of different size",
			left: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: tupleType{
				fields: []SimpleType{topType, topType, topType},
			},
			expected: nil,
			ok:       false,
		},
		{
			name: "lhsRefined with arrayType and tupleType",
			left: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: arrayType{
					innerT: topType,
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: tupleType{
				fields: []SimpleType{topType, topType},
			},
			expected: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with tupleType and arrayType",
			left: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: arrayType{
				innerT: topType,
			},
			expected: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: tupleType{
					fields: []SimpleType{topType, topType},
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
		{
			name: "lhsRefined with arrayType and arrayType",
			left: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: arrayType{
					innerT: topType,
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			right: arrayType{
				innerT: topType,
			},
			expected: &lhsRefined{
				base: nil,
				fn:   nil,
				arr: arrayType{
					innerT: topType,
				},
				traitTags: set.NewTreeSet(compareTraitTags),
				reft:      recordType{},
				typeRefs:  nil,
			},
			ok: true,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ops.leftNFAndType(tc.left, tc.right)
			assert.Equal(t, tc.ok, ok, "Expected ok=%v, got %v", tc.ok, ok)
			if tc.ok {
				// For simplicity, just compare the string representations
				assert.Equal(t, tc.expected.String(), result.String(), 
					"Expected %v, got %v", tc.expected, result)
			}
		})
	}
}