package hmtypes

import (
	. "github.com/cottand/ile/ir/hm"
	"testing"
)

var unifyTests = []struct {
	name string
	a    Type
	b    Type

	subs Subs
	err  bool // does it error?
}{

	// function types
	{"(a, a, b) ~ (proton, proton, neutron)",
		NewTupleType("", TypeVariable('a'), TypeVariable('a'), TypeVariable('b')),
		NewTupleType("", proton, proton, neutron),
		mSubs{'a': proton, 'b': neutron}, false},
}

func TestUnify(t *testing.T) {
	// assert := assert.New(t)
	var t0, t1 Type
	var u0, u1 Type
	var sub Subs
	var err error

	for _, uts := range unifyTests {
		// logf("unifying %v", uts.name)
		t0 = uts.a
		t1 = uts.b
		sub, err = Unify(t0, t1)

		switch {
		case err == nil && uts.err:
			t.Errorf("Test %q - Expected an error: %v | u0: %#v, u1: %#v", uts.name, err, u0, u1)
		case err != nil && !uts.err:
			t.Errorf("Test %q errored: %v ", uts.name, err)
		}

		if uts.err {
			continue
		}

		if uts.subs == nil {
			if sub != nil {
				t.Errorf("Test: %q Expected no substitution. Got %v instead", uts.name, sub)
			}
			continue
		}

		for _, s := range uts.subs.Iter() {
			if T, ok := sub.Get(s.Tv); !ok {
				t.Errorf("Test: %q TypeVariable %v expected in result", uts.name, s.Tv)
			} else if T != s.T {
				t.Errorf("Test: %q Expected TypeVariable %v to be substituted by %v. Got %v instead", uts.name, s.Tv, s.T, T)
			}
		}

		if uts.subs.Size() != sub.Size() {
			t.Errorf("Test: %q Expected subs to be the same size", uts.name)
		}

		sub = nil
	}
}
