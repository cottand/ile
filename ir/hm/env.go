package hm

import "iter"

// An Env is essentially a map of names to schemes
type Env interface {
	Substitutable
	SchemeOf(string) (*Scheme, bool)
	Clone() Env

	Add(string, *Scheme) Env
	Remove(string) Env
	MergeWith(Env) Env
	All() iter.Seq2[string, *Scheme]
}

type SimpleEnv map[string]*Scheme

func (e SimpleEnv) Apply(sub Subs) Substitutable {
	logf("Applying %v to env", sub)
	if sub == nil {
		return e
	}

	for _, v := range e {
		v.Apply(sub) // apply mutates AdditionalEnv, so no need to set
	}
	return e
}

func (e SimpleEnv) FreeTypeVar() TypeVarSet {
	var retVal TypeVarSet
	for _, v := range e {
		retVal = v.FreeTypeVar().Union(retVal)
	}
	return retVal
}

func (e SimpleEnv) SchemeOf(name string) (retVal *Scheme, ok bool) { retVal, ok = e[name]; return }
func (e SimpleEnv) Clone() Env {
	retVal := make(SimpleEnv)
	for k, v := range e {
		retVal[k] = v.Clone()
	}
	return retVal
}

func (e SimpleEnv) Add(name string, s *Scheme) Env {
	if s == nil {
		return e
	}
	e[name] = s
	return e
}

func (e SimpleEnv) Remove(name string) Env {
	delete(e, name)
	return e
}

func (e SimpleEnv) All() iter.Seq2[string, *Scheme] {
	return func(yield func(string, *Scheme) bool) {
		for k, v := range e {
			yield(k, v)
		}
	}
}

// MergeWith appends two Env, where env has precedence over e
func (e SimpleEnv) MergeWith(env Env) Env {
	cloned := e.Clone()
	for k, v := range env.All() {
		cloned.Add(k, v.Clone())
	}
	return cloned
}
