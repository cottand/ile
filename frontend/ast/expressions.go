// The MIT License (MIT)
//
// Copyright (c) 2019 West Damron
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Package ast
// The following expressions are supported:
//
//	Literal:         semi-opaque literal value
//	Var:             variable
//	Deref:           dereference
//	DerefAssign:     dereference and assign
//	ControlFlow:     control-flow graph
//	Pipe:            pipeline
//	Call:            function call
//	Func:            function abstraction
//	Assign:             let-binding
//	LetGroup:        grouped let-bindings
//	RecordSelect:    selecting (scoped) value of label
//	RecordExtend:    extending record
//	RecordRestrict:  deleting (scoped) label
//	RecordEmpty:     empty record
//	Variant:         tagged (ad-hoc) variant
//	MatchSubject:           variant-matching switch
package ast

import (
	"github.com/cottand/ile/frontend/types"
	"go/token"
)

var (
	_ Expr = (*Literal)(nil)
	_ Expr = (*Var)(nil)
	_ Expr = (*Deref)(nil)
	_ Expr = (*DerefAssign)(nil)
	_ Expr = (*Pipe)(nil)
	_ Expr = (*Call)(nil)
	_ Expr = (*Func)(nil)
	_ Expr = (*Assign)(nil)
	_ Expr = (*LetGroup)(nil)
	_ Expr = (*RecordSelect)(nil)
	_ Expr = (*RecordExtend)(nil)
	_ Expr = (*RecordRestrict)(nil)
	_ Expr = (*RecordEmpty)(nil)
	_ Expr = (*Variant)(nil)
	_ Expr = (*When)(nil)
	_ Expr = (*MatchSubject)(nil)
	_ Expr = (*Unused)(nil)

	_ Expr = (*ErrorExpr)(nil)
)

// Expr is the base for all expressions.
//
// The following expressions are supported:
//
//	Literal:         semi-opaque literal value
//	Var:             variable
//	Deref:           dereference
//	DerefAssign:     dereference and assign
//	ControlFlow:     control-flow graph
//	Pipe:            pipeline
//	Call:            function call
//	Func:            function abstraction
//	Assign:             let-binding
//	LetGroup:        grouped let-bindings
//	RecordSelect:    selecting (scoped) value of label
//	RecordExtend:    extending record
//	RecordRestrict:  deleting (scoped) label
//	RecordEmpty:     empty record
//	Variant:         tagged (ad-hoc) variant
//	MatchSubject:           variant-matching switch
type Expr interface {
	Positioner
	// ExprName is the name of the syntax-type of the expression.
	ExprName() string
	// Type returns an inferred type for an expression. Expression types are only available after type-inference.
	Type() types.Type

	// Transform should, in order:
	//  - copy the expression
	//  - call Transform(f) on any child expressions (thus copying them too)
	//  - call f on this Expr
	// In practice this means first copying the entire tree, applying f to each component bottom-up,
	// and returning the result
	Transform(f func(Expr) Expr) Expr

	TAnnotated
}

// TAnnotated can apply to an Expr if its type can be directly annotated in the source.
// This should only be used in the frontend, not in the backend (where we rely on the inferred
// types.Type
//
// If the Expr is a TAnnotated that did not have a declared type, it
// may return nil
type TAnnotated interface {
	GetTAnnotation() TypeAnnotation
	SetTAnnotation(TypeAnnotation)
}

// tAnnotationContainer can be embedded into a node so
// that it becomes TAnnotated
type tAnnotationContainer struct {
	annotation TypeAnnotation
}

func (t *tAnnotationContainer) GetTAnnotation() TypeAnnotation {
	return t.annotation
}

// SetTAnnotation can be called on an existing TAnnotated in order
// to give it an annotation after generating it.
//
// This is useful when building the AST as when we make an expression we do not know its
// parent node, so we won't know if it is annotated or not!
func (t *tAnnotationContainer) SetTAnnotation(a TypeAnnotation) {
	t.annotation = a
}

// PredeclaredScope is a placeholder scope for variables bound outside an expression, or top-level variables.
var PredeclaredScope = &Scope{}

// Scope is a variable-binding scope, such as a let-binding or function.
type Scope struct {
	// Expr is an expression which introduces a new variable-binding scope
	Expr Expr
	// Parent is the nearest scope which encloses the expression
	Parent *Scope
}

// Semi-opaque literal value
type Literal struct {
	// Syntax is a string representation of the literal value. The syntax will be printed when the literal is printed.
	Syntax string
	// Using may contain identifiers which will be looked up in the type-environment when the type is constructed.
	Using []string
	// Construct should produce a type at the given binding-level. The constructed type may include
	// types derived from variables which are already in scope (retrieved from the type-environment).
	Construct func(env types.TypeEnv, level uint, using []types.Type) (types.Type, error)
	inferred  types.Type

	// Kind indicates what literal this is originally
	// this is useful for the transpiling phase, and is not used during type inference.
	//
	// Should be one of
	// token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
	Kind token.Token

	Positioner
	tAnnotationContainer
}

// Returns the syntax of e.
func (e *Literal) ExprName() string { return e.Syntax }

// Get the inferred (or assigned) type of e.
func (e *Literal) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Literal) SetType(t types.Type) { e.inferred = t }

func (e *Literal) Copy() Expr {
	copied := *e
	return &copied
}

func (e *Literal) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// Variable (or identifier)
type Var struct {
	Name     string
	inferred types.Type
	scope    *Scope
	Range
	tAnnotationContainer
}

// "Var"
func (e *Var) ExprName() string { return "Var" }

// Get the inferred (or assigned) type of e.
func (e *Var) Type() types.Type { return types.RealType(e.inferred) }

// Get the inferred (or assigned) scope where e is defined.
func (e *Var) Scope() *Scope { return e.scope }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Var) SetType(t types.Type) { e.inferred = t }

// Assign a binding scope for e. Scope assignments should occur indirectly, during inference.
func (e *Var) SetScope(scope *Scope) { e.scope = scope }

func (e *Var) Copy() Expr {
	copied := *e
	return &copied
}
func (e *Var) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// We might not need QualifiedIdent after all if we represent packages as records!
/*// QualifiedIdent is a Qualified variable or identifier
type QualifiedIdent struct {
	Qualifier string
	Name      string
	inferred  types.Type
	scope     *Scope
	Range
	tAnnotationContainer
}

func (e *QualifiedIdent) ExprName() string { return "QualifiedIdent" }

func (e *QualifiedIdent) Type() types.Type { return types.RealType(e.inferred) }

func (e *QualifiedIdent) Scope() *Scope { return e.scope }

func (e *QualifiedIdent) SetType(t types.Type) { e.inferred = t }

func (e *QualifiedIdent) SetScope(scope *Scope) { e.scope = scope }

func (e *QualifiedIdent) Copy() Expr {
	copied := *e
	return &copied
}
func (e *QualifiedIdent) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// FormattedName is just Qualifier . Name
// Qualifier may contain `.`, but not Name
func (e *QualifiedIdent) FormattedName() string {
	return e.Qualifier + "." + e.Name
}
*/

// Dereference: `*x`
type Deref struct {
	Ref      Expr
	inferred types.Type
	Range
	tAnnotationContainer
}

// "Deref"
func (e *Deref) ExprName() string { return "Deref" }

// Get the inferred (or assigned) type of e.
func (e *Deref) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Deref) SetType(t types.Type) { e.inferred = t }

func (e *Deref) Copy() Expr {
	copied := *e
	return &copied
}
func (e *Deref) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// Dereference and assign: `*x = y`
type DerefAssign struct {
	Ref      Expr
	Value    Expr
	inferred types.Type
	Range
	tAnnotationContainer
}

// "DerefAssign"
func (e *DerefAssign) ExprName() string { return "DerefAssign" }

// Get the inferred (or assigned) type of e.
func (e *DerefAssign) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *DerefAssign) SetType(t types.Type) { e.inferred = t }

func (e *DerefAssign) Copy() Expr {
	copied := *e
	return &copied
}
func (e *DerefAssign) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// Application: `f(x)`
type Call struct {
	Func         Expr
	Args         []Expr
	inferred     types.Type
	inferredFunc *types.Arrow
	Range        // of the entire expression
	tAnnotationContainer
}

// "Call"
func (e *Call) ExprName() string { return "Call" }

// Get the inferred (or assigned) type of e.
func (e *Call) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Call) SetType(t types.Type) { e.inferred = t }

// Get the inferred (or assigned) function/method called in e.
func (e *Call) FuncType() *types.Arrow { return e.inferredFunc }

// Assign the function/method called in e. Type assignments should occur indirectly, during inference.
func (e *Call) SetFuncType(t *types.Arrow) { e.inferredFunc = t }

func (e *Call) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Args = make([]Expr, len(e.Args))
	for i, arg := range e.Args {
		copied.Args[i] = arg.Transform(f)
	}
	copied.Func = e.Func.Transform(f)
	return f(&copied)
}

// Function abstraction: `fn (x, y) -> x`
type Func struct {
	ArgNames []string
	Body     Expr
	inferred *types.Arrow
	Range    // of the declaration including parameters but not the body
	tAnnotationContainer
}

// "Func"
func (e *Func) ExprName() string { return "Func" }

// Get the inferred (or assigned) type of e.
func (e *Func) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Func) SetType(ft *types.Arrow) { e.inferred = ft }

// Get the inferred (or assigned) type of an argument of e.
func (e *Func) ArgType(index int) types.Type { return types.RealType(e.inferred.Args[index]) }

// Get the inferred (or assigned) return type of e.
func (e *Func) RetType() types.Type { return types.RealType(e.inferred.Return) }

func (e *Func) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = e.Body.Transform(f)
	return f(&copied)
}

// Assign is an expression that sets a Var to an Expr for the rest of the body
// in ile. It is equivalent to a let expression in other languages,
// we just syntactically omit both `let` and `in`
type Assign struct {
	Var   string
	Value Expr
	Body  Expr
	Range
	tAnnotationContainer
}

func (e *Assign) ExprName() string { return "Assign" }

// Get the inferred (or assigned) type of e.
func (e *Assign) Type() types.Type { return e.Body.Type() }

func (e *Assign) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = copied.Body.Transform(f)
	copied.Value = copied.Value.Transform(f)
	return f(&copied)
}

// Unused is like Assign, except it does not store the Value in a Var
// it is useful for calling expressions with side effects
type Unused struct {
	Value Expr
	Body  Expr
	Range
	tAnnotationContainer
}

func (e *Unused) ExprName() string { return "Unused" }
func (e *Unused) Type() types.Type { return e.Body.Type() }

func (e *Unused) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = e.Body.Transform(f)
	copied.Value = e.Value.Transform(f)
	return f(&copied)
}

// Grouped let-bindings: `let a = 1 and b = 2 in e`
type LetGroup struct {
	Vars []LetBinding
	Body Expr
	sccs [][]LetBinding
	Range
	tAnnotationContainer
}

// "LetGroup"
func (e *LetGroup) ExprName() string { return "LetGroup" }

// Get the inferred (or assigned) type of e.
func (e *LetGroup) Type() types.Type { return e.Body.Type() }

// Get the strongly connected components inferred for e, in dependency order.
// The strongly connected components will be assigned if e is inferred with
// annotation enabled.
//
// Each component is a variable bound by e.
func (e *LetGroup) StronglyConnectedComponents() [][]LetBinding { return e.sccs }

// Assign the strongly connected components for e. Assignments should occur indirectly,
// during inference.
//
// Each component should be a variable bound by e.
func (e *LetGroup) SetStronglyConnectedComponents(sccs [][]LetBinding) { e.sccs = sccs }

func (e *LetGroup) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Vars = make([]LetBinding, len(e.Vars))
	for i, b := range e.Vars {
		// b is not a pointer so this will copy it
		copied.Vars[i] = LetBinding{
			Var:   b.Var,
			Value: b.Value.Transform(f),
		}
	}
	copied.Body = e.Body.Transform(f)
	return f(&copied)
}

// Paired identifier and value
type LetBinding struct {
	Var   string
	Value Expr
}

// Get the inferred (or assigned) type of e.
func (e *LetBinding) Type() types.Type { return e.Value.Type() }

// Selecting (scoped) value of label: `r.a`
type RecordSelect struct {
	Record   Expr
	Label    string
	inferred types.Type
	Range
	tAnnotationContainer
}

// "RecordSelect"
func (e *RecordSelect) ExprName() string { return "RecordSelect" }

// Get the inferred (or assigned) type of e.
func (e *RecordSelect) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordSelect) SetType(t types.Type) { e.inferred = t }

func (e *RecordSelect) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Record = e.Record.Transform(f)
	return f(&copied)
}

// Extending record: `{a = 1, b = 2 | r}`
type RecordExtend struct {
	Record   Expr
	Labels   []LabelValue
	inferred *types.Record
	Range
	tAnnotationContainer
}

// "RecordExtend"
func (e *RecordExtend) ExprName() string { return "RecordExtend" }

// Get the inferred (or assigned) type of e.
func (e *RecordExtend) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordExtend) SetType(rt *types.Record) { e.inferred = rt }

func (e *RecordExtend) Transform(f func(Expr) Expr) Expr {
	copied := *e
	labels := make([]LabelValue, len(e.Labels))
	for i, v := range e.Labels {
		labels[i] = LabelValue{v.Label, v.Value.Transform(f)}
	}
	record := e.Record
	if record == nil {
		record = f(&RecordEmpty{})
	} else {
		record = record.Transform(f)
	}
	copied.Labels = labels
	copied.Record = record
	return f(&copied)
}

// Paired label and value
type LabelValue struct {
	Label string
	Value Expr
}

// Get the inferred (or assigned) type of e.
func (e *LabelValue) Type() types.Type { return e.Value.Type() }
func (e *LabelValue) Copy() *LabelValue {
	copied := *e
	return &copied
}

// Deleting (scoped) label: `{r - a}`
type RecordRestrict struct {
	Record   Expr
	Label    string
	inferred *types.Record
	Range
	tAnnotationContainer
}

// "RecordRestrict"
func (e *RecordRestrict) ExprName() string { return "RecordRestrict" }

// Get the inferred (or assigned) type of e.
func (e *RecordRestrict) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordRestrict) SetType(rt *types.Record) { e.inferred = rt }

func (e *RecordRestrict) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	e.Record = e.Record.Transform(f)
	return f(&copied)
}

// Empty record: `{}`
type RecordEmpty struct {
	inferred *types.Record
	Range
	tAnnotationContainer
}

// "RecordEmpty"
func (e *RecordEmpty) ExprName() string { return "RecordEmpty" }

// Get the inferred (or assigned) type of e.
func (e *RecordEmpty) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordEmpty) SetType(rt *types.Record) { e.inferred = rt }

func (e *RecordEmpty) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// Tagged (ad-hoc) variant: `:X a`
type Variant struct {
	Label string
	Value Expr
	Range
	tAnnotationContainer
}

// "Variant"
func (e *Variant) ExprName() string { return "Variant" }

// Get the inferred (or assigned) type of e.
func (e *Variant) Type() types.Type { return e.Value.Type() }

func (e *Variant) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	e.Value = e.Value.Transform(f)
	return f(&copied)
}

// When is a Bool-matching switch:
//
//	when {
//	  boolExpr1 -> expr1
//	  boolExpr2 -> expr2
//	  _ -> expr2
//	}
type When struct {
	Cases      []WhenCase
	inferred   types.Type
	Positioner // of the 'when' operator (not the clauses)
	tAnnotationContainer
}

func (e *When) ExprName() string { return "when" }
func (e *When) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	newCases := make([]WhenCase, len(e.Cases))
	for i, c := range e.Cases {
		newCases[i] = c
		newCases[i].Predicate = c.Predicate.Transform(f)
		newCases[i].Value = c.Value.Transform(f)
	}
	copied.Cases = newCases
	return f(&copied)
}
func (e *When) SetType(t types.Type) { e.inferred = t }
func (e *When) Type() types.Type     { return types.RealType(e.inferred) }

type WhenCase struct {
	Predicate Expr
	Value     Expr
	Positioner
}

// Assign a type to e. Type assignments should occur indirectly, during inference.

// MatchSubject is a Variant-matching switch:
//
//	match e {
//	    :X a -> expr1
//	  | :Y b -> expr2
//	  |  ...
//	  | z -> default_expr (optional)
//	}
type MatchSubject struct {
	Value    Expr
	Cases    []MatchCase
	Default  *MatchCase
	inferred types.Type
	Range    // of the match operator and the matched first expression (not the clauses)
	tAnnotationContainer
}

// "MatchSubject"
func (e *MatchSubject) ExprName() string { return "MatchSubject" }

// Get the inferred (or assigned) type of e.
func (e *MatchSubject) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *MatchSubject) SetType(t types.Type) { e.inferred = t }

func (e *MatchSubject) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	cases := make([]MatchCase, len(e.Cases))
	for i, v := range e.Cases {
		copiedMatchCase := v
		copiedMatchCase.Value = v.Value.Transform(f)
		cases[i] = copiedMatchCase
	}
	defaultCase := e.Default
	if defaultCase != nil {
		copiedDefault := *defaultCase
		defaultCase = &copiedDefault
		defaultCase.Value = copiedDefault.Value.Transform(f)
	}
	copied.Value = e.Value.Transform(f)
	copied.Cases = cases
	copied.Default = defaultCase
	return f(&copied)
}

// MatchPattern describes a types.Type or an Expr that we would like to match against
type MatchPattern interface {
	matchPattern() // marker

	TransformChildExprs(f func(expr Expr) Expr) MatchPattern
	Positioner
}

// ValueLiteralPattern represents matching a value.
// Value can be any Expr but the language may have restrictions on what Value can be (eg, literals only)
type ValueLiteralPattern struct {
	Value Expr
	Positioner
}

func (e *ValueLiteralPattern) matchPattern() {}
func (e *ValueLiteralPattern) TransformChildExprs(f func(expr Expr) Expr) MatchPattern {
	copied := *e
	copied.Value = e.Value.Transform(f)
	return &copied
}

// Predicate expression within MatchSubject: `:X a -> expr1`
type MatchCase struct {
	Label   string
	Var     string
	Value   Expr
	varType types.Type
}

// Get the inferred (or assigned) type of e.
func (e *MatchCase) Type() types.Type { return e.Value.Type() }

// Get the inferred (or assigned) variant-type of e.
func (e *MatchCase) VariantType() types.Type { return types.RealType(e.varType) }

// Assign a variant-type to e. Type assignments should occur indirectly, during inference.
func (e *MatchCase) SetVariantType(t types.Type) { e.varType = t }

// Pipeline: `pipe $ = xs |> fmap($, fn (x) -> to_y(x)) |> fmap($, fn (y) -> to_z(y))`
type Pipe struct {
	Source   Expr
	As       string
	Sequence []Expr
	inferred types.Type
	Range    // of the first pipe operator?
	tAnnotationContainer
}

// "Pipe"
func (e *Pipe) ExprName() string { return "Pipe" }

// Get the inferred (or assigned) type of e.
func (e *Pipe) Type() types.Type { return types.RealType(e.inferred) }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Pipe) SetType(t types.Type) { e.inferred = t }

func (e *Pipe) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Sequence = make([]Expr, len(e.Sequence))
	for i, expr := range e.Sequence {
		copied.Sequence[i] = expr.Transform(f)
	}
	copied.Source = e.Source.Transform(f)
	return f(&copied)
}

// ErrorExpr is an AST node that could not be parsed, and it is where an Expr should be
type ErrorExpr struct {
	Range
	Syntax string
	tAnnotationContainer
}

func (e *ErrorExpr) Type() types.Type { return types.NewUnit() }
func (e *ErrorExpr) ExprName() string { return "Error" }
func (e *ErrorExpr) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}
