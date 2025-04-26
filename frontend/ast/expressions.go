package ast

import (
	"encoding/binary"
	"fmt"
	"github.com/cottand/ile/frontend/hmtypes"
	"github.com/hashicorp/go-set/v3"
	"go/token"
	"hash/fnv"
	"strconv"
	"strings"
)

var (
	_ Expr = (*Literal)(nil)
	_ Expr = (*Var)(nil)
	_ Expr = (*Deref)(nil)
	_ Expr = (*Pipe)(nil)
	_ Expr = (*Call)(nil)
	_ Expr = (*Ascribe)(nil)
	_ Expr = (*Func)(nil)
	_ Expr = (*Assign)(nil)
	_ Expr = (*LetGroup)(nil)
	_ Expr = (*RecordSelect)(nil)
	_ Expr = (*RecordExtend)(nil)
	_ Expr = (*RecordRestrict)(nil)
	_ Expr = (*RecordEmpty)(nil)
	_ Expr = (*Variant)(nil)
	_ Expr = (*WhenMatch)(nil)
	_ Expr = (*Unused)(nil)
	_ Expr = (*ListLiteral)(nil)

	_ AtomicExpr = (*Literal)(nil)

	_ Expr = (*ErrorExpr)(nil)

	_ AtomicExpr = (*Var)(nil)
	_ AtomicExpr = (*Literal)(nil)
)

func (e *Literal) Describe() string {
	var kindStr string
	switch e.Kind {
	case token.INT:
		kindStr = "int"
	case token.FLOAT:
		kindStr = "float"
	default:
		panic("unhandled default case")
	}
	return kindStr + " literal"
}

func (e *Var) Describe() string            { return "variable" }
func (e *Deref) Describe() string          { return "dereference" }
func (e *Pipe) Describe() string           { return "pipeline" }
func (e *Call) Describe() string           { return "function call" }
func (e *Ascribe) Describe() string        { return "type annotation" }
func (e *Func) Describe() string           { return "function" }
func (e *Assign) Describe() string         { return "declaration" }
func (e *LetGroup) Describe() string       { return "let group" }
func (e *RecordSelect) Describe() string   { return "record select" }
func (e *RecordExtend) Describe() string   { return "record extend" }
func (e *RecordRestrict) Describe() string { return "record restrict" }
func (e *RecordEmpty) Describe() string    { return "empty record" }
func (e *Variant) Describe() string        { return "variant" }
func (e *WhenMatch) Describe() string      { return "variant-matching switch" }
func (e *Unused) Describe() string         { return "expression" }
func (e *ListLiteral) Describe() string    { return "list literal" }
func (e *ErrorExpr) Describe() string      { return "error expression" }

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
//	WhenMatch:           variant-matching switch
type Expr interface {
	Positioner
	// ExprName is the Name of the syntax-type of the expression.
	ExprName() string
	// Describe is what to call this expression in error messages
	Describe() string

	// Transform should, in order:
	//  - copy the expression
	//  - call Transform(f) on any child expressions (thus copying them too)
	//  - call f on this Expr
	// In practice this means first copying the entire tree, applying f to each component bottom-up,
	// and returning the result
	Transform(f func(Expr) Expr) Expr
	Hash() uint64
}

// InferrableExpr is an Expr which we are interested in knowing its type after the inference phase
// All Expr are inferrable, but in practice we are not interested in storing inference info for all of them.
//
// To assign a type to an expression previous to inference, use ast.Ascribe
type InferrableExpr interface {
	Expr
	// SetType mutates this Expr to assign it a type during inference
	SetType(t Type)
}

func RangeOf(expr Positioner) Range {
	if expr == nil {
		return Range{}
	}
	if asRange, ok := expr.(*Range); ok {
		return *asRange
	}
	if asRange, ok := expr.(Range); ok {
		return asRange
	}
	return Range{expr.Pos(), expr.End()}
}

// AtomicExpr is an Expr which specifically represents
// variables and literals only
//
// It is called SimpleTerm in the mlstruct scala implementation
type AtomicExpr interface {
	Expr
	// CanonicalSyntax should return equal strings for equal AtomicExpr
	// for example, 1.0 and 1.00 for floats
	//
	// It is called IdStr in the mlstruct scala implementation
	CanonicalSyntax() string
	isAtomicExpr()
	Equivalent(other AtomicExpr) bool
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
	inferred Type

	// Kind indicates what literal this is originally
	//
	// Should be one of
	// token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
	Kind token.Token

	Range
}

func (e *Literal) isAtomicExpr() {}

// Returns the syntax of e.
func (e *Literal) ExprName() string { return e.Syntax }

// getCached the inferred (or assigned) type of e.
func (e *Literal) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Literal) SetType(t Type) { e.inferred = t }

func (e *Literal) Copy() Expr {
	copied := *e
	return &copied
}

func (e *Literal) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// TODO desugar identical literals (01 == 1)
func (e *Literal) CanonicalSyntax() string { return e.Syntax }
func (e *Literal) BaseTypes() set.Collection[string] {
	switch e.Kind {
	case token.INT:
		return set.From([]string{IntTypeName, NumberTypeName})

	default:
		logger.Warn("unrecognized literal type, not providing base types", "type", e.Kind.String())
		// TODO add base types for each lit type (LitImpl in reference scala implementation)
		return set.New[string](0)
	}
}
func (e *Literal) Equivalent(other AtomicExpr) bool {
	otherAsLiteral, ok := other.(*Literal)
	return ok && e.Syntax == otherAsLiteral.Syntax
}

// Hash returns a hash value for the Literal, based on its structural characteristics
func (e *Literal) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte(e.Syntax))
	_, _ = h.Write([]byte(e.Kind.String()))
	return h.Sum64()
}

// Variable (or Identifier)
type Var struct {
	Name     string
	inferred Type
	scope    *Scope
	Range
}

// "Var"
func (e *Var) ExprName() string { return "Var" }

// getCached the inferred (or assigned) type of e.
func (e *Var) Type() Type { return e.inferred }

// getCached the inferred (or assigned) scope where e is defined.
func (e *Var) Scope() *Scope { return e.scope }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Var) SetType(t Type) { e.inferred = t }

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
func (e *Var) isAtomicExpr() {}

func (e *Var) CanonicalSyntax() string { return e.Name }
func (e *Var) Equivalent(other AtomicExpr) bool {
	otherAsVar, ok := other.(*Var)
	return ok && e.Name == otherAsVar.Name
}

// Hash returns a hash value for the Var, based on its structural characteristics
func (e *Var) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte(e.Name))
	return h.Sum64()
}

// Dereference: `*x`
type Deref struct {
	Ref      Expr
	inferred Type
	Range
}

// "Deref"
func (e *Deref) ExprName() string { return "Deref" }

// getCached the inferred (or assigned) type of e.
func (e *Deref) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Deref) SetType(t Type) { e.inferred = t }

func (e *Deref) Copy() Expr {
	copied := *e
	return &copied
}
func (e *Deref) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

func (e *Deref) Hash() uint64 {
	return e.Hash() ^ 31
}

// Application: `f(x)`
type Call struct {
	Func         Expr
	Args         []Expr
	inferred     Type
	inferredFunc *hmtypes.Arrow
	Range        // of the entire expression
}

// "Call"
func (e *Call) ExprName() string { return "Call" }

// getCached the inferred (or assigned) type of e.
func (e *Call) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Call) SetType(t Type) { e.inferred = t }

// getCached the inferred (or assigned) function/method called in e.
func (e *Call) FuncType() *hmtypes.Arrow { return e.inferredFunc }

// Assign the function/method called in e. Type assignments should occur indirectly, during inference.
func (e *Call) SetFuncType(t *hmtypes.Arrow) { e.inferredFunc = t }

func (e *Call) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Args = make([]Expr, len(e.Args))
	for i, arg := range e.Args {
		copied.Args[i] = arg.Transform(f)
	}
	copied.Func = e.Func.Transform(f)
	return f(&copied)
}

func (e *Call) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Call")
	arr = binary.LittleEndian.AppendUint64(arr, e.Func.Hash())
	for _, arg := range e.Args {
		arr = binary.LittleEndian.AppendUint64(arr, arg.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

type Ascribe struct {
	Expr  Expr
	Type_ Type
	Range // of the type annotating operator (':')
}

func (e *Ascribe) ExprName() string { return "Ascribe" }
func (e *Ascribe) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Expr = e.Expr.Transform(f)
	return f(&copied)
}
func (e *Ascribe) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Ascribe" + e.Type_.ShowIn(DumbShowCtx, 0))
	arr = binary.LittleEndian.AppendUint64(arr, e.Expr.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Function abstraction: `fn (x, y) -> x`
type Func struct {
	ArgNames []string
	Body     Expr
	Range    // of the declaration including parameters but not the body
}

// "Func"
func (e *Func) ExprName() string { return "Func" }

func (e *Func) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = e.Body.Transform(f)
	return f(&copied)
}
func (e *Func) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Func")
	arr = binary.LittleEndian.AppendUint64(arr, e.Body.Hash())
	for _, arg := range e.ArgNames {
		_, _ = h.Write([]byte(arg))
	}

	_, _ = h.Write(arr)
	return h.Sum64()
}

// Assign is an expression that sets a Var to an Expr for the rest of the body
// in ile. It is equivalent to a let expression in other languages,
// we just syntactically omit both `let` and `in`
type Assign struct {
	Var   string
	Value Expr
	Body  Expr
	// Recursive can be true for functions only
	Recursive bool
	Range
}

func (e *Assign) ExprName() string { return "Assign" }

func (e *Assign) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = copied.Body.Transform(f)
	copied.Value = copied.Value.Transform(f)
	return f(&copied)
}
func (e *Assign) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Assign" + e.Var + strconv.FormatBool(e.Recursive))
	arr = binary.LittleEndian.AppendUint64(arr, e.Body.Hash())
	arr = binary.LittleEndian.AppendUint64(arr, e.Value.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Unused is like Assign, except it does not store the Value in a Var
// it is useful for calling expressions with side effects
type Unused struct {
	Value Expr
	Body  Expr
	Range
}

func (e *Unused) ExprName() string { return "Unused" }

func (e *Unused) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Body = e.Body.Transform(f)
	copied.Value = e.Value.Transform(f)
	return f(&copied)
}
func (e *Unused) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Unused")
	arr = binary.LittleEndian.AppendUint64(arr, e.Body.Hash())
	arr = binary.LittleEndian.AppendUint64(arr, e.Value.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// ListLiteral can be used to represent lists as well as tuples of known width and subtypes (["aa", 1])
type ListLiteral struct {
	Args []Expr
	Positioner
	inferred Type
}

func (l *ListLiteral) SetType(t Type) { l.inferred = t }

func (l *ListLiteral) ExprName() string { return "list literal" }

func (l *ListLiteral) String() string {
	var exprArgs = make([]string, len(l.Args))
	for i, arg := range l.Args {
		exprArgs[i] = arg.ExprName()
	}
	return "[" + strings.Join(exprArgs, ", ") + "]"
}

func (l *ListLiteral) Transform(f func(expr Expr) Expr) Expr {
	copied := *l
	copied.Args = make([]Expr, len(l.Args))
	for i, arg := range l.Args {
		copied.Args[i] = arg.Transform(f)
	}
	return f(&copied)
}
func (l *ListLiteral) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("ListLiteral")
	for _, arg := range l.Args {
		arr = binary.LittleEndian.AppendUint64(arr, arg.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Grouped let-bindings: `let a = 1 and b = 2 in e`
type LetGroup struct {
	Vars []LetBinding
	Body Expr
	sccs [][]LetBinding
	Range
}

// "LetGroup"
func (e *LetGroup) ExprName() string { return "LetGroup" }

// getCached the strongly connected components inferred for e, in dependency order.
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

func (e *LetGroup) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("LetGroup")
	for _, b := range e.Vars {
		arr = binary.LittleEndian.AppendUint64(arr, b.Value.Hash())
		_, _ = h.Write([]byte(b.Var))
	}
	arr = binary.LittleEndian.AppendUint64(arr, e.Body.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Paired Identifier and value
type LetBinding struct {
	Var   string
	Value Expr
}

// Selecting (scoped) value of label: `r.a`
type RecordSelect struct {
	Record   Expr
	Label    string
	inferred Type
	Range
}

// "RecordSelect"
func (e *RecordSelect) ExprName() string { return "RecordSelect" }

// getCached the inferred (or assigned) type of e.
func (e *RecordSelect) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordSelect) SetType(t Type) { e.inferred = t }

func (e *RecordSelect) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	copied.Record = e.Record.Transform(f)
	return f(&copied)
}
func (e *RecordSelect) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("RecordSelect" + e.Label)
	arr = binary.LittleEndian.AppendUint64(arr, e.Record.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Extending record: `{a = 1, b = 2 | r}`
type RecordExtend struct {
	Record   Expr
	Labels   []LabelValue
	inferred Type
	Range
}

// "RecordExtend"
func (e *RecordExtend) ExprName() string { return "RecordExtend" }

// getCached the inferred (or assigned) type of e.
func (e *RecordExtend) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordExtend) SetType(rt Type) { e.inferred = rt }

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
func (e *RecordExtend) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("RecordExtend")
	for _, v := range e.Labels {
		arr = binary.LittleEndian.AppendUint64(arr, v.Value.Hash())
		_, _ = h.Write([]byte(v.Label))
	}
	arr = binary.LittleEndian.AppendUint64(arr, e.Record.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Paired label and value
type LabelValue struct {
	Label string
	Value Expr
}

func (e *LabelValue) Copy() *LabelValue {
	copied := *e
	return &copied
}

// Deleting (scoped) label: `{r - a}`
type RecordRestrict struct {
	Record   Expr
	Label    string
	inferred *RecordType
	Range
}

// "RecordRestrict"
func (e *RecordRestrict) ExprName() string { return "RecordRestrict" }

// getCached the inferred (or assigned) type of e.
func (e *RecordRestrict) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordRestrict) SetType(rt *RecordType) { e.inferred = rt }

func (e *RecordRestrict) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	e.Record = e.Record.Transform(f)
	return f(&copied)
}
func (e *RecordRestrict) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("RecordRestrict" + e.Label)
	arr = binary.LittleEndian.AppendUint64(arr, e.Record.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Empty record: `{}`
type RecordEmpty struct {
	inferred *RecordType
	Range
}

// "RecordEmpty"
func (e *RecordEmpty) ExprName() string { return "RecordEmpty" }

// getCached the inferred (or assigned) type of e.
func (e *RecordEmpty) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *RecordEmpty) SetType(rt *RecordType) { e.inferred = rt }

func (e *RecordEmpty) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}
func (e *RecordEmpty) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("RecordEmpty")
	_, _ = h.Write(arr)
	return h.Sum64()
}

// Tagged (ad-hoc) variant: `:X a`
type Variant struct {
	Label string
	Value Expr
	Range
}

// "Variant"
func (e *Variant) ExprName() string { return "Variant" }

func (e *Variant) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	e.Value = e.Value.Transform(f)
	return f(&copied)
}
func (e *Variant) Hash() uint64 {
	h := fnv.New64a()
	arr := []byte("Variant" + e.Label)
	arr = binary.LittleEndian.AppendUint64(arr, e.Value.Hash())
	_, _ = h.Write(arr)
	return h.Sum64()
}

// WhenMatch is a Variant-matching switch:
//
//	WhenMatch e {
//	    :X a -> expr1
//	  | :Y b -> expr2
//	  |  ...
//	  | z -> default_expr (optional)
//	}
type WhenMatch struct {
	Value      Expr
	Cases      []WhenCase
	Default    *LabelValue
	inferred   Type
	Positioner // of the match operator and the matched first expression (not the clauses)
}

// "WhenMatch"
func (e *WhenMatch) ExprName() string { return "WhenMatch" }

// getCached the inferred (or assigned) type of e.
func (e *WhenMatch) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *WhenMatch) SetType(t Type) { e.inferred = t }

type WhenCase struct {
	Pattern  MatchPattern
	Value    Expr
	inferred *hmtypes.Record
}

func (e *WhenCase) TransformChildExprs(f func(expr Expr) Expr) WhenCase {
	copied := *e
	copied.Pattern = e.Pattern.TransformChildExprs(f)
	copied.Value = e.Value.Transform(f)
	return copied
}

func (e *WhenMatch) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	cases := make([]WhenCase, len(e.Cases))
	for i, v := range e.Cases {
		cases[i] = v.TransformChildExprs(f)
	}
	if e.Default != nil {
		defaultCase := *e.Default
		copied.Default = &defaultCase
		copied.Default.Value = e.Default.Value.Transform(f)
	}
	copied.Value = e.Value.Transform(f)
	copied.Cases = cases
	return f(&copied)
}
func (e *WhenMatch) Hash() uint64 {
	panic("TODO implement hash for when matching")
	h := fnv.New64a()
	arr := []byte("WhenMatch")
	arr = binary.LittleEndian.AppendUint64(arr, e.Value.Hash())
	for _, v := range e.Cases {
		arr = binary.LittleEndian.AppendUint64(arr, v.Value.Hash())
	}
	if e.Default != nil {
		arr = binary.LittleEndian.AppendUint64(arr, e.Default.Value.Hash())
	}
	_, _ = h.Write(arr)
	return h.Sum64()
}

// MatchPattern describes a types.Type or an Expr that we would like to match against
type MatchPattern interface {
	matchPattern() // marker

	TransformChildExprs(f func(expr Expr) Expr) MatchPattern
	Positioner
}

// ValueLiteralPattern represents matching a value.
// Value can be any Expr but the grammar has restrictions on what Value can be (eg, literals only)
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

// Pipeline: `pipe $ = xs |> fmap($, fn (x) -> to_y(x)) |> fmap($, fn (y) -> to_z(y))`
type Pipe struct {
	Source   Expr
	As       string
	Sequence []Expr
	inferred Type
	Range    // of the first pipe operator?
}

// "Pipe"
func (e *Pipe) ExprName() string { return "Pipe" }

// getCached the inferred (or assigned) type of e.
func (e *Pipe) Type() Type { return e.inferred }

// Assign a type to e. Type assignments should occur indirectly, during inference.
func (e *Pipe) SetType(t Type) { e.inferred = t }

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
}

func (e *ErrorExpr) Type() Type {
	return &NothingType{
		Positioner: e.Range,
	}
}

func (e *ErrorExpr) ExprName() string { return "Error" }
func (e *ErrorExpr) Transform(f func(expr Expr) Expr) Expr {
	copied := *e
	return f(&copied)
}

// Hash generates a structural hash for Expr nodes, excluding location and type information
func (e *Pipe) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("Pipe"))
	_, _ = h.Write([]byte(e.As))

	// Hash source
	if e.Source != nil {
		sourceHash := e.Source.Hash()
		h.Write([]byte(fmt.Sprintf("%d", sourceHash)))
	}

	// Hash sequence of expressions
	for _, expr := range e.Sequence {
		if expr != nil {
			exprHash := expr.Hash()
			_, _ = h.Write([]byte(fmt.Sprintf("%d", exprHash)))
		}
	}

	return h.Sum64()
}

// Hash generates a structural hash for ErrorExpr nodes, excluding location information
func (e *ErrorExpr) Hash() uint64 {
	h := fnv.New64a()
	_, _ = h.Write([]byte("ErrorExpr"))
	_, _ = h.Write([]byte(e.Syntax))
	return h.Sum64()
}
