\title{
MLstruct: Principal Type Inference in a Boolean Algebra of Structural Types (Extended)*
}

\author{
LIONEL PARREAUX, HKUST, Hong Kong, China \\ CHUN YIN CHAU, HKUST, Hong Kong, China
}

\begin{abstract}
Intersection and union types are becoming more popular by the day, entering the mainstream in programming languages like TypeScript and Scala 3. Yet, no language so far has managed to combine these powerful types with principal polymorphic type inference. We present a solution to this problem in MLstruct, a language with subtyped records, equirecursive types, first-class unions and intersections, class-instance matching, and ML-style principal type inference. While MLstruct is mostly structurally typed, it contains a healthy sprinkle of nominality for classes, which gives it desirable semantics, enabling the expression of a powerful form of extensible variants that does not need row variables. Technically, we define the constructs of our language using conjunction, disjunction, and negation connectives, making sure they form a Boolean algebra, and we show that the addition of a few nonstandard subtyping rules gives us enough structure to derive a sound and complete type inference algorithm. With this work, we hope to foster the development of better type inference for present and future programming languages with expressive subtyping systems.
\end{abstract}

CCS Concepts: • Software and its engineering → Functional languages; Polymorphism.
Additional Key Words and Phrases: principal type inference, union and intersection types, structural typing

\section*{1 INTRODUCTION}

Programming languages with ML-style type inference have traditionally avoided subtyping because of the complexities it brings over a simple unification-based treatment of type constraints. But Dolan and Mycroft [2017] recently showed with MLsub that an algebraic account of subtyping resolved many of these difficulties and enabled the inference of precise types that more accurately reflect the flow of expressions in programs. Unfortunately, among other limitations, MLsub does not support union and intersection types, which are emerging as important building blocks in the design of structurally-typed programming languages like TypeScript, Flow, Scala 3, and others.

We close this gap with MLstruct, showing that MLsub-style type inference can be generalized to include well-behaved forms of union and intersection types as well as pattern matching on single-inheritance class hierarchies. As a first example, consider the following definitions:
```
class Some[A]: { value: A }
class None: {}
```

```
def flatMap f opt = case opt of
    Some → f opt.value,
    None → None\{\}
```


The type inferred by our system for flatMap is:
```
flatMap : $\forall \alpha, \beta .(\alpha \rightarrow \beta) \rightarrow($ Some $[\alpha] \vee$ None $) \rightarrow(\beta \vee$ None $)$
```


Interestingly, this is more general than the traditional type given to flatMap for Option types. Indeed, our flatMap does not require the function passed in argument to return either a None or a Some value, but allows it to return anything it wants (any $\beta$ ), which gets merged with the None value returned by the other branch (yielding type $\beta \vee$ None). For example,
```
let res $=$ flatMap $($ fun $\mathrm{x} \rightarrow \mathrm{x})$ (Some\{value $=42\}$ )
```

is given type $42 \vee$ None ${ }^{1}$ because the function may return either 42 or None. A value of this type can later be inspected with an instance match expression of the form:

\footnotetext{
*This is version 8.0 of the paper; get the latest extended version at https://lptk.github.io/mlstruct-paper.
${ }^{1}$ MLstruct supports singleton types for constant literals, e.g., 42 is both a value and a type, with $42: 42 \leqslant$ Nat $\leqslant \operatorname{lnt}$.
}
```
case res of Int → res, None → 0
```

which is inferred to be of type $42 \vee 0$, a subtype of Nat. This is not the most general version of flatMap either. We can also make the function open-ended, accepting either a Some value or anything else, instead of just Some or None, by using a default case (denoted by the underscore '_'):
```
def flatMap2 f opt = case opt of Some → f opt.value, _ → opt
```


This flatMap2 version has the following type inferred, where $\vee$ and $\wedge$ have the usual precedence:
```
flatMap2 : $\forall \alpha, \beta .(\alpha \rightarrow \beta) \rightarrow($ Some $[\alpha] \vee \beta \wedge \neg$ \#Some $) \rightarrow \beta$
```


This type demonstrates a central aspect of our approach: the use of negation types (also called complement types), written $\neg \tau$, which allows us to find principal type solutions in tricky typing situations. Here, type \#Some is the nominal tag of class Some. A nominal tag represents the identity of a class, disregarding the values of its fields and type parameters: if a value $v$ has type \#Some, this means $v$ is an instance of Some, while if $v$ has type ¬ \#Some, this means it is not. To showcase different usages of this definition, consider the following calls along with their inferred types: ${ }^{2}$
```
ex1 $=$ flatMap2 $($ fun $x \rightarrow x+1) 42$ : Int
ex2 $=$ flatMap2 $($ fun $x \rightarrow$ Some\{value $=x\})($ Some\{value $=12\}):$ Some[12]
ex3 $=$ flatMap2 $($ fun $x \rightarrow$ Some\{value $=x\}) 42: \quad$ Some[ $\perp$ ] $\vee 42$
```


It is easy to see that instantiating $\beta$ to Int and Some[12] respectively allows ex 1 and ex 2 to type check. In ex3, both types Some $[\gamma]$ and 42 flow into the result, for some type inference variable $\gamma$, but $\gamma$ is never constrained and only occurs positively so it can be simplified, yielding Some $[\perp] \vee 42$. We can convert ex3 to 42 through a case expression using the impossible helper function: ${ }^{3}$
```
def impossible $x=$ case $x$ of $\} \quad: \quad \perp \rightarrow \perp$
case ex3 of Int → ex3, Some → impossible ex3.value : 42
```


One may naively think that the following type could fit flatMap2 as well:
```
flatMap2_wrong : $\forall \alpha, \beta, \gamma \cdot(\alpha \rightarrow \beta) \rightarrow(\operatorname{Some}[\alpha] \vee \gamma) \rightarrow(\beta \vee \gamma)$
```

but this type does not work. To see why, consider what happens if we instantiate the type variables to $\alpha=\mathrm{Int}, \beta=\mathrm{Int}$, and $\gamma=$ Some[Bool]. This yields the type:
```
flatMap2_wrong ${ }^{\prime}(\operatorname{Int} \rightarrow \operatorname{Int}) \rightarrow($ Some $[\mathrm{Int}] \vee \operatorname{Some}[$ Bool $]) \rightarrow(\mathrm{Int} \vee$ Some[Bool] $)$
```

which would allow the call flatMap2 $($ fun $x \rightarrow x+1)($ Some $\{$ value $=$ false $\})$ because Some $[$ Bool $] \leqslant$ Some[Int] $\vee$ Some[Bool]. This expression, however, would crash with a runtime type mismatch! Indeed, the shape of the some argument matches the first branch of flatMap2's case expression, and therefore false is passed to our argument function, which tries to add 1 to it as though it was an integer... So we do need the negation that appears in the correct type of flatMap2, as it prevents passing in arguments that are also of the some shape, but with the wrong type arguments.

Finally, let us push the generality of our function further yet, to demonstrate the flexibility of the system. Consider this last twist on flatMap for optional values, which we will call mapSome:
```
def mapSome $f$ opt $=$ case opt of Some → $f$ opt, _ → opt
```


The difference with the previous function is that this one does not unwrap the some value received in argument, but simply passes it unchanged to its function argument. Its inferred type is:
$$
\text { mapSome }: \forall \alpha, \beta .(\alpha \rightarrow \beta) \rightarrow(\alpha \wedge \# \text { Some } \vee \beta \wedge \neg \# \text { Some }) \rightarrow \beta
$$

\footnotetext{
${ }^{2}$ Notice that only ex3 features a union of two distinct type constructors 'Some $[\perp] \vee 42$ ' because in ex1 and ex2 only one concrete type constructor statically flows into the result of the expression ( 42 and Some, respectively).
${ }^{3}$ One may expect Some $[\perp] \equiv \perp$, but this does not hold in MLstruct, as it would prevent effective principal type inference.
}

This type shows that it does not matter what specific subtype of Some we have in the first branch: as long as the argument has type $\alpha$ when it is a Some instance, then $\alpha$ is the type the argument function should take, without loss of generality. This demonstrates that our type system can tease apart different flows of values based on the nominal identities of individual matched classes.

As an example of the additional flexibility afforded by this new function, consider the following:
```
class SomeAnd[A, P]: Some[A] ^ { payload: P }
let arg = if <arbitrary condition> then SomeAnd{value = 42, payload = 23}
    else None{}
in mapSome (fun x → x.value + x.payload) arg
```

of inferred type Int ✓ None. Here, we define a new subclass of Some containing an additional payload field, and we use this class instead of some, allowing the payload field to be used from within the function argument we pass to mapSome. This is not expressible in OCaml polymorphic variants [Garrigue 2001] and related systems [Ohori 1995]. More powerful systems with row variables [Pottier 2003; Rémy 1994] would still fail here because of their use of unification: mapsome merges its opt parameter with the result, so these systems would yield a unification error at the mapSome call site, because the argument function returns an integer instead of a value of the same type as the input: ${ }^{4}$ subtyping makes MLstruct more flexible than existing systems based on row variable.

MLscript is a new programming language developed at the Hong Kong University of Science and Technology ${ }^{5}$ featuring first-class unions, intersections, negations, and ML-style type inference, among other features. For simplicity, this paper focuses on a core subset of MLscript referred to as MLstruct, containing only the features relevant to principal type inference in a Boolean algebra of structural types, used in all examples above. An MLstruct implementation is provided as an artifact [Parreaux et al. 2022] and available at github.com/hkust-taco/mlstruct, with a web demonstration at hkust-taco.github.io/mlstruct. The specific contributions we make are the following:
- We present MLstruct (Section 2), which subsumes both the original ML type system and the newer MLsub [Dolan 2017], extending the latter with simple class hierarchies and classinstance matching based on union, intersection, and negation type connectives.
- We describe our approach to type inference based on the Boolean-algebraic properties of MLstruct's types (Section 3). To the best of our knowledge, MLstruct is the first language to support principal polymorphic type inference with union and intersection types. Moreover, it does not rely on backtracking and yields types that are amenable to simplification.
- We formalize the declarative semantics of MLstruct in the $\lambda\urcorner$ calculus (Section 4), making sure to establish the Boolean-algebraic properties of its subtyping lattice (Section 4.4.4). We state the standard soundness properties of progress and preservation, whose complete proofs are given in Appendix B.
- We formally describe our type inference algorithm (Section 5). We state its soundness and completeness theorems. Again, the proofs can be found in Appendix B.

\section*{2 PRESENTATION OF MLSTRUCT}

MLstruct subsumes Dolan's MLsub, the previous state of the art in type inference with subtyping, which itself subsumes traditional ML typing: all ML terms are typeable in MLsub and all MLsub terms are typeable in MLstruct. On top of this fertile ML substrate pollinated with MLsub's rich subtyping theory of records and equirecursive types, MLstruct grows structurally-typed abstractions

\footnotetext{
${ }^{4}$ Wrapping the result in Some would not work either (as Some Int doesn't unify with Some \{value: Int, payload: Int\}).
${ }^{5}$ The GitHub repository of the full MLscript language is available at https://github.com/hkust-taco/mlscript.
}
in the form of unions, intersections, negations, structural class types, and class-instance matching. We now present these features along with some examples.

\subsection*{2.1 Overview of MLscript Features}

An MLstruct program is made of top-level statements followed by an expression, the program's body. A statements can be either a type declaration (class or type alias) or a top-level function definition, written def $f=t$ or rec $\operatorname{def} f=t$ when $f$ is recursive. MLstruct infers polymorphic types for def bindings, allowing them to be used at different type instantiations in the program.
2.1.1 Polymorphism. Polymorphic types include a set of type variables with bounds, such as $\forall(\alpha \leqslant \operatorname{lnt})$. $\operatorname{List}[\alpha] \rightarrow \operatorname{List}[\alpha]$. The bounds of polymorphic types are allowed to be cyclic, which can be interpreted as indirectly describing recursive types. For example, $\forall(\alpha \leqslant \top \rightarrow \alpha)$. $\alpha$ is the principal type scheme of rec def $\mathrm{f}=\mathrm{fun} \mathrm{a} \rightarrow \mathrm{f}$ which accepts any argument and returns itself. To simplify the presentation of inferred polymorphic types with recursive bounds, such as $\forall(\alpha \leqslant \alpha \rightarrow \beta), \beta . \alpha \rightarrow \beta$, we may use an equivalent 'as' shorthand, as follows: $\forall \beta .((\alpha \rightarrow \beta)$ as $\alpha) \rightarrow \beta$.

MLstruct applies aggressive simplification on inferred types, removing redundant type variables and inlining simple type variable bounds (see Section 3.5), so that they are usually fairly concise.
2.1.2 Classes, Inheritance, and Type Aliases. Because object orientation is not the topic of this paper, which focuses on functional-style use cases, the basic OO constructs of MLstruct presented here are intentionally bare-bone. Classes are declared with the following syntax:
class $C[A, B, \ldots]: D[S, T, \ldots] \wedge\{x: X, y: Y, \ldots\}$
where $A$, $B$, etc. are type parameters, $S, T, X, Y$, etc. are arbitrary types and $D$ is the parent class of $C$, which can be left out if the class has no parents. Along with a type constructor $\mathrm{C}[A, B, \ldots]$, such a declaration also introduces a data constructor C of type:
$$
\mathrm{C}: \forall \beta_{1}, \beta_{2}, \ldots,\left(\alpha_{1} \leqslant \tau_{1}\right),\left(\alpha_{2} \leqslant \tau_{2}\right), \ldots .\left\{x_{1}: \alpha_{1}, x_{2}: \alpha_{2}, \ldots\right\} \rightarrow \mathrm{C}\left[\beta_{1}, \beta_{2} \ldots\right] \wedge\left\{x_{1}: \alpha_{1}, x_{2}: \alpha_{2}, \ldots\right\}
$$
where $x_{i}$ are all the fields declared by $\mathrm{C}\left[\beta_{1}, \beta_{2}, \ldots\right]$ or by any of its ancestors in the inheritance hierarchy, and $\tau_{i}$ are the corresponding types - if a field is declared in several classes of the hierarchy, we take the intersection of all the declared types for that field. To retain as precise typing as possible, we let the types of the fields taken in parameters to be arbitrary subtypes $\alpha_{i}$ of the declared $\tau_{i}$, so we can refine the result type $\mathrm{C}\left[\beta_{1}, \beta_{2} \ldots\right] \wedge\left\{x_{1}: \alpha_{1}, x_{2}: \alpha_{2}, \ldots\right\}$ to retain these precise types. For instance, assuming class C : $\{\mathrm{x}$ : Int $\}$, term $\mathrm{C}\{\mathrm{x}=1\}$ is given the precise type $\mathrm{C} \wedge\{x: 1\}$.

Classes are restricted to single-inheritance hierarchies. Like in the work of Muehlboeck and Tate [2018], this has the nice property that it allows union types to be refined by reducing types like $\left(C_{0} \vee \tau\right) \wedge C_{1}$ to $C_{0} \wedge C_{1} \vee \tau \wedge C_{1}$ by distributivity and to just $\tau \wedge C_{1}$ when $C_{0}$ and $C_{1}$ are unrelated $\left(C_{0} \wedge C_{1} \equiv \perp\right)$. But MLstruct can easily be extended to support traits, which are not subject to this restriction, by slightly adapting the definition of type normal forms (our artifact [Parreaux et al. 2022] implements this). Thanks to their use of negation types (described in Section 4.3), the typing rules for pattern matching do not even have to change, and traits can also be pattern-matched. In fact, the full MLscript language supports mixin trait composition [Schärli et al. 2003] similar to Scala [Odersky et al. 2004], whereby traits can be inherited alongside classes, and method overriding is resolved in so-called "linearization order."
2.1.3 Shadowing. Non-recursive defs use shadowing semantics, ${ }^{6}$ so they can simulate the more traditional field initialization and overriding semantics of traditional class constructors. For instance:
class Person: \{name: Str, age: Nat, isMajor: Bool\}
def Person n a = Person\{name = capitalize n, age = a, isMajor = a >= 18\}

\footnotetext{
${ }^{6}$ Type names, on the other hand, live in a different namespace and are not subject to shadowing.
}
in which the def, of inferred type Person ${ }_{1}: \forall(\alpha \leqslant \mathrm{Nat})$. Str $\rightarrow \alpha \rightarrow$ Person $\wedge\{$ age : $\alpha\}$, shadows the bare constructor of the Person class (of type Person ${ }_{0}: \forall(\alpha \leqslant \operatorname{Str}),(\beta \leqslant \mathrm{Nat}),(\gamma \leqslant$ Bool). $\{$ name : $\alpha$, age : $\beta$, isMajor : $\gamma\} \rightarrow$ Person $\wedge\{$ name : $\alpha$, age : $\beta$, is Major : $\gamma\}$ ), forcing users of the class to go through it as the official Person constructor. Function capitalize returns a Str, so no ‘name’ refinement is needed(Person $\wedge\{$ age : $\alpha$, name : Str $\} \equiv \operatorname{Person} \wedge\{$ age : $\alpha\}$ ).
2.1.4 Nominality. Classes are not equivalent to their bodies. Indeed, they include a notion of "nominal identity", which means that while a class type is a subtype of its body, it is not a supertype of it. So unlike TypeScript, it is not possible to use a record $\{x=1\}$ as an instance of a class declared as class $C$ : $\{x$ : Int $\}$. To obtain a $C$, one must use its constructor, as in $C\{x=1\}$. This nominality property is a central part of our type system and is much demanded by users in practice. ${ }^{7}$ It comes at no loss of generality, as type synonyms can be used if nominality is not wanted.
2.1.5 Type Aliases. Arbitrary types can be given names using the syntax type $\mathrm{X}[\mathrm{A}, \mathrm{B}, \ldots]=\mathrm{T}$. Type aliases and classes can refer to each other freely and can be mutually recursive.
2.1.6 Guardedness Check. Classes and type aliases are checked to ensure they do not inherit or refer to themselves immediately without going through a "concrete" type constructor first (i.e., a function or record type). For instance, the recursive occurrence of A in type $\mathrm{A}[\mathrm{X}]=\mathrm{Id}[\mathrm{A}[\mathrm{X}]] \vee \operatorname{Int}$ where type $\mathrm{Id}[\mathrm{Y}]=\mathrm{Y}$ is unguarded and thus illegal, but type $\mathrm{A}[\mathrm{X}]=\{\mathrm{x}: \mathrm{A}[\mathrm{X}]\} \vee$ Int is fine.
2.1.7 Class-Instance Matching. As presented in the introduction, one can match values against class patterns in a form of primitive pattern matching. Consider the following definitions:
```
    class Cons[A]: Some[A] $\wedge$ \{ tail: List[A] \} type List[A] = Cons[A] $\vee$ None
    rec def mapList f ls = case ls of
        Cons → Cons\{value = f ls.value, tail = mapList f ls.tail\},
        None → None\{\}
of inferred type: ${ }^{8} \quad$ mapList : $\forall \alpha, \beta .(\alpha \rightarrow \beta) \rightarrow(\operatorname{Cons}[\alpha] \wedge\{$ tail : $\gamma\} \vee$ None $)$ as $\gamma \rightarrow$
            $(\operatorname{Cons}[\beta] \wedge\{$ tail : $\delta\} \vee$ None $)$ as $\delta$
```


We define a List type using None as the "nil" list and whose cons constructor extends Some (from the introduction). A list in this encoding can be passed to any function that expects an option in input - if the list is a Cons instance, it is also a Some instance, and the value field representing the head of the list will be used as the value wrapped by the option. This example demonstrates that structural typing lets us mix and match as well as refine different constructors in a flexible way.

As a slightly bigger motivating example, the List type thus defined can then be used as follows, defining the classical unzip combinator:
```
def Cons head tail = Cons \{ value = head, tail = tail \} def None = None\{\}
rec def unzip xs = case xs of
    None → \{ fst = None, snd = None \},
    Some → let tmp = unzip xs.tail in \{ fst = Cons xs.value.fst tmp.fst ,
        snd = Cons xs.value.snd tmp.snd \}
```


Below are two possible types that may be annotated explicitly by the user for these definitions, and which will be automatically checked by MLstruct for conformance (a.k.a., subsumption, see Section 3.4) against their inferred types. ${ }^{9}$

\footnotetext{
${ }^{7}$ The lack of nominal typing for classes has been a major pain point in TypeScript. The issue requesting it, created in 2014 and still not resolved, has accumulated more than 500 "thumbs up". See: https://github.com/Microsoft/Typescript/issues/202. ${ }^{8}$ The where keyword is used to visually separate the specification of type variable bounds, making them more readable. ${ }^{9}$ Annotating the types of public functions, while not required by MLstruct, is seen as good practice in some communities. Moreover, the subsumption mechanism can be used to provide and check module signatures in an ML-style module system.
}
```
def Cons: $\alpha \rightarrow(\beta \wedge \operatorname{List}[\alpha]) \rightarrow(\operatorname{Cons}[\alpha] \wedge\{$ value: $\alpha$, tail: $\beta\})$
def unzip: List[\{ fst: $\alpha$, snd: $\beta$ \}] → \{ fst: List[ $\alpha$ ], snd: List[ $\beta$ ] \}
```


\subsection*{2.2 Constructing the Lattice of Types}

The algebraic subtyping philosophy of type system design is to begin with the subtyping of data types (records, functions, etc.) and to define the order connectives to fit this subtyping order, rather than to follow set-theoretic intuitions. We follow this philosophy and aim to design our subtyping order to tackle the following design constraints:
(A) The order connectives $\wedge, \vee$, and $\neg$ should induce a Boolean algebra, so that we can manipulate types using well-known and intuitive Boolean-algebraic reasoning techniques.
(B) Nominal tags and their negations specifically should admit an intuitive set-theoretic understanding, in the sense that for any class $C$, type $\# C$ should denote all instances of $C$ while type $\neg \# C$ should correspondingly denote all instances that are not derived from class $C .^{10}$
(C) The resulting system should admit principal types as well as an effective polymorphic type inference strategy, where "effective" means that it should not rely on backtracking.
2.2.1 Lattice Types. Top, written T, is the type of all values, a supertype of every other type. Its dual bottom, written ⟂ , is the type of no values, a subtype of every other type. For every $\tau$, we have $\perp \leqslant \tau \leqslant \top$. Intersection $\wedge$ and union $\vee$ types are the respective meet and join operators in the subtyping lattice. It is worth discussing possible treatments one can give these connectives:
(1) We can axiomatize them as denoting the intersection $\cap$ and union $\cup$ of the sets of values that their operands denote, which is the approach taken by semantic subtyping.
(2) We can axiomatize them as greatest lower bound (GLB) and least upper bound (LUB) operators, usually written $\sqcap$ and $\sqcup$, whose meaning is given by following the structure of a preexisting lattice of simple types (types without order connectives). In this interpretation, we can calculate the results of these operators when their operands are concretely known.
(3) Finally, we can view $\wedge$ and $\vee$ as type constructors in their own right, with dedicated subtyping derivation rules. Then unions and intersections are not "computed away" but instead represent proper constructed types, which may or may not be equivalent to existing simple types.
2.2.2 Subtyping. We base our approach primarily on (3) but we do include a number of subtyping rules whose goal is to make the order connectives behave like (2) in some specific cases:
- We posit $\# C_{1} \wedge \# C_{2} \leqslant \perp$ whenever classes $C_{1}$ and $C_{2}$ are unrelated. ${ }^{11}$ This makes sense because there are no values that can be instances of both classes at the same time, due to single inheritance. We obviously also have $\# C_{1} \wedge \# C_{2} \geqslant \perp$, meaning the two sides are equivalent (they subtype each other), which we write $\# C_{1} \wedge \# C_{2} \equiv \perp$. On the other hand, $\# C \leqslant \# D$ for all $C, D$ where $C$ inherits from $D$; so when $\# C_{1}$ and $\# C_{2}$ are related then either $\# C_{1} \wedge \# C_{2} \equiv \# C_{1}$ or $\# C_{1} \wedge \# C_{2} \equiv \# C_{2}$. Overall, we can always "reduce" intersections of nominal class tags to a single non-intersection type, making $\wedge$ behave like a GLB operator in the class inheritance sublattice, made of nominal tags, $\triangleleft, \perp$, and $\vee$, evocative of (2).
- We also posit the nonstandard rule $\left(\tau_{1} \rightarrow \tau_{2}\right) \wedge\left(\tau_{3} \rightarrow \tau_{4}\right) \leqslant\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)$. The other direction holds by function parameter contravariance and result covariance, so again the two sides are made equivalent. $\wedge$ behaves like a GLB operator on function types in a lattice which does not contain subtyping-based overloaded functions types, such as those of Dolan [2017]; Pottier [1998b]. This rule is illogical from the set-theoretic point of view: a function that can be viewed as returning a $\tau_{2}$ when given a $\tau_{1}$ and returning a $\tau_{4}$ when given a $\tau_{3}$ cannot be

\footnotetext{
${ }^{10}$ By contrast, we have no specific requirements on the meaning of negated function and record types, which are uninhabited.
${ }^{11}$ This class intersection annihilation rule is not novel; for example, Ceylon has a similar one [Muehlboeck and Tate 2018].
}
viewed as always returning a $\tau_{2} \wedge \tau_{4}$. For instance, consider $\lambda x . x$, typeable both as $\operatorname{Int} \rightarrow \operatorname{Int}$ and as Bool ⟶ Bool. According to both classical intersection type systems and the semantic subtyping interpretation, this term could be assigned type ( $\operatorname{Int} \rightarrow \operatorname{Int}) \wedge($ Bool $\rightarrow$ Bool). But we posited that this type is equivalent to $(\operatorname{Int} \vee \operatorname{Bool}) \rightarrow(\operatorname{Int} \wedge \operatorname{Bool})$. Thankfully, in MLstruct $\lambda x . x$ cannot be assigned such an intersection type; instead, its most general type is $\forall \alpha . \alpha \rightarrow \alpha$, which does subsume both Int → Int and Bool → Bool, but not $($ Int → Int $) \wedge($ Bool → Bool $)$. This explains why intersection types cannot be used to encode overloading in MLstruct. ${ }^{12}$
- For record intersections, we have the standard rule that $\{x: \tau\} \wedge\{x: \pi\} \leqslant\{x: \tau \wedge \pi\}$, making the two sides equivalent since the other direction holds by depth subtyping. Intersections of distinct record fields, on the other hand, do not reduce and stay as they are - in fact, multi-field record types are encoded, in MLstruct, as intersections of individual single-field record types, following Reynolds [1997]. For instance, assuming $x \neq y$, then $\left\{x: \tau_{1}, y: \tau_{2}\right\}$ is not a core form but merely syntax sugar for $\left\{x: \tau_{1}\right\} \wedge\left\{y: \tau_{2}\right\}$.
- We apply similar treatments to various forms of unions: First, $\left(\tau_{1} \rightarrow \tau_{2}\right) \vee\left(\tau_{3} \rightarrow \tau_{4}\right) \equiv \left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right)$, the dual of the function intersection treatment mentioned above. Second, we recognize that $\{x: \tau\} \vee\{y: \pi\}$ and $\{x: \tau\} \vee\left(\pi_{1} \rightarrow \pi_{2}\right)$, where $x \neq y$, cannot be meaningfully used in a program, as the language has no feature allowing to tease these two components apart, so we identify these types with T , the top type. This is done by adding $\top \leqslant\{x: \tau\} \vee\{y: \pi\}$ and $\top \leqslant\{x: \tau\} \vee\left(\pi_{1} \rightarrow \pi_{2}\right)$ as subtyping derivation rules.
The full specification of our subtyping theory is presented later, in Section 4 (Figure 4).
2.2.3 Soundness. The soundness of subtyping disciplines was traditionally studied by finding semantic models corresponding to types and subtyping, where types are typically understood as predicates on the denotations of $\lambda$ terms (obtained from some $\lambda$ model) and where subtyping is understood as inclusion between the corresponding sets of denotations. In this paper, we take a much more straightforward approach: all we require from the subtyping relation is that it be consistent, in the sense that it correctly relate types constructed from the same constructors and that it not relate unrelated type constructors. For instance, $\tau_{1} \rightarrow \tau_{2} \leqslant \pi_{1} \rightarrow \pi_{2}$ should hold if and only if $\pi_{1} \leqslant \tau_{1}$ and $\tau_{2} \leqslant \pi_{2}$, and $\{x: \operatorname{lnt}\} \leqslant \# C$ should not be derivable. This turns out to be a sufficient condition for the usual soundness properties of progress and preservation to hold in our language. Consistency is more subtle than it may first appear. We cannot identify, e.g., $\# C \vee\{x: \tau\}$ with $T$ even though the components of this type cannot be teased apart through instance matching, as doing so is incompatible with distributivity. Notice the conjunctive normal form of $\pi=\# C \wedge\{x: \tau\} \vee \# D \wedge\left\{y: \tau^{\prime}\right\}$ is $\pi \equiv(\# C \vee \# D) \wedge\left(\# C \vee\left\{y: \tau^{\prime}\right\}\right) \wedge(\{x: \tau\} \vee \# D) \wedge(\{x: \tau\} \vee\left\{y: \tau^{\prime}\right\}$ ). We can make $\{x: \tau\} \vee\left\{y: \tau^{\prime}\right\}$ equivalent to T when $x \neq y$ because that still leaves $\pi \equiv(\# C \vee \# D) \wedge\left(\# C \vee\left\{y: \tau^{\prime}\right\}\right) \wedge(\{x: \tau\} \vee \# D)$, which is equivalent to the original $\pi$ by distributivity and simplification. But making $\# C \vee\left\{y: \tau^{\prime}\right\}$ and $\{x: \tau\} \vee \# D$ equivalent to T would make $\pi \equiv \# C \vee \# D$, losing all information related to the fields, and breaking pattern matching!
2.2.4 Records. Record values are built using the syntax $\{x 1=t 1, x 2=t 2, \ldots\}$ and are assigned the corresponding types $\left\{x_{1}: \tau_{1}, x_{2}: \tau_{2}, \ldots\right\}$. Record types are related via the usual width and depth subtyping relationships. Width subtyping means that, for instance, $\left\{x: \tau_{1}, y: \tau_{2}\right\} \leqslant\left\{x: \tau_{1}\right\}$, and depth subtyping means that, for instance, $\left\{x: \tau_{1}, y: \tau_{2}\right\} \leqslant\left\{x: \tau_{1}, y: \tau_{3}\right\}$ if $\tau_{2} \leqslant \tau_{3}$.
2.2.5 Negation Types. Finally, we can add Boolean-algebraic negation to our subtyping lattice. However, its interpretation is considerably constrained by the Boolean structure and by the rules already presented in Section 2.2.2. In some languages, the values of a negation type $\neg \tau$ are intuitively

\footnotetext{
${ }^{12}$ Other forms of overloading, such as type classes and constructor overloading (see Section 6), are still possible.
}
understood as all values that are not of the negated type $\tau$, but in MLstruct, this intuition only holds for nominal tags. For other constructs, such as functions and records, negations assume a purely algebraic role. For instance, we have relationships like $\neg\{x: \tau\} \leqslant \pi_{1} \rightarrow \pi_{2}$ due to $\{x: \tau\} \vee \pi_{1} \rightarrow \pi_{2}$ being identified with $T$ (see also Section 4.4.5). Because no values inhabit types like $\neg\{x: \tau\}$ and $\neg\left(\pi_{1} \rightarrow \pi_{2}\right)$, these types should be essentially thought of as special bottom types that, for algebraic reasons, technically have to contain more static information than ⟂ and have to possess fewer subtyping relationships.

Negations can express interesting patterns, such as safe division, as seen below, where 'e: T' is used to ascribe a type T to an expression e:
```
def div $n m=n /(m: \operatorname{Int} \wedge \neg 0) \quad$ def $f \times=\operatorname{div} \times 2$
div: Int → (Int $\wedge \neg 0) \rightarrow$ Int f: Int → Int
def $g(x:$ Int $)=\operatorname{div} 100 x \quad$ error: found Int, expected Int $\wedge \neg 0$
def div_opt $n m=$ case $m$ of $0 \rightarrow$ None\{\}, _ → Some\{value = div n $m$ \}
div_opt: Int → Int → (None v Some[Int])
```


Here, 'case $m$ of ...' is actually a shorthand for the core form 'case $m=m$ of ...' which shadows the outer $m$ with a local variable $m$ that is assigned a more refined type in each case branch.

As we saw in the introduction, ✓ also allows for the sound typing of class-instance matching with default cases. Moreover, together with $\mathrm{T}, \perp, \wedge$, and $\vee$, our type structure forms a Boolean lattice, whose algebraic properties are essential to enabling principal type inference (see Section 3.3.1).
2.2.6 Structural Decomposition. We reduce complex object types to simpler elementary parts, which can be handled in a uniform way. Similarly to type aliases, which can always be replaced by their bodies, we can replace class types by their fields intersected with the corresponding nominal tags. For example, Cons $[\tau]$ as defined in Section 2.1.7 reduces to \#Cons $\wedge\{$ value : $\tau$, tail : $\operatorname{List}[\tau]\}$. Recall that class tags like \#Cons represent the nominal identities of classes. They are related with other class tags by a subtyping relationship that follows the inheritance hierarchy. For instance, given class $C[\alpha]: D[\alpha \vee 2] \wedge\{x: 0 \vee \alpha\}$ and class $D[\beta]:\{x: \beta, y: \operatorname{lnt}\}$, then we have \#C $\leqslant \# D$. Moreover, the refined class type $C[1] \wedge\{y:$ Nat $\}$ reduces to the equivalent $\# C \wedge\{x: 0 \vee 1\} \wedge\{x$ : $1 \vee 2, y: \ln t\} \wedge\{y:$ Nat $\}$, which reduces further to $\# C \wedge\{x: 1, y:$ Nat $\}$.

Decomposing class types into more elementary types makes MLstruct's approach fundamentally structural, while retaining the right amount of nominality to precisely reflect the semantics of runtime class-instance matching (i.e., pattern matching based on the runtime class of objet values). It also means that there is no primitive notion of nominal type constructor variance in MLstruct: the covariance and contravariance of type parameters simply arise from the way class and alias types desugar into basic structural components.

\subsection*{2.3 Limitations}

While MLstruct features very flexible and powerful type inference, this naturally comes with some limitations, necessary to ensure the decidability and tractability of the type system. We already mentioned in Section 2.2.2 that intersections cannot be used to type overloading. Here we explain several other significant limitations.
2.3.1 Regular Structural Types. We restrict the shapes of MLstruct data types to be regular trees to make the problem of deciding whether one subsumes another decidable: concretely, occurrences of a class or alias type transitively reachable through the body of that type must have the same shape as the type's head declaration. For instance, the following are disallowed:
```
class C[A]: {x: C[Int]} class C[A]: C[{x: List[A]}] class C[A]: {x: C[C[A]]}
```


We conjecture that allowing such definitions would give our types the expressive power of contextfree grammars, for which language inclusion is undecidable, making subtyping undecidable. ${ }^{13}$ To replace illegal non-regular class fields, one can use either top-level functions or methods. The latter solve this problem by having their types known in advance and not participating in structural subtype checking. Methods are implemented in MLstruct but not presented in this paper.
2.3.2 Simplified Treatment of Unions. MLstruct keeps the expressiveness of unions in check by identifying $\left\{x: \tau_{1}\right\} \vee\left\{y: \tau_{2}\right\}(x \neq y)$ and $\left\{x: \tau_{1}\right\} \vee\left(\tau_{2} \rightarrow \tau_{3}\right)$ with T , as described in Section 2.2.2. To make unions of different fields useful, one needs to "tag" the different cases with class types, as in $C_{1} \wedge\left\{x: \tau_{1}\right\} \vee C_{2} \wedge\left\{y: \tau_{2}\right\}$, allowing us to separately handle these cases through instance matching 'case $v$ of $C_{1} \rightarrow \ldots v . x \ldots, C_{2} \rightarrow \ldots v . y \ldots$, whereas this is not necessary in, e.g., TypeScript.

A direct consequence of this restriction is that in MLstruct, there is no difference between $\{x: \operatorname{Int}, y: \operatorname{Int}\} \vee\{x: \operatorname{Str}, y: \operatorname{Str}\}$ and $\{x: \operatorname{Int} \vee \operatorname{Str}, y: \operatorname{Int} \vee \operatorname{Str}\}$ (still assuming $x \neq y$ ). Indeed, remember that $\left\{x: \tau_{1}, y: \tau_{2}\right\}$ is syntax sugar for $\left\{x: \tau_{1}\right\} \wedge\left\{y: \tau_{2}\right\}$ and by distributivity of unions over intersections, we can take $\{x: \operatorname{Int}, y: \operatorname{Int}\} \vee\{x: \operatorname{Str}, y: \operatorname{Str}\}$ to
$(\{x: \operatorname{Int}\} \vee\{x: \operatorname{Str}\}) \wedge(\{x: \operatorname{Int}\} \vee\{y: \operatorname{Str}\}) \wedge(\{y: \operatorname{Int}\} \vee\{x: \operatorname{Str}\}) \wedge(\{y: \operatorname{Int}\} \vee\{y: \operatorname{Str}\})$ and since $\left\{x: \tau_{1}\right\} \vee\left\{y: \tau_{2}\right\}$ is identified with T , as explained in Section 2.2.2, this reduces to
$$
(\{x: \operatorname{Int}\} \vee\{x: \operatorname{Str}\}) \wedge(\{y: \operatorname{Int}\} \vee\{y: \operatorname{Str}\})
$$
which reduces by field merging to $\{x: \operatorname{Int} \vee \operatorname{Str}\} \wedge\{y: \operatorname{Int} \vee \operatorname{Str}\}$, i.e., $\{x: \operatorname{Int} \vee \operatorname{Str}, y: \operatorname{Int} \vee \operatorname{Str}\}$.
Another consequence is that, e.g., List[Int] $\vee$ List[Str] is identified with List[Int $\vee$ Str]. Again, to distinguish between these two, one should prefer the use of class-tagged unions or, equivalently, proper sum types such as Either[List[Int], List[Str]], defined in terms of Left and Right classes.
2.3.3 Fewer Relationships. Unlike in semantic subtyping approaches, but like in most practical programming languages, we do not have $\{x: \perp\} \leqslant \perp$. This would in fact lead to unsoundness in MLstruct: consider $\pi=\left(\left\{x:\right.\right.$ Some[Int], $\left.y: \tau_{1}\right\} \vee\left\{x:\right.$ None, $\left.\left.y: \tau_{2}\right\}\right) \wedge\{x:$ None $\}$; we would have $\pi \equiv\left\{x: \perp, y: \tau_{1}\right\} \vee\left\{x:\right.$ None, $\left.y: \tau_{2}\right\} \equiv\left\{x:\right.$ None, $\left.y: \tau_{2}\right\}$ by distributivity and also $\pi \equiv\left\{x: \perp \vee\right.$ None, $\left.y: \tau_{1} \vee \tau_{2}\right\}$ by using (2.3.2) before distributing, but $\tau_{1} \not \equiv \tau_{1} \vee \tau_{2}$ in general.
2.3.4 No intersection overloading. Unlike languages like TypeScript, we do not permit the use of intersection types to encode inclusive function overloading [Pierce 1991]. Thankfully, simpler forms of overloading compatible with MLstruct exist; we briefly discuss one in Section 6.

\section*{3 INFERRING PRINCIPAL TYPES FOR MLSTRUCT}

We now informally describe our general approach to principal type inference in MLstruct.

\subsection*{3.1 Algebraic Subtyping}

MLstruct follows Dolan's algebraic subtyping [2017] discipline, which distinguishes itself from so-called semantic subtyping approaches in that it focuses on the algebraic properties of types, instead of focusing on set-theoretic semantics. In algebraic subtyping, some subtyping relationships are not necessary and cannot be justified if one were to look at types purely as denotations for sets of values. These algebraic relationships are nevertheless sound to have in the type system, and in turn enable principal type inference and type simplification.

As an example, consider $\left(\tau_{1} \rightarrow \tau_{2}\right) \wedge\left(\tau_{3} \rightarrow \tau_{4}\right) \leqslant\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)$, which holds in Dolan's MLsub. While the other direction holds by simple contravariance of function parameters and covariance of function results, this direction is a lot more contentious. It does not make sense from the set-theoretic point of view: a function that can be viewed as returning $\tau_{2}$ when given a $\tau_{1}$ and

\footnotetext{
${ }^{13}$ TypeScript does allow such definitions, meaning its type checker would necessarily be either unsound or incomplete.
}
returning $\tau_{4}$ when given a $\tau_{3}$ cannot be viewed as always returning a $\tau_{2} \wedge \tau_{4}$. For instance, consider $\lambda x . x$, typable both as Int $\rightarrow$ Int and as Bool $\rightarrow$ Bool, and which could therefore be assigned type $(\operatorname{lnt} \rightarrow \operatorname{lnt}) \wedge$ (Bool → Bool). Surely, this function never returns an Int $\wedge$ Bool value (an uninhabited type) when called with an Int $\vee$ Bool argument. But in MLsub, $\lambda x . x$ by design cannot be assigned such an intersection type; instead, its most general type is $\forall \alpha . \alpha \rightarrow \alpha$, which does subsume both Int → Int and Bool → Bool though not (Int → Int) $\wedge$ (Bool → Bool). This explains the restriction that intersections cannot be used to encode overloading in MLsub and MLstruct.

In MLstruct, we define further additional algebraic subtyping relationships, such as $\top \leqslant\{x$ : $\left.\tau_{1}\right\} \vee\left(\tau_{2} \rightarrow \tau_{3}\right)$, as hinted in Section 2.3.2. We similarly ensure that this relationship does not threaten soundness by making sure the language cannot meaningfully distinguish between values of these two types (i.e., one cannot pattern match on record or function types).

\subsection*{3.2 Basic Type Inference Idea}

We base the core of our type inference algorithm on a simple formulation of MLsub type inference we formulated in previous work [Parreaux 2020]. The constraint solver attaches a set of lower and upper bounds to each type variable, and maintain the transitive closure of these constraints, i.e., it makes sure that at all times the union of all lower bounds of a variable remains a subtype of the intersection of all its upper bounds. This means that when registering a new constraint of the form $\alpha \leqslant \tau$, we not only have to add $\tau$ to the upper bounds of $\alpha$, but also to constrain lowerBounds $(\alpha) \leqslant \tau$ in turn. One has to be particularly careful to maintain a "cache" of subtyping relationships currently being constrained, as the graphs formed by type variable bounds may contain cycles. Because types are regular, there is always a point, in a cyclic constraint, where we end up checking a constraint we are already in the process of checking (it is in the cache), in which case we can assume that the constraint holds and terminate. Constraints of the general form $\tau_{1} \leqslant \tau_{2}$ are handled by losslessly decomposing them into smaller constraints, until we arrive at constraints on type variables, which is made possible by the algebraic subtyping rules. The losslessness of this approach is needed to ensure that we only infer principal types. In other words, when decomposing a constraint, we must produce a set of smaller constraints that is equivalent to the original constraint. For example, we can decompose the constraint $\tau_{1} \vee\left(\tau_{2} \rightarrow \tau_{3}\right) \leqslant \tau_{4} \rightarrow \tau_{5}$ into the equivalent set of constraints: $\tau_{1} \leqslant \tau_{4} \rightarrow \tau_{5} ; \tau_{4} \leqslant \tau_{2} ;$ and $\tau_{3} \leqslant \tau_{5}$. If we arrive at a constraint between two incompatible type constructors, such as $\tau_{1} \rightarrow \tau_{2} \leqslant\left\{x: \tau_{3}\right\}$, an error is reported.

\subsection*{3.3 Solving Constraints with Unions and Intersections}

By contrast with MLsub, MLstruct supports union and intersections types in a first-class capacity, meaning that one can use these types in both positive and negative positions. ${ }^{14}$ This is particularly important to type check instance matching, which requires unions in negative positions, and class types, which require intersections in positive positions (both illegal in MLsub).

The main problem that arises in this setting is: How to resolve constraints with the shapes $\tau_{1} \leqslant \tau_{2} \vee \tau_{3}$ and $\tau_{1} \wedge \tau_{2} \leqslant \tau_{3}$ ? Such constraints cannot be easily decomposed into simpler constraints without losing information - which would prevent us from achieving complete type inference - and without having to perform backtracking - which would quickly become intractable, even in non-pathological cases, and would yield a set of possible types instead of a single principal

\footnotetext{
${ }^{14}$ Positive positions correspond to the types that a term outputs, while negative positions correspond to the types that a term takes in as input. For instance, in $\left(\tau_{0} \rightarrow \tau_{1}\right) \rightarrow \tau_{2}$, type $\tau_{2}$ is in positive position since it is the output of the main function, and the function type ( $\tau_{0} \rightarrow \tau_{1}$ ) is in negative position, as it is taken as an input to the main function. On the other hand, $\tau_{1}$, which is returned by the function taken as input is in negative position (since it is provided by callers via the argument function), and $\tau_{0}$ is in positive position (since it is provided by the main function when calling the argument function).
}
type. When faced with such constraints, we distinguish two cases: (1) there is a type variable among $\tau_{1}, \tau_{2}$, and $\tau_{3}$; and (2) conversely, none of these types are type variables.
3.3.1 Negation Types. We use negation types to reformulate constraints involving type variables into forms that allow us to make progress, relying on the Boolean-algebraic properties of negation. A constraint such as $\tau_{1} \leqslant \tau_{2} \vee \alpha$ can be rewritten to $\tau_{1} \wedge \neg \tau_{2} \leqslant \alpha$ by turning the "positive" $\tau_{2}$ on the right into a "negative" on the left, as these are equivalent in a Boolean algebra. ${ }^{15}$ Therefore, it is sufficient and necessary to constrain $\alpha$ to be a supertype of $\tau_{1} \wedge \neg \tau_{2}$ to solve the constraint at hand. Similarly, we can solve $\alpha \wedge \tau_{1} \leqslant \tau_{2}$ by constraining $\alpha$ to be a subtype of $\tau_{2} \vee \neg \tau_{1}$. ${ }^{16}$ When both transformations are possible, one may pick one or the other equivalently. The correctness of these transformations is formally demonstrated in Theorem B.20.. This approach provides a solution to case (1), but in a way it only pushes the problem around, delaying the inevitable apparition of case (2).
3.3.2 Normalization of Constraints. To solve problem (2), we normalize constraints until they are in the shape " $\tau_{\text {con }} \leqslant \tau_{\text {dis }}$ ", where (using a horizontal overline to denote 0 to $n$ repetitions):
- $\tau_{\text {con }}$ represents $\mathrm{T}, \perp$, or the intersection of any non-empty subset of $\left\{\# C, \tau_{1} \rightarrow \tau_{2},\{\overline{x: \tau}\}\right\}$.
- $\tau_{\text {dis }}$ represents types of the form $\mathrm{T}, \perp,\left(\tau_{1} \rightarrow \tau_{2}\right) \overline{\vee \# C},\{x: \tau\} \overline{\vee \# C}$, or \#C $\overline{\vee \# C^{\prime}}$.

Let us consider a few examples. First, given a constraint like $\left(\tau_{1} \vee \tau_{2}\right) \wedge \tau_{3} \leqslant \tau_{4}$, we can distribute the intersection over the union thanks to the rules of Boolean algebras (see Section 4.4.4), which results in $\left(\tau_{1} \wedge \tau_{3}\right) \vee\left(\tau_{2} \wedge \tau_{3}\right) \leqslant \tau_{4}$, allowing us to solve $\tau_{1} \wedge \tau_{3} \leqslant \tau_{4}$ and $\tau_{2} \wedge \tau_{3} \leqslant \tau_{4}$ independently. Second, given a constraint like $\tau_{1} \leqslant\left\{x: \tau_{2}\right\} \vee \tau_{3} \rightarrow \tau_{4}$, we simply use the fact that $\left\{x: \tau_{2}\right\} \vee \tau_{3} \rightarrow \tau_{4} \equiv \top$ (as explained in Section 2.2.2) to reduce the constraint to $\tau_{1} \leqslant \top$, a tautology. Third, with constraints containing intersected nominal class tags on the left, we can compute their greatest lower bound based on our knowledge of the single-inheritance class hierarchy. We eventually end up with constraints of the shape " $\tau_{\text {con }} \leqslant \tau_{\text {dis }}$ " and there always exists a $\tau_{i} \in \tau_{\text {con }}$ and $\tau_{j}^{\prime} \in \tau_{\text {dis }}$ such that we can reduce the constraint to an equivalent constraint $\tau_{i} \leqslant \tau_{j}^{\prime}$. Notice that if two related nominal tags appears on each side, it is always safe to pick that comparison, as doing so does not entail any additional constraints. If there are no such related nominal tags, the only other choice is to find a type in the right-hand side to match a corresponding type in the left-hand side, and the syntax of these normal forms prevents there being more than one possible choice. All in all, our Boolean algebra of types equipped with various algebraic simplification laws ensures that we have a lossless way of resolving the complex constraints that arise from union and intersection types, enabling principal type inference.

The constraint solving algorithm described in Section 5.3 and implemented in the artifact uses the ideas explored above but puts the entire constraint into a normal form, instead of normalizing constraints on the fly. This helps to efficiently guarantee termination by maintaining a cache of currently-processed subtyping relationships in normal forms, which is straightforward to query.

\subsection*{3.4 Subsumption Checking}

Subsumption checking, denoted by $\leqslant^{\forall}$, is important to check that definitions conform to given signatures. Contrary to MLsub, which syntactically separates positive from negative types (the polarity

\footnotetext{
${ }^{15}$ Aiken and Wimmers [1993] used a similar trick, albeit in a more specific set-theoretic interpretation of unions/intersections.
${ }^{16}$ If it were not for pattern matching, we could avoid negation types by adopting a more complicated representation of type variable bounds that internalizes the same information. That is, instead of $\alpha \leqslant \tau$ and $\alpha \geqslant \tau$ for a given type variable $\alpha$, we would have bounds of the form $\alpha \wedge \pi \leqslant \tau$ and $\alpha \vee \pi \geqslant \tau$, representing $\alpha \leqslant \tau \vee \neg \pi$ and $\alpha \geqslant \tau \wedge \neg \pi$ respectively. But reducing several upper/lower bounds into a single bound, which previously worked by simply intersecting/taking the union of them, would now be impossible without generalizing bounds further. Type simplification would also become difficult.
}
restriction), and therefore requires different algorithms for constraint solving and subsumption checking, in MLstruct we can immediately reuse the constraint solving algorithm for subsumption checking, without requiring much changes to the type system. To implement $\forall \Xi_{1} \cdot \tau_{1} \leqslant \forall \forall \Xi_{2} \cdot \tau_{2}$, we instantiate all the type variables in $\Xi_{1}$, with their bounds, to fresh type variables, and we turn all the variables in $\Xi_{2}$ into rigid variables (so-called "skolems"). The latter can be done by turning these type variables into fresh flexible nominal tags and by inlining their bounds, expressing them in terms of unions, intersections, and recursive types. Since there is no polarity restrictions in our system, the resulting types can be compared directly using the normal constraint solving algorithm.

Flexible nominal tags $\# F$ are just like nominal class tags $\# C$, except that they can coexist with unrelated tags without reducing to $\perp$. For example, while $\# C_{1} \wedge \# C_{2}$ is equivalent to ⟂ in MLstruct when $C_{1}$ and $C_{2}$ are unrelated, $\# F \wedge \# C_{2}$ is not. ${ }^{17}$ Flexible nominal tags are also the feature used to encode the nominal tags of traits, necessary to implement mixin traits as described in Section 2.1.2.

For lack of space, we do not formally describe subsumption checking in this paper.

\subsection*{3.5 Simplification and Presentation of Inferred Types}

Type simplification and pretty-printing are important components of any practical implementation of MLsub and MLstruct. They indeed perform a lot of the heavy-lifting of type inference, massaging inferred types, which are often big and unwieldy, into neat and concise equivalent type expressions. In this section, we briefly explain how simplification is performed in MLstruct.
3.5.1 Basic Simplifications. For basic simplifications, we essentially follow Parreaux [2020] - we remove polar occurrences of type variables, remove type variables "sandwiched" between identical bounds, and we perform some hash consing to simplify inferred recursive types. The simplification of unions, intersections, and negations is not fully addressed by Parreaux, since MLsub does not fully supports these features. In MLstruct, we apply standard Boolean algebra simplification techniques to simplify these types, such as putting them into disjunctive normal forms, simplifying complements, and factorizing common conjuncts. We also reduce types as they arise, based on Section 2.2.2.
3.5.2 Bound Inlining. Many types can be represented equivalently using either bounded quantification or inlined intersection and union types, so we often have to choose between them. For instance, $\forall(\alpha \leqslant \operatorname{lnt}) \cdot(\beta \geqslant \operatorname{lnt}) . \alpha \rightarrow \alpha \rightarrow \beta$ is much better expressed as the equivalent $\operatorname{lnt} \rightarrow \ln \rightarrow \operatorname{lnt}$. But whether $(\alpha \wedge \mathrm{Int}) \rightarrow(\alpha \wedge \mathrm{Int}) \rightarrow \alpha$ is better than the equivalent $\forall(\alpha \leqslant \mathrm{Int}) . \alpha \rightarrow \alpha \rightarrow \alpha$ may depend on personal preferences. As a general rule of thumb, we only inline bounds when doing so would not duplicate them and when they are not cyclic (i.e., we do not inline recursive bounds).

\subsection*{3.6 Implementation}

MLstruct is implemented in $\sim 5000$ lines of Scala code, including advanced type simplification algorithms and error reporting infrastructure. ${ }^{18}$ We have an extensive tests suite consisting of more than 4000 lines of well-typed and ill-typed MLstruct expressions, for which we automatically check the output of the type simplifier and error reporting for regressions. Running this test suite in parallel takes $\sim$ 2s on a 2020 iMac with a 3.8 GHz 8-Core Intel Core i7 and 32 GB 2667 MHz DDR4.

\section*{4 FORMAL SEMANTICS OF MLSTRUCT}

In this section, we introduce $\lambda\urcorner$, a formal calculus which reflects the core features of MLstruct.

\begin{figure}
\captionsetup{labelformat=empty}
\caption{Core syntax}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-013.jpg?height=645&width=949&top_left_y=365&top_left_x=218}
\end{figure}

Fig. 1. Syntax of types, terms, and contexts.

\subsection*{4.1 Syntax}

The syntax of $\lambda\urcorner$ is presented in Figure 1. We use the notation ${\overline{E_{i}}}^{i}$ to denote a repetition of $i=0$ to $n$ occurrences of a syntax form $E$, and we use the shorthand $\bar{E}$ when $i$ is not needed for disambiguation.
4.1.1 Core Syntax. The core syntax of $\lambda\urcorner$ follows the MLstruct source language presented previously quite closely, though it introduces a syntactic novelty: the mode ◇ or ○ of a syntactic form is used to deduplicate sentences that refer to unions and intersections as well as top and bottom, which are respective duals and can therefore often be treated symmetrically. For instance, $T^{\diamond}$ is to be understood as either $T^{\cdot}$ when $\diamond=\cdot$, i.e., $T$, or as $T^{2}$ when $\diamond=\nu$, i.e., $\perp$. A similar idea was developed independently by d. S. Oliveira et al. [2020] to cut down on boilerplate and repetition in formalizing subtyping systems.

Parametric polymorphism in $\lambda\urcorner$ is attached solely to top-level 'def' bindings, whose semantics, as in languages like Scala, is to re-evaluate their right-hand side every time they are referred to in the program. In contrast, local let bindings are desugared to immediately-applied lambdas, and are treated monomorphically. Let polymorphism is orthogonal to the features presented in this paper, and can be handled by using a level-based algorithm [Parreaux 2020] on top of the core algorithm we describe here, as well as a value restriction if the language is meant to incorporate mutation.

In $\lambda\urcorner$, def bindings are never recursive. This simplification is made without loss of generality, as recursion can be recovered using a Z fixed point combinator, typeable in MLsub [Dolan 2017] and thus also in $\lambda\urcorner$. This combinator is defined as $t_{Z}=\lambda f$. $t_{Z}^{\prime} t_{Z}^{\prime}$ where $t_{Z}^{\prime}=\lambda x$. $f$ ( $\lambda v$. $x x v$ ). One can easily verify that $t_{Z}$ can be typed as $((\alpha \rightarrow \beta) \rightarrow((\alpha \rightarrow \beta) \wedge \gamma)) \rightarrow \gamma$.

To keep the formalism on point, we only present class object types, and ignore uninteresting primitive and built-in types like Int and Bool, which can be encoded as classes. Note that singleton types like 1,2 , and true, as we use them in the introduction, are easily encoded as subclasses $1_{C}$, $2_{C}$, and true $_{C}$ of the corresponding built-in types.

\footnotetext{
${ }^{17}$ This requires extending the syntax of normal forms in a straightforward way to $\tau_{\text {con }}^{\prime}::=\tau_{\text {con }} \overline{\wedge \# F}$ and $\tau_{\text {dis }}^{\prime}::=\tau_{\text {dis }} \overline{\vee \# F}$.
${ }^{18}$ This does not include about 1200 additional lines of code to generate JavaScript (the tests are run through NodeJS).
}

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-014.jpg?height=474&width=1263&top_left_y=304&top_left_x=210}
\captionsetup{labelformat=empty}
\caption{Fig. 2. Small-step evaluation rules.}
\end{figure}

Finally, the syntax of pattern matching 'case $x=t$ of ...' includes a variable binding because the rules for typing it will refine the type of that variable in the different branches. We do not use 'case $x$ of ...' as the core form in order to allow for simple substitution of variables with terms.
4.1.2 Contexts. We use four kinds of contexts. Declarations contexts $\mathcal{D}$ hold the type declarations of the program. Throughout this paper, we assume an ambient declarations context (i.e., our formal developments are implicitly parameterized by $\mathcal{D}$ ). Typing contexts $\Gamma$ bind both monomorphic and polymorphic types, the latter corresponding to 'def' bindings. Subtyping contexts $\Sigma$ record assumptions about subtyping relationships, with some of these assumptions potentially hidden behind a ▷ (explained in Section 4.4.1). Finally, polymorphic or constraining contexts $\Xi$ contain bounds/constraints on type variables and possibly errors (err $\in \Xi$ ) encountered during type inference. The typing rules will ensure that in a polymorphic type $\forall \Xi$. $\tau$, context $\Xi$ is consistent, which implies $\boldsymbol{e r r} \notin \Xi$. Note that $\Sigma$ contexts are rooted in $\Xi$ contexts because subtyping judgments require the former but are invoked from typing judgments, which use the latter for polymorphism.
4.1.3 Shorthands. Throughout this paper, we make use of the following notations and shorthands:
$$
\begin{gathered}
R::=\{\overline{x=v}\} \quad N::=A \mid C \quad H::=\tau \leqslant \tau \quad N \equiv N[\epsilon] \quad C \rightarrow t \equiv C \rightarrow t, \epsilon \\
\left\{\overline{x: \tau_{x}} x \in S, y: \tau_{y}\right\} \equiv\left\{{\overline{x: \tau_{x}}}^{x \in S}\right\} \wedge\left\{y: \tau_{y}\right\} \quad(y \notin S) \quad \text { let } x=t_{1} \text { in } t_{2} \equiv\left(\lambda x . t_{2}\right) t_{1} \\
\text { case } y \text { of } M \equiv \text { case } x=y \text { of }[y \mapsto x] M \quad(x \notin F V(M))
\end{gathered}
$$

\subsection*{4.2 Evaluation Rules}

The small-step reduction semantics of $\lambda\urcorner$ is shown in Figure 2. The relation $P \leadsto P^{\prime}$ reads "program $P$ evaluates to program $P^{\prime}$ in one step." Note that $P$ here may refer to a simple term $t$.

We write $\left\{x=v_{2}\right\} \in v_{1}$ to say that $v_{1}$ is a value of the form ' $C\left\{\overline{z=w}, x=v_{2}\right\}$ ' or of the form ' $C\left\{\overline{z=w}, y=v_{2}^{\prime}\right\}$ ' where $y \neq x$ and $\left\{x=v_{2}\right\} \in C\{\overline{z=w}\}$. Class instances are constructed via the $C R$ introduction form, where $R$ is a record of the fields of the instance. Instance matching works by inspecting the runtime instance of a scrutinee value, in order to determine which corresponding branch to evaluate. This is done through the superclasses function $\mathcal{S}(\tau)$. Note that a term of the shape 'case $x=v$ of $\epsilon$ ' is stuck.

Definition 4.1 (Superclasses). We define the superclasses $\mathcal{S}(\tau)$ of a type $\tau$ as the set of classes transitively inherited by type $\tau$, assuming $\tau$ is a class type or the expansion of a class type. The full definition is given in appendix (Definition A.1).

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-015.jpg?height=743&width=1335&top_left_y=302&top_left_x=156}
\captionsetup{labelformat=empty}
\caption{Fig. 3. Term typing rules.}
\end{figure}

\subsection*{4.3 Declarative Typing Rules}

Program-typing judgments $\Xi, \Gamma \vdash^{\star} P: \tau$ are used to type programs while term-typing judgments $\Xi, \Gamma \vdash t: \tau$ are used to type def right-hand sides and program bodies. The latter judgement is read "under type variable bounds $\Xi$ and in context $\Gamma$, term $t$ has type $\tau$." We present only the rules for the latter judgment in Figure 3, as they are the more interesting ones, and relegate the auxiliary program-typing ( $\Xi, \Gamma \vdash^{\star} P: \tau$ ), consistency ( $\Sigma$ cons.) and subtyping entailment ( $\Sigma \vdash \sigma \leqslant^{\forall} \sigma$ and $\Sigma \models \Sigma$ ) rules to the appendix (Appendix A.1). The consistency judgment is used to make sure we type defs and program bodies under valid (i.e., consistent) bounds only. ${ }^{19}$

Rule T-Obj features a few technicalities deserving of careful explanations. First, notice that its result type is an intersection of the nominal class tag \# $C$ with a record type of all the fields passed in the instantiation. Importantly, these fields may have any types, including ones not compatible with the field declarations in $C$ or its parents. This simplifies the meta theory (especially type inference) and is done without loss of generality: indeed, we can desugar ' $\mathrm{C}\{\mathrm{x}=\mathrm{t}, \ldots\}$ ' instantiations in MLstruct into a type-ascribed instantiation ' $C\{x=t, \ldots\}: C[\bar{\alpha}]^{\prime}$ in $\left.\lambda\right\urcorner,{ }^{20}$ where all $\bar{\alpha}$ are fresh, which will ensure that the provided fields satisfy their declared types in $C$.

T-Obj also requires $C$ to be "final" using the $C$ final judgment (formally defined in Figure 10). This means that $C$ is not extended by any other classes in $\mathcal{D}$. It ensures that, at runtime, for every class pattern $D$, pattern-matching scrutinees are always instances of a class $D^{\prime}$ that is either a subclass of $D$ (meaning $\# D^{\prime} \leqslant \# D$ ) or an unrelated class (meaning $\# D^{\prime} \leqslant \neg \# D$ ). Without this property, type preservation would technically not hold. Indeed, consider the program:
class $C_{1}$ class $C_{2}: C_{1}$ class $C_{3}$
case $x=C_{1}\{ \}$ of $C_{2} \rightarrow C_{3}\{ \},-\rightarrow x$

\footnotetext{
${ }^{19}$ Indeed, under inconsistent bounds, ill-typed terms become typeable. For example, we have ( $\left.\operatorname{lnt} \leqslant \operatorname{lnt} \rightarrow \operatorname{lnt}\right) \vdash 11$ : Int.
${ }^{20}$ The alternative desugaring 'let $t m p=C\{x=t, \ldots\}$ in let_ $t m p: C[\bar{\alpha}]$ in $t m p$ ' is nicer because it allows the user to retain refined field types (as described in Section 2.1.2) as well as any new fields that were not declared in $C$ or its parents.
}

This program can be given type $\neg C_{2}$ since $C_{1} \leqslant C_{2} \vee \neg C_{2} \equiv \top$ (in T-Case3, we pick $\tau_{2}=\neg C_{2}$ ), but it reduces to $C_{1}\{ \}$, which does not have type $\neg C_{2}$ because $C_{1}$ and $C_{2}$ are not unrelated classes.

This finality requirement is merely a technicality of $\lambda\urcorner$ and it does not exist in MLstruct, where non-final classes can be instantiated. This can be understood as each MLstruct class $C$ implicitly defining a final version $C^{F}$ of itself, which is used upon instantiation. So the MLstruct program above would actually denote the following desugared $\lambda\urcorner$ program:
class $C_{1}$ class $C_{1}^{F}: C_{1}$ class $C_{2}: C_{1}$ class $C_{3}$ class $C_{3}^{F}: C_{3}$
case $x=C_{1}^{F}\{ \}: C_{1}$ of $C_{2} \rightarrow C_{3}^{F}\{ \}: C_{3}, \rightarrow x$
The refined program above now evaluates to $C_{1}^{F}\{ \}$, of type $C_{1}^{F}$, which is a subtype of $\neg C_{2}$.
In T-Subs, we use the current constraining context $\Xi$ as a subtyping context $\Sigma$ when invoking the subtyping judgement $\Xi \vdash \tau_{1} \leqslant \tau_{2}$ (presented in the next subsection), which is possible since the syntax of constraining contexts is a special case of the syntax of subtyping contexts.

Rule T-Var2 uses the entailment judgment $\Xi \vdash \sigma \leqslant^{\forall} \forall \epsilon . \tau$ defined in appendix to instantiate the polymorphic type found in the context.

The typing of instance matching is split over three rules. Rule T-Case1 specifies that no scrutinee can be matched by a case expression with no branches, which is expressed by assigning type ⟂ (the type inhabited by no value) to the scrutinee.

Rule T-Case2 handles case expressions with a single, default case, which is equivalent to a let binding, where the body $t_{2}$ of the default case is typed within a typing context extended with the case-bound variable $x$ and the type of the scrutinee. This rule requires the scrutinee to have a class type \#C; this is to prevent functions from being matched, because that would technically break preservation in a similar way as described above (since we do not have $\pi_{1} \rightarrow \pi_{2} \leqslant \neg \# D^{21}$ ).

T-Case3 is the more interesting instance matching rule. We first assume that the scrutinee $t_{1}$ has some type $\tau_{1}$ in order to type the first case branch, and then assume $t_{1}$ has type $\tau_{2}$ to type the rest of the instance matching (by reconstructing a smaller case expression binding a new variable $x$ which shadows the old variable occurring in $M$ ). Then, we make sure that the scrutinee $t_{1}$ can be typed at $\# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}$, which ensures that if $t_{1}$ is an instance of $C$, then it is also of type $\tau_{1}$, and if not, then it is of type $\tau_{2}$. In this rule, $\tau_{1}$ can be picked to be anything, so assuming $\Gamma \cdot\left(x: \tau_{1}\right)$ to type $t_{2}$ is sufficient, and there is no need to assume $\Gamma \cdot\left(x: \tau_{1} \wedge \# C\right)$. If the $t_{2}$ branch needs $\tau_{1}$ to be a subtype of $\# C$, we can always pick $\tau_{1}=\tau_{1}^{\prime} \wedge \# C$. Notice that the required type for $t_{1}$ still has the same shape $\# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2} \equiv \# C \wedge\left(\# C \wedge \tau_{1}^{\prime}\right) \vee \neg \# C \wedge \tau_{2} \equiv \# C \wedge \tau_{1}^{\prime} \vee \neg \# C \wedge \tau_{2}$.

\subsection*{4.4 Declarative Subtyping Rules}

The declarative subtyping rules are presented in Figure 4. Remember that the mode syntax ◇ is used to factor in dual formulations. For instance, $\tau \leqslant \mathrm{T}^{\diamond}$ is to be understood as either $\tau \leqslant \mathrm{T}^{\cdot}$ when $\diamond=\cdot$, i.e., $\tau \leqslant \top$, or as $\tau \leqslant^{\downarrow} \mathrm{T}^{\downarrow}$ when $\diamond=\nu$, i.e., $\tau \geqslant \perp$, also written $\perp \leqslant \tau$. The purpose of rule S-Weaken is solely to make rules which need no context slightly more concise to state. In this paper, we usually treat applications of S-Weaken implicitly.
4.4.1 Subtyping Recursive Types. A consequence of our syntactic account of subtyping is that we do not define types as some fixed point over a generative relation, as done in, e.g., [Dolan 2017; Pierce 2002]. Instead, we have to account for the fact that we manipulate finite syntactic type trees, in which recursive types have to be manually unfolded to derive things about them. This is the purpose of the S-Exp rules, which substitute a possibly-recursive type with its body to expose one layer of its underlying definition. As remarked by Amadio and Cardelli [1993, §3.2], to subtype recursive types, it is not enough to simply allow unfolding them a certain number of times.

\footnotetext{
${ }^{21}$ We cannot support this without breaking subtyping consistency, because it would mean that $\# C \wedge\left(\tau_{1} \rightarrow \tau_{2}\right) \leqslant \ldots$
}

Moreover, in our system, recursive types may arise from cyclic type variable constraints (which is important for type inference), and thus not be attached to any explicit recursive binders. Thus, we cannot simply follow Castagna [2012, §1.3.4] in admitting a $\mu$ rule, which would still be insufficient.
4.4.2 Subtyping Hypotheses. We make use of the $\Sigma$ environment to store subtyping hypotheses via S-Assum, to be leveraged later using the S-Hyp rule. We should be careful not to allow the use of a hypothesis right after assuming it, which would obviously make the system unsound (as it could derive any subtyping). In the specification of their constraint solving algorithm, Hosoya et al. [2005] use two distinct judgments $\vdash$ and $\vdash^{\prime}$ to distinguish from places where the hypotheses can or cannot be used. We take a different, but related approach. Our S-Assum subtyping rule resembles the Löb rule described by Appel et al. [2007], which uses the "later" modality ▷ in order to delay the applicability of hypotheses - by placing this symbol in front of the hypothesis being assumed, we prevent its immediate usage by S-Hyp. We eliminate ▷ when passing through a function or record constructor: the dual ◁ symbol is used to remove all ▷ from the set of hypotheses, making them available for use by S-Hyp. These precautions reflect the "guardedness" restrictions used by Dolan [2017] on recursive types, which prevents usages of $\alpha$ that are not guarded by → or $\{\ldots\}$ in a recursive type $\mu \alpha . \tau$. Such productivity restriction is also implemented by our guardedness check, preventing the definition of types such as type $A=A$ and type $A=\neg A$ (Section 2.1.6). ${ }^{22}$
${ }^{22}$ Perhaps counter-intuitively, it is not a problem to infer types like ' $\forall(\alpha \leqslant \alpha) \cdot \tau$ ' and ' $\forall(\alpha \leqslant \neg \alpha) \cdot \tau$ ' because such "funny" cyclic bounds, unlike unproductive recursive types, do not actually allow concluding incorrect subtyping relationships.

\begin{table}
\begin{tabular}{|l|l|l|l|l|l|}
\hline $\Sigma \vdash \tau \leqslant \tau$ & $\tau \leqslant \tau$ & $\triangleleft \Xi=\Xi$ & $\triangleleft(\Sigma \cdot H)=\triangleleft \Sigma \cdot H$ & $\triangleleft(\Sigma \cdot \triangleright H)=\triangleleft \Sigma \cdot H$ & \\
\hline S-Refl & S-ToBo & S-Compl ◇ & S-NegInv
$$
\Sigma \vdash \tau_{1} \leqslant \tau_{2}
$$ & S-AndOr11△ & S-AndOr12。 \\
\hline $\overline{\tau \leqslant \tau}$ & $\overline{\tau \leqslant \mathrm{T}^{0}}$ & $\overline{\tau \vee^{\diamond} \neg \tau \geqslant} \mathrm{T}^{\diamond}$ & $\Sigma \vdash \neg \tau_{2} \leqslant \neg \tau_{1}$ & $\overline{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{1}}$ & $\overline{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{2}}$ \\
\hline S-AndOr2。
$$
\Sigma \vdash \tau \geqslant \tau_{1}
$$ & $\Sigma \vdash \tau \geqslant{ }^{\diamond} \tau_{2}$ & S-Distribo & & S-Trans
$$
\Sigma \vdash \tau_{0} \leqslant \tau_{1}
$$ & $\Sigma \vdash \tau_{1} \leqslant \tau_{2}$ \\
\hline \multicolumn{2}{|c|}{$\Sigma \vdash \tau \geqslant^{\diamond} \tau_{1} \vee^{\diamond} \tau_{2}$} & & $\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \leqslant\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right)$ & & $\Sigma \vdash \tau_{0} \leqslant \tau_{2}$ \\
\hline \begin{tabular}{l}
S-Weaken \\
H
\end{tabular} & \begin{tabular}{l}
S-Assum \\
$\Sigma \cdot \triangleright H \vdash H$
\end{tabular} & \begin{tabular}{l}
S-Hyp \\
$H \in \Sigma$
\end{tabular} & \begin{tabular}{l}
S-ClsSub \\
$C_{2} \in \mathcal{S}\left(\# C_{1}\right)$
\end{tabular} & \begin{tabular}{l}
S-ClsBot \\
$C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$
\end{tabular} & \\
\hline & & & & & $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$ \\
\hline $\Sigma \vdash H$ & $\Sigma \vdash H$ & $\Sigma \vdash H$ & $\# C_{1} \leqslant \# C_{2}$ & $\# C_{1} \wedge \# C_{2} \leqslant \perp$ & \\
\hline \multicolumn{2}{|r|}{\begin{tabular}{l}
S-FunDepth \\
$\triangleleft \Sigma \vdash \tau_{0} \leqslant \tau_{1}$ \\
$\triangleleft \Sigma \vdash \tau_{2} \leqslant \tau_{3}$
\end{tabular}} & & & & \begin{tabular}{l}
S-Exp॰ \\
$\tau \exp . \tau^{\prime}$
\end{tabular} \\
\hline \multicolumn{2}{|c|}{$\Sigma \vdash \tau_{1} \rightarrow \tau_{2} \leqslant \tau_{0} \rightarrow \tau_{3}$} & & $\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right) \geqslant^{\diamond} \tau_{1} \rightarrow \tau_{2} \wedge^{\diamond} \tau_{3} \rightarrow \tau_{4}$ & & $\tau \geqslant^{\triangleright} \tau^{\prime}$ \\
\hline \begin{tabular}{l}
S-RcdDepth \\
$\triangleleft \Sigma \vdash \tau_{1}$
\end{tabular} & & S-RcoMrg ◇ & & S-RcdTop
$$
\tau \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}
$$ & \\
\hline $\Sigma \vdash\left\{x: \tau_{1}\right\}$ & $\leqslant\left\{x: \tau_{2}\right\}$ & & $\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\} \leqslant\left\{x: \tau_{1}\right\} \vee^{\diamond}\left\{x: \tau_{2}\right\}$ & $\top \leqslant\left\{x: \tau_{1}\right\} \vee \tau$ & \\
\hline $\tau$ exp. $\tau$ & & \begin{tabular}{l}
S-AlsExp \\
$\left(\right.$ type $\left.A\left[{\overline{\alpha_{i}}}^{i \in S}\right]=\tau\right) \in \mathcal{D}$
$$
A\left[\bar{\tau}_{i}^{i \in S}\right] \operatorname{exp.}\left[{\overline{\alpha_{i}}}^{i \mapsto \tau_{i}}{ }^{i \in S}\right] \tau
$$
\end{tabular} & & $\overline{C\left[\bar{\tau}_{i}{ }^{i \in S}\right] \text { exp. } \# C \wedge\left[{\overline{\alpha_{i}} \mapsto \tau_{i}}^{i \in S}\right] \tau}$ & \\
\hline
\end{tabular}
\captionsetup{labelformat=empty}
\caption{Fig. 4. Declarative subtyping rules.}
\end{table}
4.4.3 Example. As an example, let us try to derive $A_{1} \leqslant A_{2}$ where $A_{1}=\tau \rightarrow \tau \rightarrow A_{1}$ and $A_{2}=\tau \rightarrow A_{2}$, which states that the type of a function taking two curried $\tau$ arguments an arbitrary number of times is a special case of the type of a function taking a single $\tau$ argument an arbitrary number of times. To facilitate the development, we use the shorthand $H=A_{1} \leqslant A_{2}$. We start by deriving that the respective unfoldings of the recursive types are subtypes; that is, that (1) $\tau \rightarrow \tau \rightarrow A_{1} \leqslant \tau \rightarrow A_{2}$. Note that for conciseness, we omit the applications of S-Weaken in the derivations below:
$$
\operatorname{FUN} \frac{\operatorname{REFL} \frac{\operatorname{REFL} \frac{}{H \vdash \tau \leqslant \tau} \frac{\left(A_{1} \leqslant A_{2}\right) \in H}{H \vdash A_{1} \leqslant A_{2}} \operatorname{HYP}}{H \vdash \tau \leqslant \tau} \frac{\operatorname{FUN} \frac{\operatorname{Ty}}{H \vdash \tau \rightarrow A_{1} \leqslant \tau \rightarrow A_{2}} \frac{H \rightarrow A_{2} \leqslant A_{2}}{H \vdash} \text { Exp }}{H \vdash \tau \rightarrow A_{1} \leqslant A_{2}} \text { TRANS }}{\triangleright H \vdash \tau \rightarrow \tau \rightarrow A_{1} \leqslant \tau \rightarrow A_{2} \quad \text { (1) }}
$$

Then, we simply have to fold back the unfolded recursive types, using Exp and Trans:
$$
\text { Assum } \frac{\operatorname{Trans} \frac{\operatorname{Exp} \frac{\nabla H \vdash A_{1} \leqslant \tau \rightarrow \tau \rightarrow A_{1}}{\triangleright H} \quad(1)}{\triangleright H \vdash A_{1} \leqslant \tau \rightarrow A_{2}}}{\triangleright H \vdash A_{1} \leqslant A_{2}} \frac{\square}{\triangleright H \vdash \tau \rightarrow A_{2} \leqslant A_{2}} \text { Exp }
$$
4.4.4 A Boolean Algebra. The subtyping preorder in $\lambda\urcorner$ gives rise to a Boolean lattice or algebra when taking the equivalence relation ' $\tau_{1} \equiv \tau_{2}$ ' to be the relation induced by ' $\tau_{1} \leqslant \tau_{2}$ and $\tau_{2} \leqslant \tau_{1}$ '. To see why, let us inspect the standard way of defining Boolean algebras, which is as the set of complemented distributive lattices. We can define a lattice equivalently as either:
- An algebra $\langle L, \wedge, \vee\rangle$ such that $\wedge$ and $\vee$ are idempotent, commutative, associative, and satisfy the absorption law, i.e., $\tau \wedge(\tau \vee \pi) \equiv \tau \vee(\tau \wedge \pi) \equiv \tau$. Then $\tau_{1} \leqslant \tau_{2}$ is taken to mean $\tau_{1} \equiv \tau_{1} \wedge \tau_{2}$ or (equivalently) $\tau_{1} \vee \tau_{2} \equiv \tau_{2}$.
- A partially-ordered set $\langle L, \leqslant\rangle$ (i.e., $\leqslant$ is reflexive, transitive, and antisymmetric) where every two elements $\tau_{1}$ and $\tau_{2}$ have a least upper bound $\tau_{1} \vee \tau_{2}$ (supremum) and a greatest lower bound $\tau_{1} \wedge \tau_{2}$ (infimum). That is, $\forall \pi \leqslant \tau_{1}, \tau_{2} . \pi \leqslant \tau_{1} \wedge \tau_{2}$ and $\forall \pi \geqslant \tau_{1}, \tau_{2} . \pi \geqslant \tau_{1} \vee \tau_{2}$.
The latter is most straightforward to show: we have reflexivity by S-Refl, transitivity by S-Trans, antisymmetry by definition of $\equiv$, and the supremum and infimum properties are given directly by S-AndOr2- and S-AndOr2> respectively.

Moreover, to be a Boolean algebra, our lattice needs to be:
- a complemented lattice, which is
- bounded: $T$ and ⟂ are respective least and greatest elements (S-ToB );
- such that every $\tau$ has a complement $\neg \tau$ where $\tau \vee \neg \tau \equiv \mathrm{T}$ and $\tau \wedge \neg \tau \equiv \perp$ (S-Compl $\diamond$ ); ${ }^{23}$
- a distributive lattice, meaning that $\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \equiv\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right)$ for $\diamond \in\{>, \cdot\}$.

The first direction $\leqslant$ of distributivity is given directly by S-Distrib. The other direction $\geqslant^{\diamond}$ is admissible: since $\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{1}$ (S-AndOr11 $\diamond$ ) and $\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{2}$ (S-AndOr12 $\diamond$ ), we can easily derive $\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \geqslant^{\diamond} \tau \wedge^{\diamond} \tau_{1}$ and $\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \geqslant \rho \wedge^{\diamond} \tau_{2}$, and by (S-AndOr2 ) we conclude that $\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \geqslant^{\diamond}\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right)$.

A useful property of Boolean algebras is that the usual De Morgan's laws hold, which will allow us to massage constrains into normal forms during type inference.

\footnotetext{
${ }^{23}$ We can also show that our lattice is uniquely complemented, i.e., $\neg \tau_{1} \equiv \neg \tau_{2}$ implies $\tau_{1} \equiv \tau_{2}$ (Theorem B.14).
}
4.4.5 Algebraic Rules. We call S-FunMrg and S-RcdTop algebraic subtyping rules because they do not follow from a set-theoretic interpretation of order connectives $(\wedge, \vee, \neg)$. S-FunMrg and S-RcdMrg respectively make function and record types lattice homomorphisms, ${ }^{24}$ which is required to make type inference complete - this allows the existence of well-behaved normal forms. Though one can still think of types as sets of values, as in the semantic subtyping approach, in $\lambda\urcorner$ the sets of values of $\tau_{1} \wedge \tau_{2}$ is not the intersection of the sets of values of $\tau_{1}$ and $\tau_{2}$ (unless $\tau_{1}$ and $\tau_{2}$ are nominal tags or records), and similarly for unions and complements. These algebraic rules are sound in $\lambda\urcorner$ because of the careful use we make of unions and intersections, e.g., not using intersections to encode overloading. Notably, S-RcoTop implies surprising relationships like $\neg\left(\tau_{1} \rightarrow \tau_{2}\right) \leqslant\{x: \pi\}$ and $\neg\{x: \pi\} \leqslant\{y: \pi\}(x \neq y)$, exemplifying that negation in $\lambda\urcorner$ is essentially algebraic.

\subsection*{4.5 Soundness of the Declarative Type System}

We now state the main soundness theorems for $\lambda\urcorner$ 's type system, proven in Section B. 12 and B.13. In the following, $\vdash^{\star}$ is used as the syntax for program-typing judgments (see Figure 9 in appendix).

Theorem 4.2 (Progress). If $\vdash^{\star} P: \tau$ and $P$ is not a value, then $\vdash P \leadsto P^{\prime}$ for some $P^{\prime}$.
Theorem 4.3 (Preservation). If $\vdash^{\star} P: \tau$ and $\vdash P \leadsto P^{\prime}$, then $\vdash^{\star} P^{\prime}: \tau$.

\section*{5 PRINCIPAL TYPE INFERENCE FOR $\lambda$ ᄀ}

We now formally describe the type inference algorithm which was presented in Section 3.

\subsection*{5.1 Type Inference Rules}

Our type inference rules are presented in Figure 5. The judgments $\Gamma \Vdash^{\star} P: \tau \Rightarrow \Xi$ and $\Xi, \Gamma \Vdash t$ : $\tau \Rightarrow \Xi$ are similar to their declarative typing counterparts, except that they are algorithmic and produce constraining contexts $\Xi$ containing inferred type variables bounds.

We give the following formal meaning to premises of the form ' $\alpha$ fresh', and in the rest of this paper, we implicitly only consider well-formed derivations:

Definition 5.1 (Well-formed derivations). A type inference or constraining derivation is said to be well-formed if, for every $\alpha$, the ' $\alpha$ fresh' premise appears at most once in the entire derivation and, if it does, $\alpha$ does not occur in any user-specified type (i.e., on the right of ascription trees ' $t: \tau$ ').

The program-typing inference rules I-Body and I-Def mirror their declarative counterparts. In I-Def, notice how the output context corresponding to the definition's body is the one used to quantify the corresponding type in the typing context. Notice that in these rules, the consistency condition (which can be seen in the declarative typing rules in Figure 9) has disappeared, because type inference only produces consistent contexts by design.

The main difference between type inference rules and declarative typing rules is that in the former, we immediately produce a type for each subexpression irrelevant of its context, using type variables for local unknowns, and we then use a constraining judgement $\Sigma \vdash \tau \ll \pi \Rightarrow \Xi$ (explained in the next subsection) to make sure that the inferred type $\tau$ conforms to the expected type $\pi$ in this context. So whenever we need to guess a type (such as the type of a lambda's parameter in I-Abs), we simply introduce a fresh type variable. As an example, in I-Proj, we infer an unconstrained type $\tau$ for the field projection's prefix $t$, and then make sure that this is a subtype of a record type by constraining $\Xi_{0} \vdash \tau \ll\{x: \alpha\} \Rightarrow \Xi_{1}-$ where $\Xi_{1}$ is the output context containing the type variable bounds necessary to make this relationship hold. Rules I-App, I-Asc, I-Case1, I-Case2, and I-Case3 all work according to the same principles, threading the set of constraining contexts

\footnotetext{
${ }^{24}$ A lattice homomorphism $f$ is such that $f(\tau \vee \pi) \equiv f(\tau) \vee f(\pi)$ and $f(\tau \wedge \pi) \equiv f(\tau) \wedge f(\pi)$. Function types are lattice homomorphisms in their parameters in the sense that $f(\tau)=(\neg \tau) \rightarrow \pi$ is a lattice homomorphism.
}

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-020.jpg?height=1294&width=1349&top_left_y=298&top_left_x=148}
\captionsetup{labelformat=empty}
\caption{Fig. 5. Algorithmic type inference rules.}
\end{figure}
currently inferred through the next type inference steps, which is necessary to make sure that all inferred type variable bounds are consistent with each other. Rule I-Var2 refreshes all the variables of a type $\forall \Xi$. $\tau$ obtained from the typing context, which includes both variables that occur in the constraining context $\Xi$ as well as those that occur in the underlying type $\tau$, even when some of the latter may not be mentioned in $\Xi$; indeed, in $\lambda\urcorner$ all type variables are implicitly quantified.

\subsection*{5.2 Reduced Disjunctive Normal Forms}

To facilitate constraint solving, it is useful to massage types into a normal form which we call RDNF, for reduced disjunctive normal form. This normal form is similar to a classical disjunctive normal form (DNF) except that we reduce all "incompatible" intersections and unions to ⟂ and $\top$ respectively. Here, incompatible means that the type holds no useful information, either because it is inhabited by no value or because it cannot be used meaningfully, as explained in Section 2.2.2.

The syntax of RDNF is given below. It is indexed by a level $n$ and there are two possible levels: level-0 RDNF, written D ${ }^{0}$ does not contain any occurrence of class or alias types at the top level
(they will have been expanded); whereas level-1 RDNF, written $\mathrm{D}^{1}$, allows them. Notation: we will often write $D$ as a shorthand for $D^{1}$ (and similarly for the other indexed syntax forms).
$$
\begin{aligned}
\mathrm{D}^{n} & :=\perp\left|\mathrm{C}^{n}\right| \mathrm{D}^{n} \vee \mathrm{C}^{n} & \mathrm{C}^{n} & ::=\mathrm{I}^{n} \wedge \neg \mathrm{U}^{n}\left|\mathrm{C}^{n} \wedge \alpha\right| \mathrm{C}^{n} \wedge \neg \alpha \\
\mathrm{I}^{1} & :=\mathrm{I}^{0} \mid \mathrm{I}^{1} \wedge N\left[\overline{\mathrm{D}^{1}}\right] & \mathrm{I}^{0} & ::=\mathcal{I}^{\mathcal{N}}[\mathcal{N}]\left|\mathcal{I}^{\rightarrow}[\mathcal{F}]\right| \mathcal{I}^{\{ \}}[\mathcal{R}] \\
\mathrm{U}^{1} & :=\mathrm{U}^{0} \mid \mathrm{U}^{1} \vee N\left[\overline{\mathrm{D}^{1}}\right] & \mathrm{U}^{0} & ::=\perp\left|\mathrm{D}^{1} \rightarrow \mathrm{D}^{1}\right|\left\{x: \mathrm{D}^{1}\right\} \mid \mathrm{U}^{0} \vee \# C
\end{aligned}
$$
where the $\mathcal{I}^{\prime}$ contexts stand for combinations of nominal tags $\mathcal{N}$, functions $\mathcal{F}$, and records $\mathcal{R}$ :
$$
\begin{array}{lll}
\mathcal{I}^{\mathcal{N}}[\square]::=\square \wedge \mathcal{F} \wedge \mathcal{R} & \mathcal{N}::=\top \mid \# C & \mathcal{I}[\square]::=\mathcal{I}^{\mathcal{N}}[\square]\left|\mathcal{I}^{\rightarrow}[\square]\right| \mathcal{I}^{\{ \}}[\square] \\
\mathcal{I}^{\rightarrow}[\square]::=\mathcal{N} \wedge \square \wedge \mathcal{R} & \mathcal{F}::=\top \mid \mathrm{D}^{1} \rightarrow \mathrm{D}^{1} & \top^{3}::=\top \wedge \top \wedge \top \\
\mathcal{I}^{\{ \}}[\square]::=\mathcal{N} \wedge \mathcal{F} \wedge \square & \mathcal{R}::=\top \mid\left\{\overline{x: \mathrm{D}^{1}}\right\} &
\end{array}
$$

As an example, ' $\mathrm{D}_{1}=\# C \wedge \top \wedge\{x: \top\} \wedge C[$ Int, Bool $] \wedge A[$ Str $] \wedge \neg \perp \wedge \neg \alpha$ ' is a valid level-1 RDNF, but not a valid level-0 one because $C[$ Int, Bool $]$ and $A[$ Str $]$ occur at the top level and are not expanded, while ' $\mathrm{D}_{2}^{n}=\top \wedge \top \wedge\{x: C[\operatorname{Int}$, Bool $]\} \wedge \neg \perp^{\prime}$ is well-defined for both $n \in\{0,1\}$.
5.2.1 Algorithm. Figures 6 and 7 give an algorithm to convert types $\tau$ to level- $n$ RDNFs, written $\mathrm{dnf}^{n}(\tau)$. The task is essentially straightforward, if relatively tedious. Essentially, $\mathrm{dnf}^{n}$ pushes negations in using DeMorgan laws, distributes intersections over unions, and at the same time ensures that all constructed conjunctions are de-duplicated and as reduced as possible, so that for instance intersections of unrelated classes are reduced to ⟂ and function and record types are merged with themselves. We write $(\neg) \tau$ as a shorthand for either $\tau$ or $\neg \tau$ (used uniformly in a rule) and make use of auxiliary functions union ${ }^{n}\left(\mathrm{D}^{n}, \mathrm{D}^{n}\right)$ and inter ${ }^{n}\left(\mathrm{D}^{n}, \mathrm{D}^{n}\right)$, which rely on the following context definitions $S^{+}[\cdot]$ and $S^{-}[\cdot]$, used to "dig into" the various shapes of $\mathrm{C}^{n}$ syntaxes:
$$
\begin{aligned}
& S^{+}[\square]::=\mathcal{I}[\square]\left|S^{+}[\square] \wedge \alpha\right| S^{+}[\square] \wedge \neg \alpha\left|S^{+}[\square] \wedge \neg \cup\right| S^{+}[\square] \wedge N\left[\overline{\mathrm{D}^{1}}\right] \\
& S^{-}[\square]::=S^{-}[\square] \wedge \alpha\left|S^{-}[\square] \wedge \neg \alpha\right| \mathrm{I} \wedge \neg S^{\neg}[\square] \\
& S^{\neg}[\square]::=\square\left|S^{\neg}[\square] \vee N\left[\overline{\mathrm{D}^{1}}\right]\right| S^{\neg}[\square] \vee \# C \mid \mathrm{U} \vee \square
\end{aligned}
$$

For example, we can decompose $\mathrm{C}^{n}=\mathrm{I}^{n} \wedge \neg\left(\left(\mathrm{D}_{1}^{n} \rightarrow \mathrm{D}_{2}^{n}\right) \vee \# C\right) \wedge \alpha$ as $\mathrm{C}^{n}=S^{-}\left[\mathrm{D}_{1}^{n} \rightarrow \mathrm{D}_{2}^{n}\right]$ where $S^{-}[\square]=I^{n} \wedge \neg(\square \vee \# C) \wedge \alpha$.

The algorithm is well-defined on well-formed types $\tau \boldsymbol{w} \boldsymbol{f}$, assuming a well-formed declarations context $\mathcal{D} \boldsymbol{w} \boldsymbol{f}$. These notions of well-formedness are defined formally in Appendix A.2.

Lemma 5.2 (Well-Defined dnf). If $\mathcal{D}$ wf, $\tau$ wf, and $n \in\{0,1\}$, then $\operatorname{dnf}^{n}(\tau)=\mathrm{D}^{n}$ for some $\mathrm{D}^{n}$.
Lemma 5.3 (Correctness of dnf). For all $\tau, n \in\{0,1\}$, and $\mathrm{D}^{n}=\operatorname{dnf}^{n}(\tau)$, we have $\tau \equiv \mathrm{D}^{n}$.

\subsection*{5.3 Type Constraining Rules}

The type constraining rules are defined in Figure 8. They are defined for any pairs of types and input subtyping contexts, returning an output context containing err in case the constraining fails. We need err cases to distinguish an infinite loop in the algorithm from a subtype constraining error, i.e., we want to justify that we have a proper algorithm and not just a semi-algorithm.

In top-level constraining judgments, of the form $\Sigma \vdash \tau \ll \tau \Rightarrow \Xi$, we check whether a subtyping relationship is currently in the assumptions; if not, we extend the set of assumptions with the current constraint (guarded by a ▷ ) and call the nested constraining rules with the two sides $\tau_{1}$ and $\tau_{2}$ merged into a single $\mathrm{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right)$ normal form. ${ }^{25}$ Nested constraining judgments have syntax $\Sigma \vdash \mathrm{D}^{0} \Rightarrow \Xi$; they implicitly solve the constraint $\mathrm{D}^{0} \leqslant \perp$. We can do this because for all $\tau_{1}$ and $\tau_{2}$,

\footnotetext{
${ }^{25}$ The real implementation is a little smarter and does not always put the entire constraint into DNF to avoid needless work in common cases. It also uses a mutable cache to reuse previous computations and avoid exponential blowups [Pierce 2002].
}
$$
\begin{align*}
& \mathrm{dnf}^{n}(\tau): \mathrm{D}^{n} \\
& \mathrm{dnf}^{n}(\mathrm{~T})=\mathrm{dnf}^{n}(\neg \perp)=\mathrm{T}^{3} \wedge \neg \perp  \tag{1}\\
& \operatorname{dnf}^{n}(\perp)=\operatorname{dnf}^{n}(\neg \top)=\perp  \tag{2}\\
& \operatorname{dnf}^{n}(\alpha)=\top^{3} \wedge \neg \perp \wedge \alpha  \tag{3}\\
& \operatorname{dnf}^{n}(\# C)=\# C \wedge \top \wedge \top \wedge \neg \perp  \tag{4}\\
& \operatorname{dnf}^{n}\left(\tau_{1} \rightarrow \tau_{2}\right)=\top \wedge \operatorname{dnf}^{1}\left(\tau_{1}\right) \rightarrow \operatorname{dnf}^{1}\left(\tau_{2}\right) \wedge \top \wedge \neg \perp  \tag{5}\\
& \operatorname{dnf}^{n}(\{x: \tau\})=\left\{x: \operatorname{dnf}^{1}(\tau)\right\} \wedge \top \wedge \top \wedge \neg \perp  \tag{6}\\
& \operatorname{dnf}^{0}(N[\bar{\tau}])=\operatorname{dnf}^{0}\left(\tau^{\prime}\right) \quad \text { when } N[\bar{\tau}] \exp . \tau^{\prime}  \tag{7}\\
& \operatorname{dnf}^{1}(N[\bar{\tau}])=\top^{3} \wedge N\left[\overline{\operatorname{dnf}^{1}(\tau)}\right] \wedge \neg \perp  \tag{8}\\
& \operatorname{dnf}^{n}\left(\tau_{1} \wedge \tau_{2}\right)=\operatorname{inter}\left(\operatorname{dnf}^{n}\left(\tau_{1}\right), \operatorname{dnf}^{n}\left(\tau_{2}\right)\right)  \tag{9}\\
& \operatorname{dnf}^{n}\left(\tau_{1} \vee \tau_{2}\right)=\operatorname{union}\left(\operatorname{dnf}^{n}\left(\tau_{1}\right), \operatorname{dnf}^{n}\left(\tau_{2}\right)\right)  \tag{10}\\
& \operatorname{dnf}^{n}(\neg \alpha)=\top^{3} \wedge \neg \perp \wedge \neg \alpha  \tag{11}\\
& \mathrm{dnf}^{n}(\neg \# C)=\top^{3} \wedge \neg(\perp \vee \# C)  \tag{12}\\
& \operatorname{dnf}^{n}(\neg\{x: \tau\})=\top^{3} \wedge \neg\left\{x: \operatorname{dnf}^{1}(\tau)\right\}  \tag{13}\\
& \operatorname{dnf}^{n}\left(\neg\left(\tau_{1} \rightarrow \tau_{2}\right)\right)=\top^{3} \wedge \neg\left(\operatorname{dnf}^{1}\left(\tau_{1}\right) \rightarrow \operatorname{dnf}^{1}\left(\tau_{2}\right)\right)  \tag{14}\\
& \mathrm{dnf}^{0}(\neg N[\bar{\tau}])=\operatorname{dnf}^{0}\left(\neg \tau^{\prime}\right) \quad \text { when } N[\bar{\tau}] \exp \cdot \tau^{\prime}  \tag{15}\\
& \operatorname{dnf}^{1}(\neg N[\bar{\tau}])=\top^{3} \wedge \neg\left(\perp \vee N\left[\overline{\operatorname{dnf}^{1}(\tau)}\right]\right)  \tag{16}\\
& \operatorname{dnf}^{n}\left(\neg\left(\tau_{1} \wedge \tau_{2}\right)\right)=\operatorname{union}\left(\operatorname{dnf}^{n}\left(\neg \tau_{1}\right), \operatorname{dnf}^{n}\left(\neg \tau_{2}\right)\right)  \tag{17}\\
& \operatorname{dnf}^{n}\left(\neg\left(\tau_{1} \vee \tau_{2}\right)\right)=\operatorname{inter}\left(\operatorname{dnf}^{n}\left(\neg \tau_{1}\right), \operatorname{dnf}^{n}\left(\neg \tau_{2}\right)\right) \tag{18}
\end{align*}
$$
$$
\begin{align*}
\operatorname{union}\left(\mathrm{D}^{n}, \mathrm{D}^{n}\right) & : \mathrm{D}^{n} \\
\operatorname{union}\left(\mathrm{D}^{n}, \perp\right) & =\mathrm{D}^{n}  \tag{19}\\
\operatorname{union}\left(\mathrm{D}^{n}, \mathrm{C}^{n}\right) & = \begin{cases}\mathrm{D}^{n} & \text { when } \mathrm{C}^{n} \in \mathrm{D}^{n} \\
\mathrm{D}^{n} \vee \mathrm{C}^{n} & \text { otherwise }\end{cases}  \tag{20}\\
\operatorname{union}\left(\mathrm{D}_{1}^{n}, \mathrm{D}_{2}^{n} \vee \mathrm{C}^{n}\right) & =\operatorname{union}\left(\operatorname{union}\left(\mathrm{D}_{1}^{n}, \mathrm{C}^{n}\right), \mathrm{D}_{2}^{n}\right) \tag{21}
\end{align*}
$$
$$
\begin{align*}
\operatorname{inter}\left(\mathrm{D}^{n}, \mathrm{D}^{n}\right) & : \mathrm{D}^{n} \\
\operatorname{inter}\left(\perp, \mathrm{D}^{n}\right) & =\operatorname{inter}\left(\mathrm{D}^{n}, \perp\right)=\perp  \tag{22}\\
\operatorname{inter}\left(\mathrm{D}_{1}^{n} \vee \mathrm{C}^{n}, \mathrm{D}_{2}^{n}\right) & =\operatorname{union}\left(\operatorname{inter}\left(\mathrm{D}_{1}^{n}, \mathrm{D}_{2}^{n}\right), \operatorname{inter}\left(\mathrm{C}^{n}, \mathrm{D}_{2}^{n}\right)\right)  \tag{23}\\
\operatorname{inter}\left(\mathrm{C}_{1}^{n}, \mathrm{D}^{n} \vee \mathrm{C}_{2}^{n}\right) & =\operatorname{union}\left(\operatorname{inter}\left(\mathrm{C}_{1}^{n}, \mathrm{D}^{n}\right), \operatorname{inter}\left(\mathrm{C}_{1}^{n}, \mathrm{C}_{2}^{n}\right)\right) \tag{24}
\end{align*}
$$

Fig. 6. Normal form construction algorithm.
the subtyping relationship $\Sigma \vdash \tau_{1} \leqslant \tau_{2}$ is formally equivalent to $\Sigma \vdash \tau_{1} \wedge \neg \tau_{2} \leqslant \perp$. This technique was inspired by Pearce [2013], who also puts constraints into this form to solve subtyping problems involving unions, intersections, and negations. Our constraining rules are deterministic except for C-Var1 and C-Var2. By convention, we always pick C-Var1 in case both can be applied.
$$
\begin{align*}
& \operatorname{inter}\left(\mathrm{C}^{n}\left|\perp, \mathrm{C}^{n}\right| \mathrm{I}^{n} \mid \neg \mathrm{U}^{n}\right): \mathrm{C}^{n} \mid \perp \\
& \operatorname{inter}(\perp, \quad)=\perp  \tag{26}\\
& \operatorname{inter}\left(\mathrm{C}_{1}^{n}, \mathrm{C}_{2}^{n} \wedge(\neg) \alpha\right)= \begin{cases}\operatorname{inter}\left(\mathrm{C}_{1}^{n}, \mathrm{C}_{2}^{n}\right) & \text { when }(\neg) \alpha \in \mathrm{C}_{1}^{n} \\
\perp & \text { when } \alpha, \neg \alpha \in \mathrm{C}_{1}^{n} \wedge(\neg) \alpha \\
\operatorname{inter}\left(\mathrm{C}_{1}^{n} \wedge(\neg) \alpha, \mathrm{C}_{2}^{n}\right) & \text { otherwise }\end{cases}  \tag{27}\\
& \operatorname{inter}\left(\mathrm{C}^{n}, \mathrm{I}^{n} \wedge \neg \mathrm{U}^{n}\right)=\operatorname{inter}\left(\operatorname{inter}\left(\mathrm{C}^{n}, \mathrm{I}^{n}\right), \neg \mathrm{U}^{n}\right)  \tag{28}\\
& \operatorname{inter}\left(\mathrm{C}^{1}, \mathrm{I}^{1} \wedge N\left[\overline{\mathrm{D}^{1}}\right]\right)=\operatorname{inter}\left(\operatorname{inter}\left(\mathrm{C}^{1}, \mathrm{I}^{1}\right), N\left[\overline{\mathrm{D}^{1}}\right]\right)  \tag{29}\\
& \operatorname{inter}\left(\mathrm{C}^{n}, \mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R}\right)=\operatorname{inter}\left(\operatorname{inter}\left(\operatorname{inter}\left(\mathrm{C}^{n}, \mathcal{N}\right), \mathcal{F}\right), \mathcal{R}\right)  \tag{30}\\
& \operatorname{inter}\left(\mathrm{C}^{1}, \neg\left(\mathrm{U}^{1} \vee N\left[\overline{\mathrm{D}^{1}}\right]\right)\right)=\operatorname{inter}\left(\operatorname{inter}\left(\mathrm{C}^{1}, \neg \mathrm{U}^{1}\right), \neg N\left[\overline{\mathrm{D}^{1}}\right]\right)  \tag{31}\\
& \text { inter }\left(\mathrm{C}^{n}, \neg \perp\right)=\mathrm{C}^{n}  \tag{32}\\
& \operatorname{inter}\left(S^{-}\left[\mathrm{U}_{1}^{n}\right], \neg \mathrm{U}_{2}^{n}\right)=\mathrm{T}^{3} \quad \text { when } \quad\left(\mathrm{U}_{1}^{n}, \mathrm{U}_{2}^{n}\right) \in\left\{\begin{array}{l}
\left(\overrightarrow{\rightarrow_{-}},\left\{x:-{ }_{-}\right\}\right) ; \\
\left(\left\{x:-{ }_{-}\right\}, \overrightarrow{y_{-}}\right) ; \\
(\{x:\})
\end{array}\right\}  \tag{33}\\
& \operatorname{inter}\left(S^{-}\left[\mathrm{D}_{1}^{1} \rightarrow \mathrm{D}_{2}^{1}\right], \neg\left(\mathrm{D}_{3}^{1} \rightarrow \mathrm{D}_{4}^{1}\right)\right)=S^{-}\left[\operatorname{inter}\left(\mathrm{D}_{1}^{1}, \mathrm{D}_{3}^{1}\right) \rightarrow \operatorname{union}\left(\mathrm{D}_{2}^{1}, \mathrm{D}_{4}^{1}\right)\right]  \tag{34}\\
& \operatorname{inter}\left(S^{-}\left[\left\{x: \mathrm{D}_{1}^{1}\right\}\right], \neg\left\{x: \mathrm{D}_{2}^{1}\right\}\right)=S^{-}\left[\left\{x: \operatorname{union}\left(\mathrm{D}_{1}^{1}, \mathrm{D}_{2}^{1}\right)\right\}\right]  \tag{35}\\
& \operatorname{inter}\left(S^{-}\left[\mathrm{U}_{1}^{n}\right], \neg\left(\mathrm{U}_{2}^{n} \vee \# C\right)\right)= \begin{cases}\operatorname{inter}\left(S^{-}\left[\mathrm{U}_{1}^{n}\right], \neg \mathrm{U}_{2}^{n}\right) & \text { when } \# C \in \mathrm{U}_{1}^{n} \\
\operatorname{inter}\left(S^{-}\left[\mathrm{U}_{1}^{n} \vee \# C\right], \neg \mathrm{U}_{2}^{n}\right) & \text { otherwise }\end{cases}  \tag{36}\\
& \operatorname{inter}\left(S^{-}[\perp], \neg \mathrm{U}^{\mathrm{n}}\right)=S^{-}\left[\mathrm{U}^{n}\right]  \tag{37}\\
& \operatorname{inter}\left(\mathrm{D}^{1} \mid \mathrm{C}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right): \mathrm{D}^{1} \\
& \operatorname{inter}\left(\perp,(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right)=\perp  \tag{38}\\
& \operatorname{inter}\left(\mathrm{D}_{0}^{1} \vee \mathrm{C}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right)=\operatorname{inter}\left(\mathrm{D}_{0}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right) \vee \operatorname{inter}\left(\mathrm{C}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right)  \tag{39}\\
& \operatorname{inter}\left(\mathrm{C}^{1} \wedge \alpha,(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right)=\operatorname{inter}\left(\mathrm{C}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right) \wedge \alpha  \tag{40}\\
& \operatorname{inter}\left(\mathrm{C}^{1} \wedge \neg \alpha,(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right)=\operatorname{inter}\left(\mathrm{C}^{1},(\neg) N\left[\overline{\mathrm{D}^{1}}\right]\right) \wedge \neg \alpha  \tag{41}\\
& \operatorname{inter}\left(\mathrm{I}^{1} \wedge \neg \mathrm{U}^{1}, N\left[\overline{\mathrm{D}^{1}}\right]\right)= \begin{cases}\mathrm{I}^{1} \wedge \neg \mathrm{U}^{1} & \text { when } N\left[\overline{\mathrm{D}^{1}}\right] \in \mathrm{I}^{1} \\
\mathrm{I}^{1} \wedge N\left[\overline{\mathrm{D}^{1}}\right] \wedge \neg \mathrm{U}^{1} & \text { otherwise }\end{cases}  \tag{42}\\
& \operatorname{inter}\left(\mathrm{I}^{1} \wedge \neg \mathrm{U}^{1}, \neg N\left[\overline{\mathrm{D}^{1}}\right]\right)= \begin{cases}\mathrm{I}^{1} \wedge \neg \mathrm{U}^{1} & \text { when } N\left[\overline{\mathrm{D}^{1}}\right] \in \mathrm{U}^{1} \\
\mathrm{I}^{1} \wedge \neg\left(\mathrm{U}^{1} \vee N\left[\overline{\mathrm{D}^{1}}\right]\right) & \text { otherwise }\end{cases}  \tag{43}\\
& \operatorname{inter}\left(\mathrm{C}^{n}, \mathcal{N}|\mathcal{F}| \mathcal{R}\right): \mathrm{C}^{n} \mid \perp \\
& \operatorname{inter}\left(\mathrm{C}^{n}, \mathrm{~T}\right)=\mathrm{C}^{n}  \tag{44}\\
& \operatorname{inter}\left(S^{+}\left[\mathcal{I}^{\mathcal{N}}[\top]\right], \# C\right)=S^{+}\left[\mathcal{I}^{\mathcal{N}}[\# C]\right]  \tag{45}\\
& \operatorname{inter}\left(S^{+}\left[I\left[\# C_{1}\right]\right], \# C_{2}\right)= \begin{cases}\perp & \text { when } C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \text { and } C_{2} \notin \mathcal{S}\left(\# C_{1}\right) \\
S^{+}\left[\mathcal{I}\left[\# C_{2}\right]\right] & \text { when } C_{1} \in \mathcal{S}\left(\# C_{2}\right) \\
S^{+}\left[\mathcal{I}\left[\# C_{1}\right]\right] & \text { when } C_{2} \in \mathcal{S}\left(\# C_{1}\right)\end{cases}  \tag{46}\\
& \operatorname{inter}\left(S^{+}[\mathcal{I} \rightarrow[\top]], \mathrm{D}_{1}^{1} \rightarrow \mathrm{D}_{2}^{1}\right)=S^{+}\left[\mathcal{I} \rightarrow\left[\mathrm{D}_{1}^{1} \rightarrow \mathrm{D}_{2}^{1}\right]\right]  \tag{47}\\
& \operatorname{inter}\left(S^{+}\left[\mathcal{I}\left[\mathrm{D}_{1}^{1} \rightarrow \mathrm{D}_{2}^{1}\right]\right], \mathrm{D}_{3}^{1} \rightarrow \mathrm{D}_{4}^{1}\right)=S^{+}\left[\mathcal{I}\left[\operatorname{union}\left(\mathrm{D}_{1}^{1}, \mathrm{D}_{3}^{1}\right) \rightarrow \operatorname{inter}\left(\mathrm{D}_{2}^{1}, \mathrm{D}_{4}^{1}\right)\right]\right]  \tag{48}\\
& \operatorname{inter}\left(\mathrm{C}^{n},\left\{x: \mathrm{D}_{x}^{1}, \overline{y: \mathrm{D}_{y}^{1}}\right\}\right)=\operatorname{inter}\left(\operatorname{inter}\left(\mathrm{C}^{n},\left\{x: \mathrm{D}_{x}^{1}\right\}\right),\left\{\overline{y: \mathrm{D}_{y}^{1}}\right\}\right)  \tag{49}\\
& \operatorname{inter}\left(S^{+}\left[\mathcal{I}^{\{ \}}[\mathrm{T}]\right],\left\{x: \mathrm{D}^{1}\right\}\right)=S^{+}\left[\mathcal{I}^{\{ \}}\left[\left\{x: \mathrm{D}^{1}\right\}\right]\right]  \tag{50}\\
& \operatorname{inter}\left(S^{+}\left[\mathcal{I}\left[\left\{\overline{x: \mathrm{D}_{x}^{1}} x \in S\right\}\right]\right],\left\{y: \mathrm{D}^{1}\right\}\right)= \begin{cases}S^{+}\left[\mathcal{I}\left[\left\{\overline{x: \mathrm{D}_{x}^{1}} x \in S \backslash\{y\}, y: \operatorname{inter}\left(\mathrm{D}_{y}^{1}, \mathrm{D}^{1}\right)\right\}\right]\right] & \text { when } y \in S \\
S^{+}\left[\mathcal{I}\left[\left\{\overline{x: \mathrm{D}_{x}^{1}} x \in S, y: \mathrm{D}^{1}\right\}\right]\right] & \text { otherwise }\end{cases} \tag{51}
\end{align*}
$$

Fig. 7. Normal form construction algorithm (continued).

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-024.jpg?height=1097&width=1335&top_left_y=287&top_left_x=141}
\captionsetup{labelformat=empty}
\caption{Fig. 8. Normal form constraining rules.}
\end{figure}

Definition 5.4 (Upper and lower bounds). We use the following definitions of lower and upper bounds $l b_{\Xi}(\alpha)$ and $u b_{\Xi}(\alpha)$ of a type variable $\alpha$ inside constraining context $\Xi$ :
$$
\begin{aligned}
l b_{\Xi}(\alpha) & : \tau \\
l b_{\Xi \cdot \text { err }}(\alpha)=l b_{\Xi \cdot \triangleright H}(\alpha) & =l b_{\Xi}(\alpha) \\
l b_{\Xi \cdot(\tau \leqslant \alpha)}(\alpha) & =\tau \vee l b_{\Xi}(\alpha) \\
l b_{\Xi \cdot(\tau \leqslant \beta)}(\alpha) & =l b_{\Xi}(\alpha) \quad(\alpha \neq \beta) \\
l b_{\Xi \cdot(\beta \leqslant \tau)}(\alpha) & =l b_{\Xi}(\alpha) \\
l b_{\epsilon}(\alpha) & =\perp
\end{aligned}
$$
$$
\begin{aligned}
u b_{\Xi}(\alpha) & : \tau \\
u b_{\Xi \cdot \operatorname{err}}(\alpha)=u b_{\Xi \cdot \triangleright H}(\alpha) & =u b_{\Xi}(\alpha) \\
u b_{\Xi \cdot(\tau \leqslant \beta)}(\alpha) & =u b_{\Xi}(\alpha) \\
u b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) & =\tau \wedge u b_{\Xi}(\alpha) \\
u b_{\Xi \cdot(\beta \leqslant \tau)}(\alpha) & =u b_{\Xi}(\alpha) \quad(\alpha \neq \beta) \\
u b_{\epsilon}(\alpha) & =\top
\end{aligned}
$$

Notice how the C-Var1/2 rules solve tricky constraints involving type variables by moving the rest of a type expression to the other side of the inequality, relying on negation types and on the properties of Boolean algebras (see Theorem B.20). Moreover, C-Var1/2 look up the existing bounds of the type variable being constrained and perform a recursive call to ensure that the new bound is consistent with these existing ones. This is required to ensure we only produce consistent output contexts, and it explains why we have to thread constraining contexts throughout all type inference derivations. As part of this recursive call, we extend the subtyping assumptions context with the bound being recorded. For example, C-Var2 recurses with context $\Sigma \cdot(\mathrm{C} \leqslant \alpha)$ instead of just $\Sigma$. This is crucial for two reasons: First, it is possible that new upper bounds $\tau_{i}$ be recorded for $\alpha$ as part
of the recursive call. By adding C to the current lower bounds of $\alpha$ within the recursive call, we make sure that any such new upper bounds $\tau_{i}$ will be checked against C as part of the resulting $l b_{\Sigma}(\alpha) \ll \tau_{i}$ constraining call performed when adding bound $\tau_{i}$. Second, it is quite common for type inference to result in direct type variable bound cycles, such as $\alpha \leqslant \beta, \beta \leqslant \alpha$, which can for instance arise from constraining $\beta \rightarrow \beta \leqslant \alpha \rightarrow \alpha$. These cycles do not lead to divergence of type inference thanks to the use of $\Sigma \cdot(\mathrm{C} \leqslant \alpha)$ instead of $\Sigma$ in the recursive call, ensuring that any constraint resulting from a type variable bound cycle will end up being caught by C-Hyp.

The other constraining rules are fairly straightforward. The "beauty" of the RDNF is that it essentially makes constraint solving with $\lambda\urcorner$ types obvious. In each case, there is always an obvious choice to make: either (1) the constraint is unsatisfiable (for example with $\mathrm{T} \leqslant \perp$ in C-NotBot, which yields an err); or (2) the constraint needs to unwrap an irrelevant part of the type to continue (for example with $\mathrm{D}_{1} \rightarrow \mathrm{D}_{2} \leqslant \mathrm{U} \vee \# C$ in C-CLs3, which can be solved iff $\mathrm{D}_{1} \rightarrow \mathrm{D}_{2} \leqslant \mathrm{U}$ itself can be solved, because function types are unrelated to nominal class tags); or (3) we can solve the \left. constraint in an obvious, unambiguous way (for example with ${\overline{x: \mathrm{D}_{x}}}^{x \in S}\right\} \leqslant\{y: \mathrm{D}\}$ where $y \in S$ in C-Rcd1).

Normalizing types deeply (i.e., not solely on the outermost level) makes the termination of constraining (Theorem A.9) straightforward. If we did not normalize nested types and for example merged $\left\{x: \tau_{1}\right\} \wedge\left\{x: \tau_{2}\right\}$ syntactically as $\left\{x: \tau_{1} \wedge \tau_{2}\right\}$, constraining recursive types in a way that repetitively merges the same type constructors together could lead to unbounded numbers of equivalent types being constrained, such as $\left\{x: \tau_{1} \wedge \tau_{1} \wedge \tau_{1} \wedge \ldots\right\}$, failing to terminate by C-Hyp.

Example. Consider the constraint $\tau=\{x:$ Nat, $y:$ Nat $\} \ll \pi=\{x: \operatorname{lnt}, y: \top\}$. After adding the pair to the set of hypotheses, C-Assum computes the RDNF $\operatorname{dnf}^{0}(\tau \wedge \neg \pi)=\{x:$ Nat, $y$ : Nat $\} \wedge \neg\{x: \operatorname{Int}\} \vee\{x:$ Nat, $y:$ Nat $\} \wedge \neg\{y: \top\}$. Then this constrained type is decomposed into two smaller constrained types $\{x:$ Nat, $y:$ Nat $\} \wedge \neg\{x: \operatorname{Int}\}$ and $\{x:$ Nat, $y:$ Nat $\} \wedge \neg\{y: \top\}$ by C-Or, and each one is solved individually by C-RcD1, which requires constraining respectively Nat << Int and Nat << T. The former yields RDNF \#Nat $\wedge \neg$ \#Int, which is solved by C-ClsCls1, and the latter yields $\mathrm{RDNF} \perp$, which is solved by C-Вот.

\subsection*{5.4 Correctness of Type Inference}

We conclude this section by presenting the main correctness lemmas and theorems of type inference.
Theorem 5.5 (Soundness of type inference). If the type inference algorithm successfully yields a type for program $P$, then $P$ has this type. Formally: if $\Vdash^{\star} P: \tau \Rightarrow \Xi$ and $\operatorname{err} \notin \Xi$, then $\Xi \vdash^{\star} P: \tau$.

Lemma 5.6 (Sufficiency of Constraining). Successful type constraining ensures subtyping: if $\Sigma$ cons. and $\Sigma \vdash \tau \ll \pi \Rightarrow \Xi$ and err $\notin \Xi$, then $\Xi \cdot \Sigma$ cons. and $\Xi \cdot \Sigma \vdash \tau \leqslant \pi$.

Theorem 5.7 (Constraining Termination). For all $\tau, \pi, \mathcal{D}, \Sigma \boldsymbol{w} f, \Sigma \vdash \tau \ll \pi \Rightarrow \Xi$ for some $\Xi$.
Theorem 5.8 (Completeness of type inference). If a program $P$ can be typed at type $\sigma$, then the type inference algorithm derives a type $\sigma^{\prime}$ such that $\sigma^{\prime} \leqslant^{\forall} \sigma$. Formally: if $\Xi \vdash^{\star} P: \tau$, then $\Vdash^{\star} P: \tau^{\prime} \Rightarrow \Xi^{\prime}$ for some $\Xi^{\prime}$ and $\tau^{\prime}$ where $\Xi^{\prime}$ cons. and $\forall \Xi^{\prime} . \tau^{\prime} \leqslant^{\forall} \forall \Xi$. $\tau$.

In the following lemma, which is crucial for proving the above theorem, $\rho$ refers to type variable substitutions and $\Xi \models \Xi^{\prime}$ denotes that $\Xi$ entails $\Xi^{\prime}$ (both defined formally in Appendix B).

Lemma 5.9 (Completeness of Constraining). If there is a substitution $\rho$ that makes $\rho\left(\tau_{1}\right)$ a subtype of $\rho\left(\tau_{2}\right)$ in some consistent $\Xi$, then constraining $\tau_{1} \ll \tau_{2}$ succeeds and only introduces type variable bounds that are entailed by $\Xi$ (modulo $\rho$ ). Formally: if $\Xi$ cons. and $\Xi \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$ and $\Xi \vDash \rho\left(\Xi_{0}\right)$, then $\Xi_{0} \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi_{1}$ for some $\Xi_{1}$ so that err $\notin \Xi_{1}$ and $\Xi \models \rho\left(\Xi_{1}\right)$.

\section*{6 RELATED WORK}

We now relate the different aspects of MLstruct and $\lambda\urcorner$ with previous work.
Intersection type systems. Intersection types for lambda calculus were pioneered by Coppo and Dezani-Ciancaglini [1980]; Barendregt et al. [1983], after whom the "BCD" type system is named. BCD has the very powerful " $\mathrm{T}-\wedge-\mathrm{I}$ " rule, stating: if $\Gamma \vdash t: \tau_{1}$ and $\Gamma \vdash t: \tau_{2}$, then $\Gamma \vdash t: \tau_{1} \wedge \tau_{2}$. Such systems have the interesting property that typeability coincides with strong normalization [Ghilezan 1996], making type inference undecidable. Thankfully, we do not need something as powerful as $\mathrm{T}-\wedge-\mathrm{I}-$ instead, we introduce intersections in less general ways (i.e., through T-Obj), and we retain decidability of type inference. Most intersection type systems, including MLstruct and $\lambda\urcorner$, do admit the following standard BCD subtyping rules given by Barendregt et al.: (1) $\tau_{1} \wedge \tau_{2} \leqslant \tau_{1}$; (2) $\tau_{1} \wedge \tau_{2} \leqslant \tau_{2}$; and (3) if $\tau_{1} \leqslant \tau_{2}$ and $\tau_{1} \leqslant \tau_{3}$, then $\tau_{1} \leqslant \tau_{2} \wedge \tau_{3}$. Some systems use intersection types to encode a form of overloading [Pierce 1991]. However, Smith [1991] showed that ML-style type inference with such a general form of overloading and subtyping is undecidable (more specifically, finding whether inferred sets of constraints are satisfiable is) and proposed constructor overloading, a restricted form of overloading with more tractable properties, sufficient to encode many common functions, such as addition on different primitive types as well as vectors of those types. Constructor overloading is eminently compatible with MLstruct and MLscript. Another design decision for intersection systems is whether and how this connective should distribute over function types. BCD subtyping states ${ }^{26}\left(\tau \rightarrow \pi_{1}\right) \wedge\left(\tau \rightarrow \pi_{2}\right) \leqslant \tau \rightarrow\left(\pi_{1} \wedge \pi_{2}\right)$ and Barbanera et al. [1995] also propose $\left(\tau_{1} \rightarrow \pi\right) \wedge\left(\tau_{2} \rightarrow \pi\right) \leqslant\left(\tau_{1} \vee \tau_{2}\right) \rightarrow \pi$. Together, these correspond to the minimal relevant logic B+ [Dezani-Ciancaglini et al. 1998]. Approaches like that of Pottier [1998b] use a greatest lower bound connective $\sqcap$ that resembles type intersection $\wedge$ but admits a more liberal rule that generalizes the previous two: $\left(\tau_{1} \rightarrow \pi_{1}\right) \wedge\left(\tau_{2} \rightarrow \pi_{2}\right) \leqslant\left(\tau_{1} \vee \tau_{2}\right) \rightarrow\left(\pi_{1} \wedge \pi_{2}\right)$, which we will refer to as (full) function distributivity. However, notice that in a system with primitives, full function distributivity is incompatible with $\mathrm{T}-\wedge-\mathrm{I}$ and thus precludes intersection-based overloading. ${ }^{27}$

Union and intersection types in programming. Union types are almost as old as intersection types, first introduced by MacQueen et al. [1986], ${ }^{28}$ and both have a vast (and largely overlapping) research literature, with popular applications such as refinement types [Freeman and Pfenning 1991]. These types have seen a recent resurgence, gaining a lot of traction both in academia [Alpuim et al. 2017; Binder et al. 2022; Castagna et al. 2022; Dunfield 2012; Huang and Oliveira 2021; Muehlboeck and Tate 2018; Rehman et al. 2022] and in industry, ${ }^{29}$ with several industry-grade programming languages like TypeScript, Flow, and Scala 3 supporting them, in addition to a myriad of lesser-known research languages. It is worth noting that many modern type systems with intersection types do not support $\mathrm{T}-\wedge-\mathrm{I}$ in its full generality. For example, in TypeScript, a term can only assume an overloaded intersection type if that term is a function with a list of pre-declared type signatures, and in Scala intersections can only be introduced through inheritance. Unions and intersections have also found uses in program analysis. Palsberg and Pavlopoulou [1998] showed that polyvariant analysis can be related formally to a subtyping system with union, intersection, and recursive types. Unions model sets of abstract values and intersections model each usage of an

\footnotetext{
${ }^{26}$ This rule together with T- $\wedge-\mathrm{I}$ was shown unsound in the presence of imperative features by Davies and Pfenning [2000].
${ }^{27}$ For instance, term $i d=\lambda x . x$ has both types Int → Int and Bool → Bool so by T- $\wedge-$ I it would also have type $(\operatorname{lnt} \rightarrow \operatorname{lnt}) \wedge($ Bool $\rightarrow$ Bool $)$. But by function distributivity and subsumption, this would allow typing id as ( $\operatorname{lnt} \vee$ Bool) $\rightarrow$ ( $\operatorname{Int} \wedge$ Bool) and thus typing id 0 (which reduces to 0 ) as Int $\wedge$ Bool, breaking type preservation.
${ }^{28}$ Funnily, MacQueen et al. reported at the time that "type-checking difficulties seem to make intersection and union awkward in practice; moreover it is not clear if there are any potential benefits from their use,"
${ }^{29}$ The first author of this paper has received emails from various people reimplementing Simple-sub [Parreaux 2020] and wanting to know how to add support for first-class union and intersection types, showing the enduring interest in these.
}
abstract value. Their system conspicuously does not feature polymorphism, but it is well-known that there is a correspondence between intersection types and polymorphism - a polymorphic type can be viewed as an infinite intersection of all its possible instantiations [Aiken and Wimmers 1993]. Smith and Wang [2000] propose inferring polymorphic types, rather than intersections, for function definitions, which is more flexible and composable as it can process unrelated definitions separately, whereas the approach based solely on intersections is a global process. We believe that having both intersections and polymorphism, as in MLscript, represents the best of both worlds.

Type inference for unions and intersections. None of the previous approaches we know have proposed a satisfactory ML-style type inference algorithm for full union and intersection types. By satisfactory, we mean that the algorithm should infer principal polymorphic types without backtracking. Earlier approaches used heavily-restricted forms of unions and intersections. For instance, Aiken and Wimmers [1993]; Aiken et al. [1994] impose very strict restrictions on negative unions (they must be disjoint) and on positive intersections (they must not have free variables and must be "upward closed"). Trifonov and Smith [1996] go further and restrict intersections to negative or input positions (those appearing on the right of $\leqslant$ constraints) and unions types to positive or output positions (those appearing on the left). Binder et al. [2022]; Dolan [2017]; Parreaux [2020]; Pottier [1998b] all follow the same idea. In these systems, unions and intersections are not first-class citizens: they cannot be used freely in type annotations. Frisch et al. [2008] infer set-theoretic types (see semantic subtyping below) for a higher-order language with overloading but do not infer polymorphic types. Castagna et al. [2016] propose a complete polymorphic set-theoretic type inference system, but their types are not principal so their algorithm returns several solutions, which leads to the need for backtracking. It seems this should have severe scalability issues, as the number of possible types for an expression would commonly grow exponentially. ${ }^{30}$ Petrucciani [2019] describes ways to reduce backtracking, but recognizes it as fundamentally "unavoidable."

Negation or complement types. Negation types have not been nearly as ubiquitous as unions and intersection in mainstream programming language practice and theory, except in the field of semantic subtyping (see below). Nevertheless, our use of negation types to make progress while solving constraints is not new - Aiken and Wimmers [1993] were the first to propose using complement types in such a way. However, their complement types are less precise than our negation types, ${ }^{31}$ and in their system $\alpha \wedge \tau_{1} \leqslant \tau_{2}$ and $\alpha \leqslant \tau_{2} \vee \neg \tau_{1}$ are not always equivalent.

Recursive types. Recursive types in the style of MLstruct, where a recursive type is equivalent to its unfolding (a.k.a. equi-recursive types, not to be confused with iso-recursive types), have a long history in programming languages research [Abadi and Fiore 1996; Amadio and Cardelli 1993; Appel et al. 2007; Hosoya et al. 2005; MacQueen et al. 1986; Pierce 2002], dating as far back as Morris’ thesis, where he conjectured their use under the name of cyclic types [Morris 1969, pp.122-124]. Recursive types with subtyping were developed in the foundational work of Amadio and Cardelli [1993] and Brandt and Henglein [1998] gave a coinductive axiomatization of such recursive types. Jim and Palsberg [1999] described a co-inductive formalization of recursive types as arbitrary infinite trees which is more general than approaches like ours, which only allows reasoning about regular types. Nevertheless, the algorithms they gave were unsurprisingly restricted to regular types. Gapeyev et al. [2002]; Pierce [2002] reconciled the representation as infinite regular trees with the representation as $\mu$ types, and described the standard algorithms to decide the corresponding subtyping relationship. An important aspect of practical recursive type algorithms is that one

\footnotetext{
${ }^{30}$ Hindley-Milner type inference and derived systems like MLsub and MLstruct can also infer types that grow exponentially in some situations, but these mostly occur in pathological cases, and not in common human-written programs.
${ }^{31}$ For example, in their system $\neg(\tau \rightarrow \pi)$ is the type of all values that are not functions, regardless of $\tau$ and $\pi$.
}
needs to maintain the cache of discovered subtyping relationships across recursive calls to avoid exponential blowup [Gapeyev et al. 2002]. Our implementation of MLstruct follows the same principle, as a naive implementation of $\lambda\urcorner$ would lead to exactly the same blowup. Also refer to Section 4.4.2 for more parallels between the handling of recursive types in $\lambda\urcorner$ and previous work.

Early approaches to subtype inference. The problem of type inference in the presence of subtyping was kick-started in the 1980s [Fuh and Mishra 1989; Mitchell 1984; Stansifer 1988] and studied extensively in the 1990s [Aiken and Wimmers 1993; Curtis 1990; Fuh and Mishra 1990; Jim and Palsberg 1999; Kozen et al. 1994; Palsberg et al. 1997; Pottier 1998a,b; Smith 1991], mostly through the lens of constraint solving on top of Hindley-Milner-style type inference [Damas and Milner 1982; Hindley 1969; Milner 1978]. These approaches often involved combinations of record, intersection, union, and recursive types, but as far as we know none proposed an effective (i.e., without backtracking) principal type inference technique for a system with all of these combined. Odersky et al. [1999] gave them a unified account by proposing a general framework called $\mathrm{HM}(\mathrm{X})$, where the ' X ' stands for a constraint solver to plug into their generic system. While these approaches often claimed a form of principal type inference (also called minimality ${ }^{32}$ ), the constrained types they inferred were often large and unwieldy. Beyond inferring constraint sets and ensuring their satisfiability, the related problem of simplification to produce more readable and efficiently-processable types was also studied, often by leveraging the connection between regular type trees and finite-state automata [Aiken 1996; Eifrig et al. 1995; Pottier 1996, 1998b, 2001; Simonet 2003]. A major stumbling block with all of these approaches was the problem of non-structural subtyping entailment ${ }^{33}$ (NSSE), which is to decide whether a given type scheme, which consists in a polymorphic type along with its constraints on type variables, subsumes another. Solving this issue is of central importance because it is needed to check implementations against user-provided interfaces and type signatures, and because it provides a foundation from which to derive sound type simplification techniques. However, to this day NSSE remains an open problem, and it is not known whether it is even decidable [Dolan 2017]. Due to these difficulties, interest in this very powerful form of subtyping all but faded in the subsequent decade, in what we interpret as a minor "subtype inference winter." Indeed, many subsequent approaches were developed in reaction to this complexity with the aim of being simpler to reason about (e.g., polymorphic variants - see below).

Algebraic subtyping. Approaches like that of Pottier [1998b] used a lattice-theoretic construction of types inspired by the connection between types and term automata. Meet $\sqcap$ and join □ operators resembling intersection and union types are used to compactly representing conjunctions of constraints, but these are not first-class types, in that they are restricted to appearing respectively in negative and positive positions only. Full function distributivity (defined above, in intersection type systems) holds in these approaches due to the lattice structure. Pottier's system still suffered from a lack of complete entailment algorithm due to NSSE. Dolan [2017]; Dolan and Mycroft [2017] later built upon that foundation and proposed an algebraic construction of types which allowed breaking free of NSSE and finally enjoying a sound and complete entailment algorithm. Two magical ingredients allowed this to be possible: 1. the definition of "extensible" type semantics based on constructing types as a distributive lattice of coproducts; and 2. a different treatment of type variables than in previous work, representing them as part of the lattice of types and not as unknowns ranging over a set of ground types. In this paper, we in turn build on these foundations, although we only retain the latter innovation, somehow forgoing the "extensible"

\footnotetext{
${ }^{32}$ Some authors like Aiken et al. [1994] make a distinction between a concept of principality which is purely syntactic (relating types by a substitution instance relationship) and minimality which involve a semantic interpretation of types.
33 "Non-structural" here is by opposition to so-called structural subtyping, which is a more tractable but heavily restricted form of subtyping that only relates type constructors of identical arities [Palsberg et al. 1997] (precluding, e.g., $\{x: \tau\} \leqslant \mathrm{T}$ ).
}
construction of types. ${ }^{34}$ Together with our generalization of the subtyping lattice to a Boolean one by adding negations and with the additional structure we impose on types (such as reducing unions of unrelated records to $T$ ), this turns out to be sufficient for allowing principal type inference and decidable entailment (though we only sketched the latter in this paper for lack of space). Ingredient 1 allowed Dolan to show the soundness of his system in a very straightforward way, relying on the property (called Proposition 12 by Dolan [2017]) that any constraint of the form $\bigwedge_{i} \tau_{i} \leqslant \bigvee_{i} \pi_{i}$ holds iff there is a $k$ such that $\tau_{k} \leqslant \pi_{k}$ when all $\tau_{i}$ have distinct constructors and all $\pi_{i}$ similarly. By contrast, we allow some intersections of unrelated type constructors to reduce to ⟂ and some unions of them to T , and we are thus not "extensible" in Dolan's terminology. This is actually desirable in the context of pattern matching, where we want to eliminate impossible cases by making the intersections of unrelated class types empty. It is also needed in order to remove the ambiguity from constraints like $\left(\tau_{1} \rightarrow \tau_{2}\right) \wedge\{x: \pi\} \leqslant\left(\tau_{1}^{\prime} \rightarrow \tau_{2}^{\prime}\right) \vee\left\{x: \pi^{\prime}\right\}$ which in our system reduces to $\left(\tau_{1} \rightarrow \tau_{2}\right) \wedge\{x: \pi\} \leqslant \mathrm{T}$. The present paper also takes heavy inspiration from our earlier operationally-focused take on Dolan's type inference algorithm [Parreaux 2020]. While Dolan shirks from explicitly representing constraints, which he prefers to inline inside types on the fly as $\sqcap$ and $\sqcup$ types, we use an approach closer to the original constrained-types formulation followed by Pottier. Besides being much easier to implement, our approach has other concrete advantages, such as the ability to deal with invariance seamlessly (class $C[A]:\{f: A \rightarrow A\}$, which is invariant in A, is valid in MLstruct) and a simpler treatment of cyclic type variable constraints.

Semantic subtyping and set-theoretic types. The semantic subtyping approaches [Castagna et al. 2022, 2016; Frisch et al. 2002, 2008; Petrucciani 2019] view types as sets of values which inhabit them and define the subtyping relationship as set inclusion, giving set-based meaning to union, intersection, and negation (or complement) connectives. This is by contrast to algebraic subtyping, which may admit subtyping rules that violate the set-theoretic interpretation, such as function distributivity, to ensure that the subtyping lattice has desirable algebraic properties. For more detailed discussions contrasting semantic subtyping with other approaches, we refer the reader to Parreaux [2020] and Muehlboeck and Tate [2018].

Occurrence and flow typing. Occurrence typing was originally introduced by Tobin-Hochstadt and Felleisen [2008] for Typed Scheme, and was later incorporated into TypeScript and Flow, where it is known as flow typing. It allows the types of variables to be locally refined based on path conditions encountered in the program. Negation types are pervasive in this context, though they are often only used at the meta-theoretic level. Instance-matching in MLstruct can be understood as a primitive form of occurrence typing in that it refines the types of scrutinee variables in case expressions, similarly to the approach of Rehman et al. [2022]. Occurrence typing was also recently extended to the semantic subtyping context [Castagna et al. 2021, 2022], where negation types are first-class types. The latter work proposes a powerful type inference approach that can infer overloaded function signatures as intersections types; however, this approach does not support polymorphism and likely does not admit principal types. The idea of simplifying the definition of core object-oriented type languages by using class tags (or brands) in addition to structural typing is not new and was notably developed by Jones et al. [2015]; Lee et al. [2015].

Polymorphic records/variants and row polymorphism. Polymorphic records are structurallytyped products whose types admit the usual width and depth subtyping relationships. Their dual, polymorphic variants, are another useful language feature [Garrigue 1998, 2001], used to encode structural sum types. In their simplest expression, polymorphic records (resp. variants) do not support ad-hoc field extension (resp. default match cases). Previous approaches have thus extended

\footnotetext{
${ }^{34}$ As discussed in prior work [Parreaux 2020], we believe the argument for Dolan's notion of extensibility to be rather weak.
}
polymorphic records and variants with row polymorphism, which uses a new kind of variables, named "row" variables, to record the presence and absence of fields (resp. cases) in a given type. Some approaches, like OCaml's polymorphic variants and object types, use row polymorphism exclusively to simulate subtype polymorphism, in order to avoid subtyping in the wider languages. However, row polymorphism and subtyping actually complement each other well, and neither is as flexible without the other [Pottier 1998b, Chapter 14.7]. There are also techniques for supporting variant and record extensibility through union, intersection, and negation types, as shown by Castagna et al. [2016], who also explain that their system resolves long-standing limitations with OCaml-style row polymorphism. In our system, we solve many (though not all) of these limitations, but we also support principal type inference. It is worth pointing out that OCaml's polymorphic variants [Garrigue 2001] and related systems based on kinds [Ohori 1995] lack support for polymorphic extension [Gaster and Jones 1996; White 2015], whereas MLstruct does (see mapSome in the introduction). As a simpler example, def foo x dflt els = case x of \{ Apple → dflt | _ → els $x$ \} would be assigned a too restrictive type in OCaml and as a consequence foo (Banana \{\}) 0 (fun $z \rightarrow$ case $z$ of $\{$ Banana $\rightarrow 1\}$ ) would not type check (OCaml would complains that the function argument does not handle Apple). A more expressive row-polymorphic system exposing row variables to users would support this use case [Gaster and Jones 1996; Rémy 1994], but as explained in the introduction, even these have limitations compared to our subtyped unions.

\section*{7 CONCLUSION AND FUTURE WORK}

In this paper, we saw that polymorphic type inference for first-class union, intersection, and negation types is possible, enabling class-instance matching patterns yielding very precise types, comparable in expressiveness to row-polymorphic variants. We saw that this type inference approach relies on two crucial aspects of MLstruct's type system: 1. using the full power of Boolean algebras to normalize types and massage constraints into shapes amenable to constraint solving without backtracking; and 2. approximating some unions and intersections, most notably unions of records and intersections of functions, in order to remove potential ambiguities during constraint solving without threatening the soundness of the system.

Future Work. In the future, we intend to explore more advanced forms of polymorphism present in MLscript, such as first-class polymorphism, as well as how to remove some of the limitations of regular types, which currently prevent fully supporting object-oriented programming idioms.

Acknowledgements. We would like to sincerely thank the anonymous reviewers as well as François Pottier, Didier Rémy, Alan Mycroft, Bruno C. d. S. Oliveira, Andong Fan, and Anto Chen for their constructive and helpful comments on earlier versions of this paper. We are particularly grateful to Stephen Dolan, who gave us some invaluable feedback and mathematical intuitions on the development of this new algebraic subtyping system.

\section*{REFERENCES}

Martin Abadi and Marcelo P. Fiore. 1996. Syntactic considerations on recursive types. In Proceedings 11th Annual IEEE Symposium on Logic in Computer Science. IEEE, 242-252. ⟶ page 27
Alexander Aiken. 1996. Making set-constraint program analyses scale. In In CP96 Workshop on Set Constraints. ↪ page 28
Alexander Aiken and Edward L. Wimmers. 1993. Type Inclusion Constraints and Type Inference. In Proceedings of the Conference on Functional Programming Languages and Computer Architecture (Copenhagen, Denmark) (FPCA '93). Association for Computing Machinery, New York, NY, USA, 31-41. https://doi.org/10.1145/165180.165188 ↪ pages 11, 27, and 28
Alexander Aiken, Edward L. Wimmers, and T. K. Lakshman. 1994. Soft Typing with Conditional Types. In Proceedings of the 21st ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (Portland, Oregon, USA) (POPL '94).

Association for Computing Machinery, New York, NY, USA, 163-173. https://doi.org/10.1145/174675.177847 pages 27 and 28
João Alpuim, Bruno C. d. S. Oliveira, and Zhiyuan Shi. 2017. Disjoint Polymorphism. In Programming Languages and Systems, Hongseok Yang (Ed.). Springer Berlin Heidelberg, Berlin, Heidelberg, 1-28. ↪ page 26
Roberto M. Amadio and Luca Cardelli. 1993. Subtyping Recursive Types. ACM Trans. Program. Lang. Syst. 15, 4 (Sept. 1993), 575-631. https://doi.org/10.1145/155183.155231 ↔ pages 16 and 27
Andrew W. Appel, Paul-André Melliès, Christopher D. Richards, and Jérôme Vouillon. 2007. A Very Modal Model of a Modern, Major, General Type System. In Proceedings of the 34th Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (Nice, France) (POPL '07). Association for Computing Machinery, New York, NY, USA, 109-122. https://doi.org/10.1145/1190216.1190235 ↔ pages 17 and 27
F. Barbanera, M. Dezaniciancaglini, and U. Deliguoro. 1995. Intersection and Union Types: Syntax and Semantics. Information and Computation 119, 2 (1995), 202-230. https://doi.org/10.1006/inco.1995.1086 ↪ page 26
Henk Barendregt, Mario Coppo, and Mariangiola Dezani-Ciancaglini. 1983. A filter lambda model and the completeness of type assignment. Journal of Symbolic Logic 48, 4 (1983), 931-940. https://doi.org/10.2307/2273659 page 26
David Binder, Ingo Skupin, David Läwen, and Klaus Ostermann. 2022. Structural Refinement Types. In Proceedings of the 7 th ACM SIGPLAN International Workshop on Type-Driven Development (TyDe '22). Association for Computing Machinery, New York, NY, USA. https://doi.org/10.1145/3546196.3550163 ↪ pages 26 and 27
Michael Brandt and Fritz Henglein. 1998. Coinductive axiomatization of recursive type equality and subtyping. Fundamenta Informaticae 33, 4 (1998), 309-338.
Giuseppe Castagna. 2012. Object-Oriented Programming A Unified Foundation. Springer Science \& Business Media. ↪ page 17
Giuseppe Castagna, Victor Lanvin, Mickaël Laurent, and Kim Nguyen. 2021. Revisiting Occurrence Typing. arXiv:1907.05590 [cs.PL] ⟷ page 29
Giuseppe Castagna, Mickaël Laurent, Kim Nguyundefinedn, and Matthew Lutze. 2022. On Type-Cases, Union Elimination, and Occurrence Typing. Proc. ACM Program. Lang. 6, POPL, Article 13 (jan 2022), 31 pages. https://doi.org/10.1145/3498674 ⟷ pages 26 and 29
Giuseppe Castagna, Tommaso Petrucciani, and Kim Nguyen. 2016. Set-theoretic types for polymorphic variants. In Proceedings of the 21st ACM SIGPLAN International Conference on Functional Programming (ICFP 2016). Association for Computing Machinery, Nara, Japan, 378-391. https://doi.org/10.1145/2951913.2951928 pages 27, 29, and 30
M. Coppo and M. Dezani-Ciancaglini. 1980. An extension of the basic functionality theory for the $\lambda$-calculus. Notre Dame Journal of Formal Logic 21, 4 (1980), 685-693. https://doi.org/10.1305/ndjfl/1093883253 ↪ page 26
Pavel Curtis. 1990. Constrained Qualification in Polymorphic Type Analysis. Ph.D. Dissertation. USA. UMI Order No. GAX90-26980. $\hookrightarrow$ page 28
Bruno C. d. S. Oliveira, Cui Shaobo, and Baber Rehman. 2020. The Duality of Subtyping. In 34th European Conference on Object-Oriented Programming (ECOOP 2020) (Leibniz International Proceedings in Informatics (LIPIcs), Vol. 166), Robert Hirschfeld and Tobias Pape (Eds.). Schloss Dagstuhl-Leibniz-Zentrum für Informatik, Dagstuhl, Germany, 29:1-29:29. https://doi.org/10.4230/LIPIcs.ECOOP.2020.29 ⇝ page 13
Luis Damas and Robin Milner. 1982. Principal type-schemes for functional programs. In Proceedings of the 9th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '82). Association for Computing Machinery, Albuquerque, New Mexico, 207-212. https://doi.org/10.1145/582153.582176 ↔ page 28
Rowan Davies and Frank Pfenning. 2000. Intersection Types and Computational Effects. In Proceedings of the Fifth ACM SIGPLAN International Conference on Functional Programming (ICFP '00). Association for Computing Machinery, New York, NY, USA, 198-208. https://doi.org/10.1145/351240.351259 ↪ page 26
Van Bakel Dezani-Ciancaglini, S. Van Bakel, M. Dezani-ciancaglini, and Y. Motohama. 1998. The Minimal Relevant Logic and the Call-by-Value Lambda Calculus. Technical Report. $\hookrightarrow$ page 26
Stephen Dolan. 2017. Algebraic subtyping. Ph.D. Dissertation. ↔ pages 3, 6, 9, 13, 16, 17, 27, 28, and 29
Stephen Dolan and Alan Mycroft. 2017. Polymorphism, subtyping, and type inference in MLsub. ACM SIGPLAN Notices 52, 1 (Jan. 2017), 60-72. https://doi.org/10.1145/3093333.3009882 ⇝ pages 1 and 28
Jana Dunfield. 2012. Elaborating Intersection and Union Types. In Proceedings of the 17th ACM SIGPLAN International Conference on Functional Programming (Copenhagen, Denmark) (ICFP '12). Association for Computing Machinery, New York, NY, USA, 17-28. https://doi.org/10.1145/2364527.2364534 ↪ page 26
Jonathan Eifrig, Scott Smith, and Valery Trifonov. 1995. Sound Polymorphic Type Inference for Objects. In Proceedings of the Tenth Annual Conference on Object-Oriented Programming Systems, Languages, and Applications (Austin, Texas, USA) (OOPSLA '95). Association for Computing Machinery, New York, NY, USA, 169-184. https://doi.org/10.1145/217838.217858
⟼ page 28
Tim Freeman and Frank Pfenning. 1991. Refinement Types for ML. In Proceedings of the ACM SIGPLAN 1991 Conference on Programming Language Design and Implementation (PLDI '91). ACM, New York, NY, USA, 268-277. https://doi.org/10. 1145/113445.113468 event-place: Toronto, Ontario, Canada. ⇝ page 26
A. Frisch, G. Castagna, and V. Benzaken. 2002. Semantic subtyping. In Proceedings 17th Annual IEEE Symposium on Logic in Computer Science. 137-146. https://doi.org/10.1109/LICS.2002.1029823 ↪ page 29
Alain Frisch, Giuseppe Castagna, and Véronique Benzaken. 2008. Semantic Subtyping: Dealing Set-Theoretically with Function, Union, Intersection, and Negation Types. 7. ACM 55, 4, Article 19 (Sept. 2008), 64 pages. https://doi.org/10. 1145/1391289.1391293 ஒ pages 27 and 29
You-Chin Fuh and Prateek Mishra. 1989. Polymorphic subtype inference: Closing the theory-practice gap. In TAPSOFT '89, J. Díaz and F. Orejas (Eds.). Springer Berlin Heidelberg, Berlin, Heidelberg, 167-183. ↪ page 28

You-Chin Fuh and Prateek Mishra. 1990. Type inference with subtypes. Theoretical Computer Science 73, 2 (1990), 155-175. https://doi.org/10.1016/0304-3975(90)90144-7 ஒ page 28
Vladimir Gapeyev, Michael Y Levin, and Benjamin C Pierce. 2002. Recursive subtyping revealed. Journal of Functional Programming 12, 6 (2002), 511-548. $\hookrightarrow$ pages 27 and 28
Jacques Garrigue. 1998. Programming with polymorphic variants. In ML Workshop, Vol. 13. Baltimore, 7. ↪ page 29
Jacques Garrigue. 2001. Simple Type Inference for Structural Polymorphism.. In APLAS. 329-343. ↔ pages 3, 29, and 30
Benedict R. Gaster and Mark P. Jones. 1996. A Polymorphic Type System for Extensible Records and Variants. ↪ page 30
Silvia Ghilezan. 1996. Strong Normalization and Typability with Intersection Types. Notre Dame Journal of Formal Logic 37, 1 (1996), 44-52. https://doi.org/10.1305/ndjfl/1040067315 ↪ page 26
Roger Hindley. 1969. The Principal Type-Scheme of an Object in Combinatory Logic. Trans. Amer. Math. Soc. 146 (1969), 29-60. https://doi.org/10.2307/1995158 Publisher: American Mathematical Society. $\hookrightarrow$ page 28
Haruo Hosoya, Jérôme Vouillon, and Benjamin C. Pierce. 2005. Regular Expression Types for XML. ACM Trans. Program. Lang. Syst. 27, 1 (Jan. 2005), 46-90. https://doi.org/10.1145/1053468.1053470 pages 17 and 27
Xuejing Huang and Bruno C. d. S. Oliveira. 2021. Distributing Intersection and Union Types with Splits and Duality (Functional Pearl). Proc. ACM Program. Lang. 5, ICFP, Article 89 (aug 2021), 24 pages. https://doi.org/10.1145/3473594 ⟼ page 26
Edward V. Huntington. 1904. Sets of independent postulates for the algebra of logic. Trans. Amer. Math. Soc. 5, 3 (1904), 288-309. https://doi.org/10.1090/s0002-9947-1904-1500675-4
Trevor Jim and Jens Palsberg. 1999. Type Inference in Systems of Recursive Types With Subtyping. ↪ pages 27 and 28
Timothy Jones, Michael Homer, and James Noble. 2015. Brand Objects for Nominal Typing. In 29th European Conference on Object-Oriented Programming (ECOOP 2015) (Leibniz International Proceedings in Informatics (LIPIcs), Vol. 37), John Tang Boyland (Ed.). Schloss Dagstuhl - Leibniz-Zentrum für Informatik, Dagstuhl, Germany, 198-221. https://doi.org/10. 4230/LIPIcs.ECOOP. $2015.198 \hookrightarrow$ page 29
Dexter Kozen, Jens Palsberg, and Michael I. Schwartzbach. 1994. Efficient inference of partial types. 7. Comput. System Sci. 49, 2 (1994), 306-324. https://doi.org/10.1016/S0022-0000(05)80051-0 ↔ page 28
Joseph Lee, Jonathan Aldrich, Troy Shaw, and Alex Potanin. 2015. A Theory of Tagged Objects. In 29th European Conference on Object-Oriented Programming (ECOOP 2015) (Leibniz International Proceedings in Informatics (LIPIcs), Vol. 37), John Tang Boyland (Ed.). Schloss Dagstuhl - Leibniz-Zentrum für Informatik, Dagstuhl, Germany, 174-197. https://doi.org/10. 4230/LIPIcs.ECOOP.2015.174 ↔ page 29
David MacQueen, Gordon Plotkin, and Ravi Sethi. 1986. An ideal model for recursive polymorphic types. Information and Control 71, 1 (1986), 95-130. https://doi.org/10.1016/S0019-9958(86)80019-5 ↔ pages 26 and 27
Robin Milner. 1978. A theory of type polymorphism in programming. 7. Comput. System Sci. 17, 3 (Dec. 1978), 348-375. https://doi.org/10.1016/0022-0000(78)90014-4 4 page 28
John C. Mitchell. 1984. Coercion and Type Inference. In Proceedings of the 11th ACM SIGACT-SIGPLAN Symposium on Principles of Programming Languages (Salt Lake City, Utah, USA) (POPL '84). Association for Computing Machinery, New York, NY, USA, 175-185. https://doi.org/10.1145/800017.800529 ↪ page 28
James Hiram Morris. 1969. Lambda-calculus models of programming languages. Ph.D. Dissertation. Massachusetts Institute of Technology. $\hookrightarrow$ page 27
Fabian Muehlboeck and Ross Tate. 2018. Empowering Union and Intersection Types with Integrated Subtyping. Proc. ACM Program. Lang. 2, OOPSLA, Article 112 (Oct. 2018), 29 pages. https://doi.org/10.1145/3276482 ↔ pages 4, 6, 26, and 29
Martin Odersky, Philippe Altherr, Vincent Cremet, Burak Emir, Sebastian Maneth, Stéphane Micheloud, Nikolay Mihaylov, Michel Schinz, Erik Stenman, and Matthias Zenger. 2004. An overview of the Scala programming language. (2004). ⟷ page 4
Martin Odersky, Martin Sulzmann, and Martin Wehr. 1999. Type inference with constrained types. Theory and Practice of Object Systems 5, 1 (1999), 35-55. ⟶ page 28
Atsushi Ohori. 1995. A Polymorphic Record Calculus and Its Compilation. ACM Trans. Program. Lang. Syst. 17, 6 (nov 1995), 844-895. https://doi.org/10.1145/218570.218572
Jens Palsberg and Christina Pavlopoulou. 1998. From Polyvariant Flow Information to Intersection and Union Types. In Proceedings of the 25th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (San Diego, California, USA) (POPL '98). Association for Computing Machinery, New York, NY, USA, 197-208. https://doi.org/10.1145/268946.
$268963 \hookrightarrow$ page 26
Jens Palsberg, Mitchell Wand, and Patrick O'Keefe. 1997. Type inference with non-structural subtyping. Formal Aspects of Computing 9, 1 (Jan. 1997), 49-67. https://doi.org/10.1007/BF01212524 ↔ page 28
Lionel Parreaux. 2020. The Simple Essence of Algebraic Subtyping: Principal Type Inference with Subtyping Made Easy (Functional Pearl). Proc. ACM Program. Lang. 4, ICFP, Article 124 (Aug. 2020), 28 pages. https://doi.org/10.1145/3409006 ↪ pages 10, 12, 13, 26, 27, and 29
Lionel Parreaux, Luyu Cheng, Tony Chau, Ishan Bhanuka, Andong Fan, Malcolm Law, Ali Mahzoun, and Elise Rouillé. 2022. MLstruct: Principal Type Inference in a Boolean Algebra of Structural Types (Artifact). https://doi.org/10.5281/zenodo. $7121838 \hookrightarrow$ pages 3 and 4
David J. Pearce. 2013. Sound and Complete Flow Typing with Unions, Intersections and Negations. In Verification, Model Checking, and Abstract Interpretation (Lecture Notes in Computer Science), Roberto Giacobazzi, Josh Berdine, and Isabella Mastroeni (Eds.). Springer, Berlin, Heidelberg, 335-354. https://doi.org/10.1007/978-3-642-35873-9_21 ↔ page 22
Tommaso Petrucciani. 2019. Polymorphic set-theoretic types for functional languages. Ph.D. Dissertation. Università di Genova; Université Sorbonne Paris Cité - Université Paris Diderot. ⟶ pages 27 and 29
Benjamin C Pierce. 1991. Programming with intersection types and bounded polymorphism. Ph.D. Dissertation. Citeseer. ⟼ pages 9 and 26
Benjamin C. Pierce. 2002. Types and programming languages. MIT press. $\hookrightarrow$ pages 16,21 , and 27
François Pottier. 1996. Simplifying Subtyping Constraints. In Proceedings of the First ACM SIGPLAN International Conference on Functional Programming (Philadelphia, Pennsylvania, USA) (ICFP '96). Association for Computing Machinery, New York, NY, USA, 122-133. https://doi.org/10.1145/232627.232642 ⟷ page 28
François Pottier. 1998a. A Framework for Type Inference with Subtyping. In Proceedings of the Third ACM SIGPLAN International Conference on Functional Programming (Baltimore, Maryland, USA) (ICFP '98). Association for Computing Machinery, New York, NY, USA, 228-238. https://doi.org/10.1145/289423.289448 ↔ page 28
François Pottier. 1998b. Type Inference in the Presence of Subtyping: from Theory to Practice. Research Report RR-3483. INRIA. https://hal.inria.fr/inria-00073205 ↔ pages 6, 26, 27, 28, 29, and 30
François Pottier. 2001. Simplifying Subtyping Constraints: A Theory. Information and Computation 170, 2 (2001), 153-183. https://doi.org/10.1006/inco.2001.2963 ↪ page 28
François Pottier. 2003. A Constraint-Based Presentation and Generalization of Rows. In IEEE Symposium on Logic In Computer Science (LICS). Ottawa, Canada, 331-340. http://cambium.inria.fr/~fpottier/publis/fpottier-lics03.pdf $\hookrightarrow$ page 3
Baber Rehman, Xuejing Huang, Ningning Xie, and Bruno C. d. S. Oliveira. 2022. Union Types with Disjoint Switches. In 36th European Conference on Object-Oriented Programming (ECOOP 2022) (Leibniz International Proceedings in Informatics (LIPIcs), Vol. 222), Karim Ali and Jan Vitek (Eds.). Schloss Dagstuhl - Leibniz-Zentrum für Informatik, Dagstuhl, Germany, 25:1-25:31. https://doi.org/10.4230/LIPIcs.ECOOP.2022.25 ↔ pages 26 and 29
Didier Rémy. 1994. Type Inference for Records in Natural Extension of ML. MIT Press, Cambridge, MA, USA, 67-95. pages 3 and 30
John C. Reynolds. 1997. Design of the Programming Language Forsythe. Birkhäuser Boston, Boston, MA, 173-233. https: //doi.org/10.1007/978-1-4612-4118-8_9 ↪ page 7
Nathanael Schärli, Stéphane Ducasse, Oscar Nierstrasz, and Andrew P Black. 2003. Traits: Composable units of behaviour. In European Conference on Object-Oriented Programming. Springer, 248-274. ↪ page 4
Vincent Simonet. 2003. Type Inference with Structural Subtyping: A Faithful Formalization of an Efficient Constraint Solver. In Programming Languages and Systems, Atsushi Ohori (Ed.). Springer Berlin Heidelberg, Berlin, Heidelberg, 283-302. ⟷ page 28
Geoffrey Seward Smith. 1991. Polymorphic type inference for languages with overloading and subtyping. Ph.D. Dissertation. Cornell University. → pages 26 and 28
Scott F. Smith and Tiejun Wang. 2000. Polyvariant Flow Analysis with Constrained Types. In Programming Languages and Systems, Gert Smolka (Ed.). Springer Berlin Heidelberg, Berlin, Heidelberg, 382-396. ↔ page 27
R. Stansifer. 1988. Type Inference with Subtypes. In Proceedings of the 15th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (San Diego, California, USA) (POPL '88). Association for Computing Machinery, New York, NY, USA, 88-97. https://doi.org/10.1145/73560.73568 ↪ page 28
Sam Tobin-Hochstadt and Matthias Felleisen. 2008. The Design and Implementation of Typed Scheme. In Proceedings of the 35th Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (San Francisco, California, USA) (POPL '08). Association for Computing Machinery, New York, NY, USA, 395-406. https://doi.org/10.1145/1328438.1328486 ⟷ page 29
Valery Trifonov and Scott Smith. 1996. Subtyping constrained types. In Static Analysis, Radhia Cousot and David A. Schmidt (Eds.). Springer Berlin Heidelberg, Berlin, Heidelberg, 349-365. ↪ page 27
Leo White. 2015. Row polymorphism. https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf ↔ page 30

\section*{A FORMALIZATION, CONTINUED}

The full formalization does not fit in the main body of the paper, so we give the missing parts here.

\section*{A. 1 Declarative Typing Rules}

The declarative typing rules of $\lambda\urcorner$ are presented in Figure 9.
Rule T-Body is used to type programs that happen to be simple terms, after having accumulated a set of declarations in the context $\mathcal{D}$, which is checked for well-formedness using the rules presented in Figure 10 and explained later (Section A.2).

In T-DeF, we type the body of a def inside a constraining context $\Xi$ added on top of the current declarations context, and subsequently use $\Xi$ as part of the resulting polymorphic type of this def, which is placed into the typing context for use later in the program. Importantly, $\Xi$ has to be checked for consistency, which is done with the $\Xi$ cons. judgement, defined in Figure $9-$ essentially, this makes sure that there is at least one assignment of variable that makes the constraints hold in the base declarations context. This is to forbid the use of inconsistent bounds on type variables, such as (Bool $\leqslant \alpha) \cdot(\alpha \leqslant \ln t)$, which could lead to accepting ill-typed definitions.

As a concrete example for T-Def, consider a definition such as def $f=\lambda x . x+1$ in a program where a type synonym type $A=\operatorname{Int}$ is defined. One hypothetical judgement used to type this definition could be '(type $A=\operatorname{lnt}) \cdot(\alpha \leqslant A), \Gamma \vdash \lambda x \cdot x+1: \alpha \rightarrow \operatorname{Int}$ ' where $\Xi=(\alpha \leqslant A)$ is the constraints part of the context. According to T-Def, because $\Xi$ is consistent (since $l b_{\Xi}(\alpha)=\perp \leqslant u b_{\Xi}(\alpha)=$ Int), we can type the definition $f$ as ' $\forall(\alpha \leqslant A)$. $\alpha \rightarrow$ Int'. As a side note, this type can be rewritten to $f: A \rightarrow A$, which is equivalent in the declarations context (type $A=\operatorname{lnt}$ ).

Rule T-Var2 is an interesting counterpart to rule T-Def explained above. It instantiates a given polymorphic type through the $\leqslant^{\forall}$ relation defined by rule S-All.

Rule S-All uses a substitution $\rho$, a premise that the subtyping holds under this substitution, and the entailment judgement $\Sigma \cdot \Xi^{\prime} \models \rho(\Xi)$, which simply makes sure that every subtyping constraint in $\rho(\Xi)$ holds in $\Sigma$ with $\Xi^{\prime}$ (which is $\epsilon$ for T-Var2). Condition $\operatorname{dom}(\rho)=T V(\Xi) \cup T V(\tau)$, where $T V(\cdot)$ is defined in Section A.3, is used to make sure that $\rho$ assigns a substitution to all the variables quantified by the polymorphic type.

\section*{A.1.1 Superclasses.}

Definition A. 1 (Superclasses). We define the superclasses $\mathcal{S}(\tau)$ of a type $\tau$ as the set of classes transitively inherited by type $\tau$, assuming $\tau$ is a class type or the expansion of a class type:
$$
\overline{C \in \mathcal{S}(\# C)} \quad \frac{C \in \mathcal{S}(\# D)}{C \in \mathcal{S}(D[\bar{\tau}])} \quad \frac{\tau \exp . \tau^{\prime} \quad C \in \mathcal{S}\left(\tau^{\prime}\right)}{C \in \mathcal{S}(\tau)} \quad \frac{C \in \mathcal{S}\left(\tau_{1}\right) \cup \mathcal{S}\left(\tau_{2}\right)}{C \in \mathcal{S}\left(\tau_{1} \wedge \tau_{2}\right)}
$$

\section*{A.1.2 Substitution.}

Definition A. 2 (Term substitution). A term substitution is a pair of variable and term $[x \mapsto t]$. Applying a term substitution to a term $t^{\prime}$, denoted by $[x \mapsto t] t^{\prime}$, replaces all free occurrences of $x$ in $t^{\prime}$ with $t$, which is defined as follows:
$$
\left.\begin{array}{rlr}
{[x \mapsto t] y} & = \begin{cases}t & \text { if } y=x \\
y & \text { if } y \neq x\end{cases} & {[x \mapsto t]\left(t_{0} t_{1}\right)=[x \mapsto t] t_{0}[x \mapsto t] t_{1}} \\
{[x \mapsto t]\left(t^{\prime}: \tau\right)=[x \mapsto t] t^{\prime}: \tau} & {[x \mapsto t] t^{\prime} \cdot x^{\prime}=\left([x \mapsto t] t^{\prime}\right) \cdot x^{\prime}}
\end{array}\right] \begin{array}{lll}
{[x \mapsto t] \lambda x^{\prime} \cdot t^{\prime}} & =\left\{\begin{array}{lll}
\lambda x^{\prime} \cdot t^{\prime} & \text { if } x^{\prime}=x & {[x \mapsto t]\left(C\left\{\overline{x^{\prime}=t^{\prime}}\right\}\right)=C\left\{\overline{x^{\prime}=[x \mapsto t] t^{\prime}}\right\}} \\
\lambda x^{\prime} \cdot[x \mapsto t] t^{\prime} & \text { if } x^{\prime} \neq x & \text { if } x^{\prime}=x
\end{array}\right. \\
{[x \mapsto t] \text { case } x^{\prime}=t^{\prime} \text { of } M= \begin{cases}\text { case } x^{\prime}=[x \mapsto t] t^{\prime} \text { of } M & \text { if } x^{\prime} \neq x \\
\text { case } x^{\prime}=[x \mapsto t] t^{\prime} \text { of }[x \mapsto t] M\end{cases} }
\end{array}
$$

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-035.jpg?height=1697&width=1358&top_left_y=300&top_left_x=150}
\captionsetup{labelformat=empty}
\caption{Fig. 9. Full declarative typing, consistency, and subtyping entailment rules.}
\end{figure}

Where case branches term substitution $[x \mapsto t] M$ is defined as:
$[x \mapsto t] \epsilon=\epsilon \quad[x \mapsto t]\left(\_\rightarrow t^{\prime}\right)=\_\rightarrow[x \mapsto t] t^{\prime} \quad[x \mapsto t]\left(C \rightarrow t^{\prime}, M\right)=C \rightarrow[x \mapsto t] t^{\prime},[x \mapsto t] M$

Similarly, applying a term substitution to a program $P$, denoted by $[x \mapsto t] P$, replaces all free occurrences of $x$ in $P$ with $t$, which is defined as follows:
$$
[x \mapsto t]\left(\operatorname{def} x^{\prime}=t^{\prime} ; P\right)= \begin{cases}\operatorname{def} x^{\prime}=t^{\prime} ; P & \text { if } x^{\prime}=x \\ \operatorname{def} x^{\prime}=[x \mapsto t] t^{\prime} ;[x \mapsto t] P & \text { if } x^{\prime} \neq x\end{cases}
$$

Definition A. 3 (Type substitution). A type substitution $\rho=\{\overline{\alpha \longmapsto \tau}\}$ is a mapping from type variables to types.

We use the notation $\left(\alpha_{1} \mapsto \tau_{1}\right) \in \rho$ to signify that $\alpha_{1} \in \operatorname{dom}(\rho)$ and $\rho\left(\alpha_{1}\right)=\tau_{1}$.
$\operatorname{dom}(\rho)$ is the domain of $\rho$, defined as follows:
$$
\operatorname{dom}\left(\})=\varnothing \quad \operatorname{dom}\left(\left\{\overline{\alpha \longmapsto \tau}, \alpha^{\prime} \mapsto \tau^{\prime}\right\}\right)=\operatorname{dom}(\{\overline{\alpha \longmapsto \tau}\}) \cup\left\{\alpha^{\prime}\right\}\right.
$$

Definition A. 4 (Type substitution on type). Application of a type substitution to a type $\rho(\tau)$ is defined as follows:
$$
\begin{aligned}
\rho\left(\tau_{1} \rightarrow \tau_{2}\right) & =\rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right) & \rho(\alpha) & = \begin{cases}\tau & \text { if }(\alpha \mapsto \tau) \in \rho \\
\alpha & \text { if } \alpha \notin \operatorname{dom}(\rho)\end{cases} \\
\rho(\{x: \tau\}) & =\{x: \rho(\tau)\} & \rho\left(T^{\diamond}\right) & =T^{\diamond} \\
\rho(N[\bar{\tau}]) & =N[\overline{\rho(\tau)}] & \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & =\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right) \\
\rho(\# C) & =\# C & \rho(\neg \tau) & =\neg \rho(\tau)
\end{aligned}
$$

Definition A. 5 (Type substitution on term). Application of a type substitution to a term $\rho(t)$ is defined as follows:
$$
\begin{array}{rlrl}
\rho(x) & =x & \rho(t . x) & =\rho(t) . x \\
\rho(t: \tau) & =\rho(t): \rho(\tau) & \rho(C\{\overline{x=t}\}) & =C\{\overline{x=\rho(t)}\} \\
\rho(\lambda x . t) & =\lambda x . \rho(t) & \rho(\text { case } x=t \text { of } M) & =\text { case } x=\rho(t) \text { of } \rho(M) \\
\rho\left(t_{0} t_{1}\right) & =\rho\left(t_{0}\right) \rho\left(t_{1}\right) &
\end{array}
$$

Where type substitution $\rho(M)$ on case branches is defined as:
$$
\rho(\epsilon)=\epsilon \quad \rho\left(z_{-} \rightarrow t\right)=_{-} \rightarrow \rho(t) \quad \rho(C \rightarrow t, M)=C \rightarrow \rho(t), \rho(M)
$$

Definition A. 6 (Type substitution on typing context). Application of a type substitution to a typing context $\rho(\Gamma)$ is defined as follows:
$$
\rho(\epsilon)=\epsilon \quad \rho(\Gamma \cdot(x: \tau))=\rho(\Gamma) \cdot(x: \rho(\tau)) \quad \rho(\Gamma \cdot(x: \sigma))=\rho(\Gamma) \cdot(x: \sigma)
$$

Definition A. 7 (Type substitution on subtyping context). Application of a type substitution to a subtyping context $\rho(\Sigma)$ is defined as follows:
$\rho(\epsilon)=\epsilon$
$$
\rho\left(\Sigma \cdot\left(\tau_{1} \leqslant \tau_{2}\right)\right)=\rho(\Sigma) \cdot\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right) \quad \rho\left(\Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right)\right)=\rho(\Sigma) \cdot \triangleright\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right)
$$

\section*{A. 2 Well-Formedness}

The well-formedness rules are presented in Figure 10. They ensure that the declarations of a program lead to a decidable type inference algorithm by restricting the shapes of recursive types to regular trees. This is done by making sure that all recursive occurrences of class and type declarations are given the same type arguments $\bar{\alpha}$ as the declaration's head $N[\bar{\alpha}]$ itself. Note that well-formed type declaration may refer to each other freely, possibly forming mutually-recursive definitions.

Definition A. 8 (Occurrences). We define the occurrences of a type $\tau$, written occs $(\tau)$, as all the types transitively reachable by progressively traversing the subterms of $\tau$ and expanding the alias and class types as we encounter them. This is always a finite set, thanks to the regularity check (Section 2.3.1).

The type variables of a piece of syntax $s$, written $T V(s)$, is defined in Section A.3. Function $\operatorname{guard}_{N}(\tau)$ refers to the guardedness check described in Section 2.1.6.

Theorem A. 9 (Regularity). If $\mathcal{D} \boldsymbol{w}$, then for all $\tau$, the set occs $(\tau)$ is finite.

This notably means that given well-formed declarations $\mathcal{D}$, we can easily compute $\mathcal{S}(\tau)$.

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-037.jpg?height=1210&width=1382&top_left_y=644&top_left_x=152}
\captionsetup{labelformat=empty}
\caption{Fig. 10. Well-formedness and finality rules.}
\end{figure}

Proof A. 9 (Regularity). Since each type constructor declared as $N[\bar{\alpha}]$ can only appear in its body (and transitively in the bodies of other declarations) with the same type variables $\bar{\alpha}$ as type arguments, the expansion $\tau$ of a type $N[\bar{\pi}]$ may only lead to $N$ occurrences of the form $N[\bar{\pi}]$, which itself has the same occurrences as $\tau$; thus the number of distinct type occurrences transitively reachable from a given declaration is finite. \(\square\)

\section*{A. 3 Free type variables}

Definition A. 10 (Free type variables). The set of free type variables of a type $\tau$, written $T V(\tau)$, is defined as:
$$
\begin{aligned}
T V\left(\tau_{1} \rightarrow \tau_{2}\right) & =T V\left(\tau_{1}\right) \cup T V\left(\tau_{2}\right) & T V(\alpha) & =\{\alpha\} \\
T V(\{x: \tau\}) & =T V(\tau) & T V\left(T^{\diamond}\right) & =\varnothing \\
T V(\# C) & =\varnothing & T V\left(\tau_{1} \vee \tau_{2}\right) & =T V\left(\tau_{1}\right) \cup T V\left(\tau_{2}\right) \\
T V(N[\bar{\tau}]) & =\bigcup_{\bar{\tau}} T V(\tau) & T V(\neg \tau) & =T V(\tau)
\end{aligned}
$$

Definition A. 11 (Free type variables of declaration context). The free type variables of a declaration context $T V(\mathcal{D})$ is defined as:
$$
\begin{gathered}
T V(\epsilon)=\varnothing \quad T V(\mathcal{D} \cdot(\text { class } C[\bar{\alpha}]: \tau))=T V(\mathcal{D}) \cup(T V(\tau) \backslash\{\bar{\alpha}\}) \\
T V(\mathcal{D} \cdot(\text { type } A[\bar{\alpha}]=\tau))=T V(\mathcal{D}) \cup(T V(\tau) \backslash\{\bar{\alpha}\})
\end{gathered}
$$

Definition A. 12 (Free type variables of typing context). The free type variables of a typing context $T V(\Gamma)$ is defined as:
$$
T V(\epsilon)=\varnothing \quad T V(\Gamma \cdot(x: \tau))=T V(\Gamma) \cup T V(\tau) \quad T V(\Gamma \cdot(X: \sigma))=T V(\Gamma)
$$

Definition A. 13 (Free type variables of constraining context). The free type variables of a constraining context $T V(\Xi)$ is defined as:
$$
T V(\epsilon)=\varnothing \quad T V(\Xi \cdot(\alpha \leqslant \tau))=T V(\Xi) \cup\{\alpha\} \cup T V(\tau)
$$

Definition A. 14 (Top-level free type variables). The set of top-level free type variables of a type $\tau$, written $\operatorname{TTV}(\tau)$, is defined as:
$$
\begin{aligned}
T T V\left(\tau_{1} \rightarrow \tau_{2}\right) & =\varnothing \\
T T V(\{x: \tau\}) & =\varnothing \\
T T V(\# C) & =\varnothing \\
T T V(N[\bar{\tau}]) & =T T V\left(\tau^{\prime}\right) \quad \text { when } N[\bar{\tau}] \text { exp. } \tau^{\prime}
\end{aligned}
$$
$$
\begin{aligned}
T T V(\alpha) & =\{\alpha\} \\
T T V\left(T^{\diamond}\right) & =\varnothing \\
T T V\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & =T T V\left(\tau_{1}\right) \cup T T V\left(\tau_{2}\right) \\
T T V(\neg \tau) & =T T V(\tau)
\end{aligned}
$$

The list of top-level free type variables of a type $\tau$ (i.e., with duplicates), written $T T V^{\prime}(\tau)$, is defined similarly, except for the cases $T T V^{\prime}(\alpha)=\alpha$ and $T T V^{\prime}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)=T T V^{\prime}\left(\tau_{1}\right) \cdot T T V^{\prime}\left(\tau_{2}\right)$.

\section*{B FORMAL CORRECTNESS PROOFS}

\section*{B. 1 Subtyping Derivation Shapes}

We first give a few definitions characterizing the shapes of subtyping derivations, and prove properties about them.

Definition B. 1 (Right-leaning derivations). A subtyping derivation is said to be right-leaning if all its applications of rule S-Trans have a first premise which is not itself an application of rule S-Trans.

It is easy to see that any subtyping derivation can be rewritten into an equivalent right-leaning derivation of the same size by reorganizing its uses of S-Trans.

Definition B. 2 (Bottom-level rules). A rule is used at the bottom level in a derivation if it is one of the following:
(1) the last rule used in the derivation;
(2) either premise of a bottom-level application of rule S-Trans;
(3) the premise of a bottom-level application of rule S-Expo;
(4) the first premise of a bottom-level application of rule T-Subs.

Definition B. 3 (Unassuming derivation). An unassuming derivation is a subtyping derivation that does not make use of S-Assum at the bottom level.

Lemma B. 4 (Unassuming derivation). Any subtyping derivation can be rewritten to an equivalent unassuming derivation.

Proof. Consider a derivation $D$ whose last applied rule is S-Assum. This rule application introduces a hypothesis $\triangleright H$ into the context of its premise derivation $D^{\prime}$. In $D^{\prime}, \triangleright H$ is kept unusable (because of the $\triangleright$ ) until applications of rules S-FunDepth or S-RcdDepth, within the premise derivations of which $H$ may be used, through applications $D_{i}^{H}$ of the S-Hyp rule Therefore, $H$ is never used at the bottom level of $D^{\prime}$. Moreover, each $D_{i}^{H}$ will have a premise of the form $\Sigma \cdot H \cdot \Sigma_{i}$. So we can substitute all $D_{i}^{H}$ in $D$ with a weakened form (Lemma B.30) of the derivation $D$ itself. After this substitution, the main application of S-Assum becomes useless (the $H$ it introduces is no longer used in any subderivation), and it can therefore be removed, leaving the updated derivation $D^{\prime}$. It is easy to show that we can perform this S-Assum-elimination on bottom-level subderivations of any given derivation until that derivation becomes unassuming.

Definition B. 5 (Subsumption-normalized derivation). A subsumption-normalized derivation is a typing derivation that makes at most one use of T-Subs at the bottom level.

Lemma B. 6 (Subsumption-normalized derivation). Any typing derivation can be rewritten to an equivalent subsumption-normalized derivation.

Proof. By induction on the number of bottom-level applications of T-Subs.
The result is immediate for derivations with zero or one bottom-level applications of T-Subs.
For derivations with $n \geqslant 2$ bottom-level applications of T-Subs, we first observe that the last two typing rules applied must be T-Subs (indeed, if the last rule applied was not T-Subs, then by definition the derivation would have no bottom-level applications of T-Subs; and the same reasoning goes for the second last application). The premises of the last application of T-Subs are $t: \tau^{\prime}$ and $\tau^{\prime} \leqslant \tau$ for some $\tau^{\prime}$, where the subderivation for $t: \tau^{\prime}$ has $n-1$ bottom-level applications of T-Subs. The premises of the second last application of T-Subs are $t: \tau^{\prime \prime}$ and $\tau^{\prime \prime} \leqslant \tau^{\prime}$ for some $\tau^{\prime \prime}$, where the subderivation for $t: \tau^{\prime \prime}$ has $n-2$ bottom-level applications of T-Subs. The subderivations of $\tau^{\prime \prime} \leqslant \tau^{\prime}$ and $\tau^{\prime} \leqslant \tau$ can be merged by S-Trans into a derivations for $\tau^{\prime \prime} \leqslant \tau$. We can then apply T-Subs to the subderivation for $t: \tau^{\prime \prime}$ and the new derivation for $\tau^{\prime \prime} \leqslant \tau$ to obtain a new derivation for $t: \tau$ with $n-1$ bottom-level applications of T-Subs. By IH, such a derivation can be rewritten to an equivalent subsumption-normalized derivation.

\section*{B. 2 Constraining Context Cleanup}

Constraining context cleanup removes occurrences of a type variable from the top level of its bounds, resulting in an equivalent guarded constraining context.

Definition B. 7 (Constraining context cleanup). The constraining context cleanup function is defined as follows:
$$
\begin{aligned}
& \text { cleanup }(\epsilon)=\epsilon \\
& \text { cleanup }(\Xi \cdot(\alpha \leqslant \tau))=\text { cleanup }(\Xi) \cdot \text { cleanup }^{\prime}(\alpha \leqslant \operatorname{cdn}(\tau)) \\
& \operatorname{cleanup}(\Xi \cdot(\tau \leqslant \alpha))=\operatorname{cleanup}(\Xi) \cdot \operatorname{cleanup}^{\prime}(\operatorname{dcn}(\tau) \leqslant \alpha) \\
& \text { cleanup }^{\prime}\left(\alpha \leqslant \bigwedge_{i} \tau_{i}^{\mathrm{dn}}\right)=\left(\alpha \leqslant \bigwedge_{j} \pi_{j}^{\mathrm{dn}}\right) \quad \text { where } \overline{\text { cleanup }}^{\prime \prime}\left(\alpha \leqslant \tau_{i}^{\mathrm{dn}}\right) ~ i={\overline{\left(\alpha \leqslant \pi_{j}^{\mathrm{dn}}\right)}}^{j} \\
& \text { cleanup }^{\prime}\left(\bigvee_{i} \tau_{i}^{\mathrm{cn}} \leqslant \alpha\right)=\left(\bigvee_{j} \pi_{j}^{\mathrm{cn}} \leqslant \alpha\right) \quad \text { where } \overline{\text { cleanup }}^{\prime \prime}\left(\tau_{i}^{\mathrm{cn}} \leqslant \alpha\right) ~ i={\overline{\left(\pi_{j}^{\mathrm{cn}} \leqslant \alpha\right)}}^{j} \\
& \text { cleanup }^{\prime \prime}\left(\alpha \leqslant \bigvee_{i} \tau_{i}^{\mathrm{n}}\right)= \begin{cases}\epsilon & \text { when } \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \\
\left(\alpha \leqslant \bigvee_{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \alpha} \tau_{i}^{\mathrm{n}}\right) & \text { otherwise }\end{cases} \\
& \text { cleanup }^{\prime \prime}\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \alpha\right)= \begin{cases}\epsilon & \text { when } \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \\
\left(\bigwedge_{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \alpha} \tau_{i}^{\mathrm{n}} \leqslant \alpha\right) & \text { otherwise }\end{cases}
\end{aligned}
$$

Lemma B. 8 (Equivalence of constraining context cleanup). $\overline{H \models \operatorname{cleanup}(H)}^{H \in \Xi}$ for all $\Xi$.
Lemma B. 9 (Guardedness of constraining context cleanup). cleanup $(\Xi)$ guard. for all $\Xi$.
Lemma B. 10 (Equivalence of bounds under constraining context cleanup). $\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge u b_{\text {cleanup }(\Xi)}(\alpha) \vee l b_{\text {cleanup }(\Xi)}(\alpha)$ for all $\Xi$ and $\alpha$.

\section*{B. 3 Some Useful Subtyping Relationships}

Next, we demonstrate a few useful subtyping rules that can be derived in our system.
Theorem B. 11 (Duality of Extrema). $T^{\diamond} \equiv \neg \perp^{\diamond}$
Proof.
Case . We have $\neg \perp \leqslant \top$ by S-ToB. For $T \leqslant \neg \perp$ : We have $T \leqslant \perp \vee \neg \perp$ by S-Compl., which implies $T \leqslant \neg \perp$ by Lemma B.24>.
Case D. We have $\perp \leqslant \neg \top$ by S-ToB . For $\neg \top \leqslant \perp$ : We have $\top \wedge \neg \top \leqslant \perp$ by S-Compl D, which implies $\neg \top \leqslant \perp$ by Lemma B. $24 \cdot$.

Theorem B. 12 (Double Negation Introduction).
$$
\frac{S-N E G 2}{\tau \leqslant \neg \neg \tau}
$$

Proof.
$$
\begin{array}{rr}
\text { S-Compl} \supset \frac{\text { S-ToB } \supset}{\tau \wedge \neg \tau \leqslant \perp} & \text { S-ANDOr } 2 \cdot \frac{\perp \leqslant \neg \neg \tau}{\perp \leqslant} \\
\text { THEOREM B.20} \frac{\text { S-REFL }}{\neg \leqslant \perp \vee \neg \neg \neg}
\end{array}
$$

Theorem B. 13 (Double Negation Elimination).
$$
\frac{S-N E G 1}{\neg \neg \tau \leqslant \tau}
$$

Proof.
$$
\begin{array}{r}
\text { S-AndOr2 } \frac{\neg \neg \tau \leqslant \mathrm{T}}{\neg \neg \tau \leqslant \mathrm{~T} \wedge \neg \neg \tau} \quad \text { S-Refl } \frac{\neg \neg \tau \leqslant \neg \neg \tau}{\neg} \\
\neg \neg \tau \leqslant \tau
\end{array} \text { Theorem B.20. } \frac{\text { S-Compl. } \overline{\mathrm{T} \leqslant \tau \vee \neg \tau}}{\mathrm{~T} \wedge \neg \neg \tau \leqslant \tau}
$$ \(\square\)

Theorem B. 14 (Unique Complementation). For all $\tau_{1}$ and $\tau_{2} \neg \tau_{1} \equiv \neg \tau_{2}$ implies $\tau_{1} \equiv \tau_{2}$, i.e., " $\neg \tau_{1} \leqslant \neg \tau_{2}$ and $\neg \tau_{2} \leqslant \neg \tau_{1}$ "imply" $\tau_{1} \leqslant \tau_{2}$ and $\tau_{2} \leqslant \tau_{1}$ ".

Proof.
$$
\text { S-Trans } \frac{\text { S-Neg } 2 \frac{\text { S-NegINv } \frac{\neg \tau_{n} \leqslant \neg \tau_{m}}{\neg \neg \tau_{m} \leqslant \neg \neg \tau_{n}}}{\tau_{m} \leqslant \neg \neg \tau_{m}} \quad \text { S-Trans } \frac{\text { S-Neg } 1 \longdiv { \neg \tau _ { m } \leqslant \tau _ { n } }}{\neg \neg \tau_{n} \leqslant \tau_{n}}}{\tau_{m} \leqslant \tau_{n}}
$$

Taking $(n, m)=(1,2)$ and $(n, m)=(2,1)$ yields the desired results. \(\square\)

Theorem B. 15 (Associativity).
$$
\frac{\text { S-AssOC } \diamond}{\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \vee^{\diamond} \tau_{3} \equiv \tau_{1} \vee^{\diamond}\left(\tau_{2} \vee^{\diamond} \tau_{3}\right)}
$$

Proof.
$$
\begin{aligned}
& \text { S-AnDOR } 2 \diamond \frac{\text { S-ANDOR } 11 \diamond \overline{(\mathbf{1})\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \vee^{\diamond} \tau_{3} \geqslant^{\diamond} \tau_{1} \vee^{\diamond} \tau_{2}}}{\text { S-TRANS } \frac{\text { (1) }}{\text { S-ANDOR } 12 \diamond \overline{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{2}}} \quad \text { S-ANDOR } 12 \diamond \overline{\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) v^{\diamond} \tau_{3} \geqslant^{\diamond} \tau_{3}}} \\
& \left(\tau_{1} v^{\diamond} \tau_{2}\right) v^{\diamond} \tau_{3} \geqslant^{\diamond} \tau_{2} \\
& \text { (2) }\left(\tau_{1} v^{\diamond} \tau_{2}\right) v^{\diamond} \tau_{3} \geqslant^{\diamond} \tau_{2} v^{\diamond} \tau_{3} \\
& \text { S-ANDOR } 2 \diamond \frac{\text { S-TRANS } \frac{\mathbf{( 1 )}}{\left(\tau_{1} v^{\diamond} \tau_{2}\right) v^{\diamond} \tau_{3} \geqslant^{\diamond} \tau_{1} v^{\diamond}\left(\tau_{2} v^{\diamond} \tau_{3}\right)} \quad \mathbf{( 2 )}}{\tau_{1} v^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{1}}
\end{aligned}
$$

The other direction follows from S-Commut (Theorem B. 16 below). \(\square\)

Theorem B. 16 (Commutativity).
S-Commutb
$$
\overline{\tau_{1} \vee^{\diamond} \tau_{2} \equiv \tau_{2} \vee^{\diamond} \tau_{1}}
$$

Proof.
$$
\text { S-ANDOR } 2 \diamond \frac{\text { S-ANDOR } 12 \diamond \overline{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{2}} \quad \text { S-ANDOR } 11 \diamond \overline{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{1}}}{\tau_{1} \vee^{\diamond} \tau_{2} \geqslant^{\diamond} \tau_{2} \vee^{\diamond} \tau_{1}}
$$ \(\square\)

Theorem B. 17 (Distributivity).
$$
\frac{\text { S-DISTR }}{\tau_{1} \vee^{\diamond}\left(\tau_{2} \wedge^{\diamond} \tau_{3}\right) \equiv\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{3}\right)}
$$

Proof.
Case ◇, $\geqslant$ direction. By S-Distribo.
Case , $\leqslant$ direction.
$\underset{\text { Lemma B.22. } \frac{\text { S-Refl } \frac{\tau_{1} \leqslant \tau_{1}}{\text { S-AndOr11 } \cdot} \frac{\tau_{2} \wedge \tau_{3} \leqslant \tau_{2}}{\tau_{1} \vee\left(\tau_{2} \wedge \tau_{3}\right) \leqslant \tau_{1} \vee \tau_{2}}}{\tau_{1} \vee\left(\tau_{2} \wedge \tau_{3}\right) \leqslant\left(\tau_{1} \vee \tau_{2}\right) \wedge\left(\tau_{1} \vee \tau_{3}\right)} \text { Lemma B.22. } \frac{\text { S-Refl } \frac{\tau_{1} \leqslant \tau_{1}}{\text { S-AndOr12 } \cdot \overline{\tau_{2} \wedge \tau_{3} \leqslant \tau_{3}}}}{\tau_{1} \vee\left(\tau_{2} \wedge \tau_{3}\right) \leqslant \tau_{1} \vee \tau_{3}}}{\text { S-AnDOR2d }}$
Case $2, \geqslant$ direction. Symmetric.

Theorem B. 18 (Absorption).
$$
\frac{\text { S-ABSORP }}{\tau_{1} \vee^{\diamond}\left(\tau_{1} \wedge^{\diamond} \tau_{2}\right) \equiv \tau_{1}}
$$

Proof.
Case ▷ , $\geqslant^{\diamond}$ direction. By S-AndOr11॰.
Case , $\leqslant$ direction.
$$
\text { LeMMA B. } 22 \frac{\text { S-RefL } \frac{1}{\tau_{1} \leqslant \tau_{1}} \quad \text { S-ToB. } \overline{\tau_{1} \leqslant \top}}{\tau_{1} \leqslant \tau_{1} \wedge \top} \quad \text { S-ReFL } \frac{\text { (1) } \tau_{1} \vee\left(\tau_{1} \wedge \tau_{2}\right) \leqslant\left(\tau_{1} \wedge \mathrm{~T}\right) \vee\left(\tau_{1} \wedge \tau_{2}\right)}{\tau_{1} \wedge \tau_{2} \leqslant \tau_{1} \wedge \tau_{2}}
$$

S-Trans $\frac{\text { S-Distrd } \frac{(\mathbf{1})}{\left(\tau_{1} \wedge \mathrm{~T}\right) \vee\left(\tau_{1} \wedge \tau_{2}\right) \leqslant \tau_{1} \wedge\left(\mathrm{~T} \vee \tau_{2}\right)} \text { S-AndOr11} \frac{}{\tau_{1} \wedge\left(\mathrm{~T} \vee \tau_{2}\right) \leqslant \tau_{1}}}{\left(\tau_{1} \wedge \mathrm{~T}\right) \vee\left(\tau_{1} \wedge \tau_{2}\right) \leqslant \tau_{1}}$
Case $>, \geqslant$ direction. Symmetric.

Theorem B. 19 (De Morgan's Laws).
S-DeMorgan
$$
\overline{\neg\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \equiv \neg \tau_{1} \wedge^{\diamond} \neg \tau_{2}}
$$

\section*{Proof.}
$$
\text { Lemma B.22. } \frac{\text { S-Compl. } \frac{\mathrm{T} \leqslant \tau \vee \neg \tau}{\mathrm{~T} \vee \pi \leqslant(\tau \vee \neg \tau) \vee \pi} \quad \text { S-Refl } \frac{\pi \leqslant \pi}{\pi}}{\text { S-Trans }} \quad \text { Lemma B.23. } \frac{}{(\tau \vee \neg \tau) \vee \pi \leqslant(\tau \vee \pi) \vee \neg \tau}
$$
$$
\text { (1) }\rceil \vee \pi \leqslant(\tau \vee \pi) \vee \neg \tau
$$
$$
\text { S-Trans } \frac{\text { S-Refl } \frac{}{\tau \leqslant \tau} \text { S-Compl } \cdot \overline{T \leqslant \pi \vee \neg \pi}}{\tau \vee \top \leqslant \tau \vee(\pi \vee \neg \pi)} \quad \text { S-Assoc } \cdot \overline{\tau \vee(\pi \vee \neg \pi) \leqslant(\tau \vee \pi) \vee \neg \pi}
$$
![](https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-043.jpg?height=222&width=1245&top_left_y=1472&top_left_x=214)
$\neg \tau \wedge \neg \pi \leqslant \neg(\tau \vee \pi)$ can be derived by similar reasoning. \(\square\)

\section*{Theorem B. 20 (Swapping).}
$$
\begin{aligned}
& \text { S-SWAP } \\
& \frac{\Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \geqslant}{\Sigma \vdash \tau_{3}} \\
& \Sigma \tau_{1} \geqslant^{\diamond} \tau_{3} \wedge^{\diamond} \neg \tau_{2}
\end{aligned}
$$

Proof. Case 2. Given (1) $\Sigma \vdash \tau_{1} \wedge \tau_{2} \leqslant \tau_{3}$, derive (2) $\Sigma \vdash \tau_{1} \leqslant \tau_{3} \vee \neg \tau_{2}$ :
$$
\begin{aligned}
& \text { S-Refl } \frac{\text { S-ToB. }}{\neg \tau_{2} \vee \tau_{1} \leqslant \neg \tau_{2} \vee \tau_{1}} \quad \text { S-Trans } \frac{\text { S-Compl } \cdot \overline{\text { T } \leqslant \tau_{2} \vee \neg \tau_{2}}}{\text { S-AnDOR2 } \text { (1) } \neg \tau_{2} \vee \tau_{1} \leqslant\left(\neg \tau_{2} \vee \tau_{1}\right) \wedge\left(\tau_{2} \vee \neg \tau_{2}\right)} \\
& \text { Lemma B.22. } \frac{\text { S-Refl } \overline{\neg \tau_{2} \vee \tau_{1} \leqslant \neg \tau_{2} \vee \tau_{1}} \quad \text { S-Commut. } \overline{\tau_{2} \vee \neg \tau_{2} \leqslant \neg \tau_{2} \vee \tau_{2}}}{\text { (2) }\left(\neg \tau_{2} \vee \tau_{1}\right) \wedge\left(\tau_{2} \vee \neg \tau_{2}\right) \leqslant\left(\neg \tau_{2} \vee \tau_{1}\right) \wedge\left(\neg \tau_{2} \vee \tau_{2}\right)} \\
& \text { S-Trans } \frac{\text { (2) S-Distrib } \overline{\left(\neg \tau_{2} \vee \tau_{2}\right) \wedge\left(\neg \tau_{2} \vee \tau_{2}\right) \leqslant \neg \tau_{2} \vee\left(\tau_{1} \wedge \tau_{2}\right)}}{\text { (3) }\left(\neg \tau_{2} \vee \tau_{1}\right) \wedge\left(\tau_{2} \vee \neg \tau_{2}\right) \leqslant \neg \tau_{2} \vee\left(\tau_{1} \wedge \tau_{2}\right)} \\
& \text { S-Trans } \frac{\text { S-Trans } \frac{\text { S-AndOr12 } \overline{\tau_{1} \leqslant \neg \tau_{2} \vee \tau_{1}}}{\tau_{1} \leqslant\left(\neg \tau_{2} \vee \tau_{1}\right) \wedge\left(\tau_{2} \vee \neg \tau_{2}\right)}}{\text { (4) } \tau_{1} \leqslant \neg \tau_{2} \vee\left(\tau_{1} \wedge \tau_{2}\right)} \\
& \text { S-Trans } \frac{\text { S-Refl } \overline{\neg \tau_{2} \leqslant \neg \tau_{2}} \quad \tau_{1} \wedge \tau_{2} \leqslant \tau_{3}}{\text { Lemma B.22 ⋅ } \frac{\square \tau_{2} \vee\left(\tau_{1} \wedge \tau_{2}\right) \leqslant \neg \tau_{2} \vee \tau_{3}}{\neg \tau_{2}} \quad \text { S-Commut } \frac{}{\neg \tau_{2} \vee \tau_{3} \leqslant \tau_{3} \vee \neg \tau_{2}}} \tau_{1 \leqslant \tau_{3} \vee \neg \tau_{2}}^{\text {S-Trans } \frac{\tau_{2} \vee\left(\tau_{1} \wedge \tau_{2}\right) \leqslant \tau_{3} \vee \neg \tau_{2}}{\tau_{1} \leqslant \tau_{3}}}
\end{aligned}
$$

Case . Symmetric.

Lemma B.21. For all $\Sigma$, we have $\Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \leqslant^{\diamond} \tau_{3} \Longleftrightarrow \Sigma \vdash \tau_{1} \leqslant^{\diamond} \tau_{3} \wedge \Sigma \vdash \tau_{2} \leqslant^{\diamond} \tau_{3}$.
Proof.
Case ⋅ , ⇒ Given (1) $\boldsymbol{\Sigma} \vdash \tau_{1} \vee \tau_{2} \leqslant \tau_{3}$, derive (2) $\boldsymbol{\Sigma} \vdash \tau_{1} \leqslant \tau_{3}$ and (3) $\boldsymbol{\Sigma} \vdash \tau_{2} \leqslant \tau_{3}$ :
$$
\text { S-Trans } \frac{\text { S-ANDOR11• } \overline{\tau_{1} \leqslant \tau_{1} \vee \tau_{2}} \quad \text { (1) } \cdot \tau_{1} \vee \tau_{2} \leqslant \tau_{3}}{\text { (2) } \cdot \tau_{1} \leqslant \tau_{3}}
$$

Similar derivation for concluding (3)•.
Case •, ⇐. Given (2)• and (3)•, derive (1)•:
$$
\begin{array}{ll}
\text { Lemma B.22. } \frac{(\mathbf{2}) \cdot \tau_{1} \leqslant \tau_{3} \quad \mathbf{( 3 )} \cdot \tau_{2} \leqslant \tau_{3}}{\tau_{1} \vee \tau_{2} \leqslant \tau_{3} \vee \tau_{3}} \quad \text { S-AndOR2. } \frac{\text { S-RefL } \overline{\tau_{3} \leqslant \tau_{3}} \quad \text { S-RefL } \overline{\tau_{3} \leqslant \tau_{3}}}{\tau_{3} \vee \tau_{3} \leqslant \tau_{3}} \\
\text { S-Trans } & \mathbf{( 1 ) \cdot \tau _ { 1 } \vee \tau _ { 2 } \leqslant \tau _ { 3 }}
\end{array}
$$

Case >, ⇒. Given (1) $\supset \Sigma \vdash \tau_{3} \leqslant \tau_{1} \wedge \tau_{2}$, derive (2) $\supset \Sigma \vdash \tau_{3} \leqslant \tau_{1}$ and (3) $\supset \Sigma \vdash \tau_{3} \leqslant \tau_{2}$ :
S-Trans $\frac{\text { (1) } \text { ↓ } \tau_{3} \leqslant \tau_{1} \wedge \tau_{2} \text { S-AndOr11 } \overline{\tau_{1} \wedge \tau_{2} \leqslant \tau_{1}}}{\text { (2) } \nu \tau_{3} \leqslant \tau_{1}}$

Similar derivation for concluding (3).
Case ১, ⇐. Given (2). and (3) >, derive (1) >:

S-AndOr2D $\frac{\text { S-Refl } \overline{\tau_{3} \leqslant \tau_{3}} \quad \text { S-Refl } \overline{\tau_{3} \leqslant \tau_{3}}}{\tau_{3} \leqslant \tau_{3} \wedge \tau_{3}} \quad$ Lemma B.22. $\frac{(\mathbf{2}) \downarrow \tau_{3} \leqslant \tau_{1} \quad(\mathbf{3}) \downarrow \tau_{3} \leqslant \tau_{2}}{\tau_{3} \vee \tau_{3} \leqslant \tau_{1} \vee \tau_{2}}$
(1) $\supset \tau_{3} \leqslant \tau_{1} \vee \tau_{2}$

Lemma B.22.
$$
\frac{\Sigma \vdash \tau_{1} \leqslant \tau_{2} \quad \Sigma \vdash \tau_{3} \leqslant \tau_{4}}{\Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \leqslant \tau_{3} \vee^{\diamond} \tau_{4}}
$$

Proof.
$$
\text { S-Trans } \frac{\tau_{1} \leqslant \tau_{2} \text { S-AndOr11 } \diamond \overline{\tau_{2} \leqslant \tau_{2} \vee^{\diamond} \tau_{4}}}{\tau_{1} \leqslant \tau_{2} \vee^{\diamond} \tau_{4}} \text { S-Trans } \frac{\tau_{3} \leqslant \tau_{4}}{\tau_{1} \vee^{\diamond} \tau_{2} \leqslant \tau_{3} \vee^{\diamond} \tau_{4}}
$$

Lemma B.23.
$$
\Sigma \vdash\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \vee^{\diamond} \tau_{3} \leqslant\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \vee^{\diamond} \tau_{2}
$$

Proof.
$$
\text { LEMMA B. } 22 \diamond \frac{\text { S-REFL } \frac{1}{\tau_{1} \leqslant \tau_{1}} \quad \text { S-COMMUT } \diamond \overline{\tau_{2} \vee^{\diamond} \tau_{3} \leqslant \tau_{3} \vee^{\diamond} \tau_{2}}}{\left(\mathbf{1} \tau_{1} \vee^{\diamond}\left(\tau_{2} \vee^{\diamond} \tau_{3}\right) \leqslant \tau_{1} \vee^{\diamond}\left(\tau_{3} \vee^{\diamond} \tau_{2}\right)\right.}
$$

S-Assoc $\diamond \frac{}{\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \vee^{\diamond} \tau_{3} \leqslant \tau_{1} \vee^{\diamond}\left(\tau_{2} \vee^{\diamond} \tau_{3}\right)}$ S-Trans $\frac{(\mathbf{1})}{\text { S-Trans }} \frac{\tau_{1} \vee^{\diamond}\left(\tau_{2} \vee^{\diamond} \tau_{3}\right) \leqslant\left(\tau_{1} v^{\diamond} \tau_{3}\right) v^{\diamond} \tau_{2}}{\tau_{1} v^{\diamond}\left(\tau_{3} v^{\diamond} \tau_{2}\right) \leqslant\left(\tau_{1} v^{\diamond} \tau_{3} v^{\diamond} \tau_{2}\right.}$
$\left(\tau_{1} v^{\diamond} \tau_{2}\right) v^{\diamond} \tau_{3} \leqslant\left(\tau_{1} v^{\diamond} \tau_{3}\right) v^{\diamond} \tau_{2}$ \(\square\)

Lemma B.24.
$$
\frac{\Sigma \vdash \mathrm{T}^{\diamond} \wedge \delta \leqslant \pi}{\Sigma \vdash \tau \leqslant \pi}
$$

Proof.
$$
\text { S-Trans } \frac{\text { S-AndOr2} \bar{\triangleright} \frac{\text { S-ToB } \diamond \overline{\tau \leqslant T^{\diamond}} \operatorname{S-ReFL} \overline{\tau \leqslant \tau}}{\tau \leqslant T^{\diamond} \wedge^{\diamond} \tau}}{\tau \leqslant \pi} \quad T^{\diamond} \wedge^{\diamond} \tau \leqslant \pi
$$ \(\square\)

\section*{B. 4 Some Useful Subtyping Entailment Relationships}

Lemma B. 25 (Reflexivity and weakening). $\Sigma \cdot \Sigma^{\prime} \models(\triangleright) \Sigma$ for all $\Sigma$ and $\Sigma^{\prime}$.
Proof. By repeated applications of S-Cons or S-Cons↓ on S-Hyp.
Lemma B. 26 (Transitivity). If $\Sigma \models \Sigma^{\prime}$ and $\Sigma^{\prime} \models \Sigma^{\prime \prime}$, then $\Sigma \models \Sigma^{\prime \prime}$.
Proof. By straightforward induction on subtyping entailment derivations, making use of Lemma B. 30 for cases S-Cons and S-Consv.

Lemma B. 27 (Merging). If $\Sigma_{1} \models \Sigma_{1}^{\prime}$ and $\Sigma_{2} \models \Sigma_{2}^{\prime}$, then $\Sigma_{1} \cdot \Sigma_{2} \models \Sigma_{1}^{\prime} \cdot \Sigma_{2}^{\prime}$.
Proof. By straightforward induction on subtyping entailment derivations for $\Sigma_{2} \models \Sigma_{2}^{\prime}$, making use of Lemma B. 25 and Lemma B. 26 for case S-Empty, and Lemma B. 30 for cases S-Cons and S-Consp.

Lemma B. 28 (Guarding). If $\Sigma \models \Sigma^{\prime}$, then $\triangleright \Sigma \models \triangleright \Sigma^{\prime}$.
Proof. By straight forward induction on subtyping entailment judgements.
Lemma B. 29 (Unguarding). If $\Sigma \models \Sigma^{\prime}$, then $\triangleleft \Sigma \models \triangleleft \Sigma^{\prime}$.
Proof. By straight forward induction on subtyping entailment judgements.
Lemma B. 30 (Weakening of subtyping contexts in subtyping judgements). If $\Sigma \vdash \tau \leqslant \pi$ and $\Sigma^{\prime} \models \Sigma$, then $\Sigma^{\prime} \models \tau \leqslant \pi$.

Proof. By induction on unassuming subtyping derivations. The only non-trivial cases are S-Hyp, S-FunDepth, and S-RcdDepth.
Case S-Hyp. Then the premise of the rule is $(\tau \leqslant \pi) \in \Sigma$. By straightforward induction on subtyping entailment judgements, $\Sigma^{\prime} \models \Sigma$ and $(\tau \leqslant \pi) \in \Sigma$ implies $\Sigma^{\prime} \vdash \tau \leqslant \pi$.
Case S-FunDepth. Then we have $\tau=\tau_{1} \rightarrow \tau_{2}$ for some $\tau_{1}$ and $\tau_{2}$, and $\pi=\pi_{1} \rightarrow \pi_{2}$ for some $\pi_{1}$ and $\pi_{2}$. The premises of the rule are $\triangleleft \Sigma \vdash \pi_{1} \leqslant \tau_{1}$ and $\triangleleft \Sigma \vdash \tau_{2} \leqslant \pi_{2}$. By Lemma B.29, $\Sigma^{\prime} \models \Sigma$ implies $\triangleleft \Sigma^{\prime} \models \triangleleft \Sigma$. Then by IH on the premises, we have $\triangleleft \Sigma^{\prime} \vdash \pi_{1} \leqslant \tau_{1}$ and $\triangleleft \Sigma^{\prime} \vdash \tau_{2} \leqslant \pi_{2}$. Then we have $\Sigma^{\prime} \vdash \tau_{1} \rightarrow \tau_{2} \leqslant \pi_{1} \rightarrow \pi_{2}$ by S-FunDepth.
Case S-RcoDepth. Then we have $\tau=\left\{x: \tau_{1}\right\}$ for some $\tau_{1}$ and $x$, and $\pi=\left\{x: \pi_{1}\right\}$ for some $\pi_{1}$. The premise of the rule is $\triangleleft \Sigma \vdash \tau_{1} \leqslant \pi_{1}$. By Lemma B.29, $\Sigma^{\prime} \models \Sigma$ implies $\triangleleft \Sigma^{\prime} \models \triangleleft \Sigma$. Then by IH on the premise, we have $\triangleleft \Sigma^{\prime} \vdash \tau_{1} \leqslant \pi_{1}$. Then we have $\Sigma^{\prime} \vdash\left\{x: \tau_{1}\right\} \leqslant\left\{x: \pi_{1}\right\}$ by S-RcdDepth.

Corollary B. 31 (Weakening of guarded subtyping contexts in subtyping judgements). If $\triangleright \Sigma \vdash \tau \leqslant \pi$ and $\Sigma^{\prime} \models \Sigma$, then $\triangleright \Sigma^{\prime} \models \tau \leqslant \pi$.

Proof. By Lemma B. 28 and Lemma B.30.
Lemma B. 32 (Weakening of guarded constraining contexts in consistency judgements). If $\Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons. and $\triangleright \Xi_{\triangleright}^{\prime} \models \triangleright \Xi_{\triangleright}$, then $\Sigma \vdash \triangleright \Xi_{\triangleright}^{\prime} \cdot \Xi ; \rho$ cons..

Proof. By induction on consistency derivations.
Base case. For the base case, we have $\Xi=\epsilon$. Then by the base case of the definition of consistency, we have:
$$
\begin{equation*}
\Sigma \vdash \triangleright \Xi_{\triangleright}^{\prime} ; \rho \text { cons. } \tag{1}
\end{equation*}
$$

Inductive case. For the inductive case, we have $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$, where $\operatorname{dom}\left(\rho_{1}\right)= \{\alpha\}$ for some $\alpha$. The premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\alpha}  \tag{2}\\
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} ; \rho_{2} \text { cons. } \tag{3}
\end{gather*}
$$
where $\operatorname{split}_{\alpha}\left(\Xi, \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\alpha}, \Xi_{\mathscr{X}}\right)$. From the assumption, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright}^{\prime} \models \triangleright \Xi_{\triangleright} \tag{4}
\end{equation*}
$$

By Lemma B. 30 with (4), (2) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright}^{\prime} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\alpha} \tag{5}
\end{equation*}
$$

By IH on (3), we have:
$$
\begin{equation*}
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright}^{\prime} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\not \alpha} ; \rho_{2} \text { cons. } \tag{6}
\end{equation*}
$$

Then by the inductive case of the definition of consistency, (5) and (6) imply:
$$
\begin{equation*}
\Sigma \vdash \triangleright \Xi_{\triangleright}^{\prime} \cdot \Xi ; \rho \text { cons. } \tag{7}
\end{equation*}
$$

Lemma B. 33 (Weakening of subtyping contexts in consistency judgements). If $\Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons. and $\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma^{\prime} \models \Sigma$, then $\Sigma^{\prime} \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons..

Proof. By induction on consistency derivations.
Base case. For the base case, we have $\Xi=\epsilon$. Then by the base case of the definition of consistency, we have:
$$
\begin{equation*}
\Sigma^{\prime} \vdash \triangleright \Xi_{\triangleright} ; \rho \text { cons. } \tag{1}
\end{equation*}
$$

Inductive case. For the inductive case, we have $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$, where $\operatorname{dom}\left(\rho_{1}\right)= \{\alpha\}$ for some $\alpha$. The premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\alpha}  \tag{2}\\
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} ; \rho_{2} \text { cons. } \tag{3}
\end{gather*}
$$
where $\operatorname{split}_{\alpha}\left(\Xi \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\alpha}, \Xi_{\mathscr{\alpha}}\right)$. From the assumption, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma^{\prime} \models \Sigma \tag{4}
\end{equation*}
$$

By Lemma B.45, (4) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\not \alpha} \cdot \rho_{1} \Sigma^{\prime} \vDash \rho_{1} \Sigma \tag{5}
\end{equation*}
$$

By Lemma B. 30 with (5), (2) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\mathscr{\alpha}} \cdot \rho_{1} \Sigma^{\prime} \models \rho_{1} \Xi_{\alpha} \tag{6}
\end{equation*}
$$

By IH on (3) and (5), we have:
$$
\begin{equation*}
\rho_{1} \Sigma^{\prime} \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\not x} ; \rho_{2} \text { cons. } \tag{7}
\end{equation*}
$$

Then by the inductive case of the definition of consistency, (6) and (7) imply:
$$
\begin{equation*}
\Sigma^{\prime} \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho \text { cons. } \tag{8}
\end{equation*}
$$

Lemma B. 34 (Weakening of constraining contexts in typing judgements). If $\Xi, \Gamma \vdash t: \tau$ and $\Xi^{\prime} \models \Xi$, then $\Xi^{\prime}, \Gamma \vdash t: \tau$.

Proof. By straightforward induction on typing derivations. The only non-trivial vases are T-Subs and T-Var2.

Case T-Subs. By IH on the first premise, Lemma B. 30 on the second premise, followed by T-Subs.
Case T-Var2. $\quad \Gamma(x)=\forall \Xi^{\prime \prime} \cdot \tau^{\prime}$
We first notice that the subtyping entailment judgement is transitive by straightforward induction on subtyping entailment judgements, applying Lemma B. 30 to the second premise of S-Cons. The first premise of S-All is $\Xi \models \rho\left(\Xi^{\prime \prime}\right)$, which implies $\Xi^{\prime} \models \rho\left(\Xi^{\prime \prime}\right)$ by transitivity with the assumption $\Xi^{\prime} \models \Xi$. The result then follows from Lemma B. 30 on the second premise S-All, followed by S-All and T-Var2.

\section*{B. 5 Some Useful Lemmas on Substitutions}

Lemma B. 35 (Preservation of typing under substitution). If $\Xi, \Gamma \vdash t: \tau$ and $\mathcal{D}$ wf, then $\rho(\Xi), \rho(\Gamma) \vdash \rho(t): \rho(\tau)$.

Proof. By induction on typing derivations of $\Xi, \Gamma \vdash t: \tau$.
Case T-Subs. By IH on the first premise, we have $\rho(\Xi), \rho(\Gamma) \vdash \rho(t): \rho\left(\tau_{1}\right)$. By preservation of subtyping under substitution (Lemma B.36) on the second premise, $\rho(\Xi) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. The result then follows from T-Subs.
Case T-Obj. By the definition of type substitution, $\rho(\# C \wedge\{\overline{x: \tau}\})=\# C \wedge\{\overline{x: \rho(\tau)}\}$. By the definition of term substitution, $\rho(C\{\overline{x=t}\})=C\{\overline{x=\rho(t)}\}$. By IH on the premises, we have $\overline{\rho(\Xi), \rho(\Gamma) \vdash \rho(t): \rho(\tau)}$. Then $\rho(\Xi), \rho(\Gamma) \vdash C\{\overline{x=\rho(t)}\}: \# C \wedge\{\overline{x: \rho(\tau)}\}$ by T-ObJ, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(C\{\overline{x=t}\}): \# C \wedge \rho(\{\overline{x: \tau}\})$.
Case T-Proj. By the definition of term substitution, $\rho(t . x)=\rho(t) . x$ By IH on the premise, we have $\rho(\Xi), \rho(\Gamma) \vdash t: \rho(\{x: \tau\})$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(t):\{x: \rho(\tau)\}$ by the definition of type substitution. Then $\rho(\Xi), \rho(\Gamma) \vdash \rho(t) . x: \rho(\tau)$ by T-Proj, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(t . x): \rho(\tau)$.
Case T-Var1. Then $t=x$. By the definition of term substitution, $\rho(x)=x$. From the premise and the definition of typing context substitution, we have $\rho(\Gamma)(x)=\rho(\tau)$. Then $\rho(\Xi), \rho(\Gamma) \vdash x: \rho(\tau)$ by T-Var1, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(x): \rho(\tau)$.
Case T-Var2. Then $t=x$. By the definition of term substitution, $\rho(x)=x$. From the premise, we have $\Xi \vdash \Gamma(x) \leqslant^{\forall} \forall \epsilon . \tau$, where $\Gamma(x)=\forall \Xi^{\prime} . \tau^{\prime}$. Note that the judgement $\leqslant^{\forall}$ can only be derived by S-All, then from the premises of S-All, we have $\Xi \vDash \rho^{\prime}\left(\Xi^{\prime}\right)$ and $\Xi \vdash \rho^{\prime}\left(\tau^{\prime}\right) \leqslant \tau$. By preservation of subtyping under substitution (Lemma B.36), we have $\rho(\Xi) \vDash \rho\left(\rho^{\prime}\left(\Xi^{\prime}\right)\right)$ and $\rho(\Xi) \vdash \rho\left(\rho^{\prime}\left(\tau^{\prime}\right)\right) \leqslant \rho(\tau)$. Then $\rho(\Xi) \vdash \forall \Xi^{\prime} . \tau^{\prime} \leqslant \forall \forall \epsilon . \rho(\tau)$ by S-All. Note that by the definition of typing context substitution, $\Gamma(x)=\forall \Xi^{\prime} . \tau^{\prime}$ implies $\rho(\Gamma)(x)=\forall \Xi^{\prime} . \tau^{\prime}$, then $\rho(\Xi), \rho(\Gamma) \vdash x: \rho(\tau)$ by T-Var, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(x): \rho(\tau)$.
Case T-Abs. By the definition of type substitution, $\rho\left(\tau_{1} \rightarrow \tau_{2}\right)=\rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right)$. By IH on the premise, we have $\rho(\Xi), \rho\left(\Gamma \cdot\left(x: \tau_{1}\right)\right) \vdash t: \rho\left(\tau_{2}\right)$, i.e., $\rho(\Xi), \rho(\Gamma) \cdot\left(x: \rho\left(\tau_{1}\right)\right) \vdash t: \rho\left(\tau_{2}\right)$ by the definition of typing context substitution. Then $\rho(\Xi), \rho(\Gamma) \vdash \lambda x . t: \rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right)$ by T-Abs, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \lambda x . t: \rho\left(\tau_{1} \rightarrow \tau_{2}\right)$.
Case T-App. By IH on the premise, we have $\rho(\Xi), \rho(\Gamma) \vdash t_{1}: \rho\left(\tau_{1}\right)$ and $\rho(\Xi), \rho(\Gamma) \vdash t_{0}$ : $\rho\left(\tau_{1} \rightarrow \tau_{2}\right)$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash t_{0}: \rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right)$ by the definition of type substitution. Then $\rho(\Xi), \rho(\Gamma) \vdash t_{0} t_{1}: \rho\left(\tau_{2}\right)$ by T-App.
Case T-Asc. By the definition of term substitution, $\rho(t: \tau)=\rho(t): \rho(\tau)$. By IH on the premise, we have $\rho(\Xi), \rho(\Gamma) \vdash \rho(t): \rho(\tau)$. Then $\rho(\Xi), \rho(\Gamma) \vdash(\rho(t): \rho(\tau)): \rho(\tau)$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho(t: \tau): \rho(\tau)$.

Case T-Case1. By the definition of type substitution, $\rho(\perp)=\perp$. By the definition of term substitution, $\rho$ (case $x=t_{1}$ of $\epsilon$ ) = (case $x=\rho\left(t_{1}\right)$ of $\epsilon$ ). By IH on the premise, we have $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(t_{1}\right): \rho(\perp)$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(t_{1}\right): \perp$. Then $\rho(\Xi), \rho(\Gamma) \vdash$ case $x=\rho\left(t_{1}\right)$ of $\epsilon$ : $\perp$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(\right.$ case $x=t_{1}$ of $\left.\epsilon\right): \rho(\perp)$.
Case T-Case2. By the definition of term substitution, $\rho$ (case $x=t_{1}$ of → $t_{2}$ ) = (case $x=\rho\left(t_{1}\right)$ of ${ }_{-} \rightarrow \rho\left(t_{2}\right)$ ). By IH on the premises, we have $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(t_{1}\right): \rho\left(\tau_{1}\right)$ and $\rho(\Xi), \rho\left(\Gamma \cdot\left(x: \tau_{1}\right)\right) \vdash \rho\left(t_{2}\right): \rho(\tau)$, i.e., $\rho(\Xi), \rho(\Gamma) \cdot\left(x: \rho\left(\tau_{1}\right)\right) \vdash \rho\left(t_{2}\right): \rho(\tau)$ by the definition of typing context substitution. Then $\rho(\Xi), \rho(\Gamma) \vdash$ case $x=\rho\left(t_{1}\right)$ of $_{-} \longrightarrow \rho\left(t_{2}\right): \rho(\tau)$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(\right.$ case $x=t_{1}$ of $\left._{-} \longrightarrow t_{2}\right): \rho(\tau)$.
Case T-Case3. By the definition of term substitution, $\rho$ (case $x=t_{1}$ of $\left.C \rightarrow t_{2}, M\right)=$ (case $x=\rho\left(t_{1}\right)$ of $C \rightarrow \rho\left(t_{2}\right), \rho(M)$ ). By IH on the first premise, we have $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(t_{1}\right): \rho\left(\# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}\right)$, i.e., $\rho(\Xi), \rho(\Gamma) \vdash \rho\left(t_{1}\right): \# C \wedge \rho\left(\tau_{1}\right) \vee \neg \# C \wedge \rho\left(\tau_{2}\right)$ by the definition of type substitution. By IH on the second premise, we have $\rho(\Xi), \rho\left(\Gamma \cdot\left(x: \tau_{1}\right)\right) \vdash \rho\left(t_{2}\right): \rho(\tau)$, i.e., $\rho(\Xi), \rho(\Gamma) \cdot\left(x: \rho\left(\tau_{1}\right)\right) \vdash \rho\left(t_{2}\right): \rho(\tau)$. By IH on the third premise, we have $\rho(\Xi), \rho\left(\Gamma \cdot\left(x: \tau_{2}\right)\right) \vdash \quad \rho($ case $x=x$ of $M) \quad: \quad \rho(\tau)$, i.e., $\rho(\Xi), \rho(\Gamma) \cdot\left(x: \rho\left(\tau_{2}\right)\right) \quad \vdash$ case $x=x$ of $\rho(M): \rho(\tau)$ by the definition of term substitution. Then $\rho(\Xi), \rho(\Gamma) \vdash$ case $x=\rho\left(t_{1}\right)$ of $C \rightarrow \rho\left(t_{2}\right), \rho(M) \quad: \quad \rho(\tau)$ by T-Case3, i.e., $\rho(\Xi), \rho(\Gamma) \quad \vdash \rho\left(\right.$ case $x=t_{1}$ of $\left.C \rightarrow t_{2}, M\right): \rho(\tau)$.

Lemma B. 36 (Preservation of subtyping under substitution). If $\Sigma \vdash \tau_{1} \leqslant \tau_{2}$ and $\mathcal{D} \boldsymbol{w} \boldsymbol{f}$, then $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$.

Proof. By induction on subtyping derivations of $\Sigma \vdash \tau_{1} \leqslant \tau_{2}$.
Case S-Refl. The result $\rho(\tau) \leqslant \rho(\tau)$ follows immediately from S-Refl.
Case S-ToB ◇. By the definition of type substitution, $\rho\left(T^{\diamond}\right)=T^{\diamond}$. By S-ToB ◇, $\rho(\tau) \leqslant T^{\diamond}$, i.e., $\rho(\tau) \leqslant \rho\left(T^{\diamond}\right)$.
Case S-Compl . By the definition of type substitution, $\rho\left(\tau \vee^{\diamond} \neg \tau\right)=\rho(\tau) \vee^{\diamond} \rho(\neg \tau)=\rho(\tau) \vee^{\diamond} \neg \rho(\tau)$ and $\rho\left(T^{\diamond}\right)=T^{\diamond}$. By S-Compl $\diamond$, $\rho(\tau) \vee^{\diamond} \neg \rho(\tau) \geqslant^{\diamond} T^{\diamond}$, i.e., $\rho\left(\tau \vee^{\diamond} \neg \tau\right) \geqslant^{\diamond} \rho\left(T^{\diamond}\right)$.
Case S-NegInv. By the definition of type substitution, $\rho(\neg \tau)=\neg \rho(\tau)$. By IH on the premise, we have $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\rho(\Sigma) \vdash \neg \rho\left(\tau_{2}\right) \leqslant \neg \rho\left(\tau_{1}\right)$ by S-NegInv, i.e., $\rho(\Sigma) \vdash \rho\left(\neg \tau_{2}\right) \leqslant \rho\left(\neg \tau_{1}\right)$.
Case S-AndOr11 . By the definition of type substitution, $\rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)=\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)$. By IH on the premise, we have $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \geqslant \rho(\tau)$. Then $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right) \geqslant \rho(\tau)$ by S-AndOr11 ◇, i.e., $\rho(\Sigma) \vdash \rho\left(\tau_{1} \vee \tau_{2}\right) \geqslant \rho(\tau)$.
Case S-AndOr12 . Symmetric to the case above.
Case S-AndOr2 ↓ . By the definition of type substitution, $\rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)=\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)$. By IH on the premises, we have $\rho(\Sigma) \vdash \rho(\tau) \geqslant^{\diamond} \rho\left(\tau_{1}\right)$ and $\rho(\Sigma) \vdash \rho(\tau) \geqslant^{\diamond} \rho\left(\tau_{2}\right)$. Then $\rho(\Sigma) \vdash \rho(\tau) \geqslant \rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)$ by S-AndOr2 , i.e., $\rho(\Sigma) \vdash \rho(\tau) \geqslant \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)$.
Case S-Distrib . By the definition of type substitution, $\rho\left(\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)\right)=\rho(\tau) \wedge^{\diamond} \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)= \rho(\tau) \wedge^{\diamond}\left(\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)\right)$ and $\rho\left(\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right)\right)=\rho\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond} \rho\left(\tau \wedge^{\diamond} \tau_{2}\right)=(\rho(\tau) \left.\wedge^{\diamond} \rho\left(\tau_{1}\right)\right) \vee^{\diamond}\left(\rho(\tau) \wedge^{\diamond} \rho\left(\tau_{2}\right)\right)$. By S-Distrib $\diamond$, $\rho(\tau) \wedge^{\diamond}\left(\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)\right) \leqslant\left(\rho(\tau) \wedge^{\diamond} \rho\left(\tau_{1}\right)\right) \vee^{\diamond}\left(\rho(\tau) \wedge^{\diamond} \rho\left(\tau_{2}\right)\right)$, i.e., $\rho\left(\tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)\right) \leqslant \rho\left(\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right)\right)$.
Case S-Trans. By IH on the premises, we have $\rho(\Sigma) \vdash \rho\left(\tau_{0}\right) \leqslant \rho\left(\tau_{1}\right)$ and $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\rho(\Sigma) \vdash \rho\left(\tau_{0}\right) \leqslant \rho\left(\tau_{2}\right)$ by S-Trans.
Case S-Weaken. By IH on the premise, we have $\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$ by S-Weaken.

Case S-Assum. By the definition of subtyping context substitution, $\rho\left(\Xi \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right)\right)=\rho(\Xi)$. $\triangleright\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right)$. By IH on the premise, we have $\mathcal{D} \cdot \rho\left(\Xi \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right)\right) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$, i.e., $\mathcal{D} \cdot \rho(\Xi) \cdot \triangleright\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\mathcal{D} \cdot \rho(\Xi) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$ by S-Assum.
Case S-Hyp. By the definition of subtyping context substitution and the $H \in \Sigma$ judgement, it is straightforward to show that if $\left(\tau \leqslant \tau^{\prime}\right) \in \Sigma$, then $\left(\rho(\tau) \leqslant \rho\left(\tau^{\prime}\right)\right) \in \rho(\Sigma)$ by induction on the size of $\Sigma$. Applying to the premise $\left(\tau_{1} \leqslant \tau_{2}\right) \in \Sigma$, we have $\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right) \in \rho(\Sigma)$. Then $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$ by S-Hyp.
Case S-FunDepth. By the definition of type substitution, $\rho\left(\tau \rightarrow \tau^{\prime}\right)=\rho(\tau) \rightarrow \rho\left(\tau^{\prime}\right)$. By IH on the premises, we have $\triangleleft \rho(\Sigma) \vdash \rho\left(\tau_{0}\right) \leqslant \rho\left(\tau_{1}\right)$ and $\triangleleft \rho(\Sigma) \vdash \rho\left(\tau_{2}\right) \leqslant \rho\left(\tau_{3}\right)$. Then $\triangleleft \rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right) \leqslant \rho\left(\tau_{0}\right) \rightarrow \rho\left(\tau_{3}\right)$ by S-FunDepth, i.e., $\triangleleft \rho(\Sigma) \vdash \rho\left(\tau_{1} \rightarrow \tau_{2}\right) \leqslant \rho\left(\tau_{0} \rightarrow \tau_{3}\right)$.
Case S-FunMrg $\diamond$. By the definition of type substitution, $\rho\left(\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)\right)=\rho\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow \rho\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)=\left(\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{3}\right)\right) \rightarrow\left(\rho\left(\tau_{2}\right) \wedge^{\diamond} \rho\left(\tau_{4}\right)\right)$. and $\rho\left(\tau_{1} \rightarrow \tau_{2} \wedge \tau_{3} \rightarrow \tau_{4}\right)=\rho\left(\tau_{1} \rightarrow\right. \left.\tau_{2}\right) \wedge \rho\left(\tau_{3} \rightarrow \tau_{4}\right)=\rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right) \wedge \rho\left(\tau_{3}\right) \rightarrow \rho\left(\tau_{4}\right)$. By S-FUNMRG $\diamond,\left(\rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{3}\right)\right) \rightarrow \left(\rho\left(\tau_{2}\right) \wedge^{\diamond} \rho\left(\tau_{4}\right)\right) \geqslant^{\diamond} \rho\left(\tau_{1}\right) \rightarrow \rho\left(\tau_{2}\right) \wedge \rho\left(\tau_{3}\right) \rightarrow \rho\left(\tau_{4}\right)$, i.e., $\rho\left(\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)\right) \geqslant^{\diamond} \rho\left(\tau_{1} \rightarrow\right. \left.\tau_{2} \wedge \tau_{3} \rightarrow \tau_{4}\right)$.
Case S-RcoDepth. By the definition of type substitution, $\rho(\{x: \tau\})=\{x: \rho(\tau)\}$. By IH on the premise, we have $\triangleleft \rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\triangleleft \rho(\Sigma) \vdash\left\{x: \rho\left(\tau_{1}\right)\right\} \leqslant\left\{x: \rho\left(\tau_{2}\right)\right\}$ by S-RcoDepth, i.e., $\triangleleft \rho(\Sigma) \vdash \rho\left(\left\{x: \tau_{1}\right\}\right) \leqslant \rho\left(\left\{x: \tau_{2}\right\}\right)$.
Case S-RcoMrg $\diamond$. By the definition of type substitution, $\rho\left(\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\}\right)=\left\{x: \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)\right\}= \left\{x: \rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)\right\}$ and $\rho\left(\left\{x: \tau_{1}\right\} \vee^{\diamond}\left\{x: \tau_{2}\right\}\right)=\rho\left(\left\{x: \tau_{1}\right\}\right) \vee^{\diamond} \rho\left(\left\{x: \tau_{2}\right\}\right)=\{x: \left.\rho\left(\tau_{1}\right)\right\} \vee^{\diamond}\left\{x: \rho\left(\tau_{2}\right)\right\}$. By S-RcdMRG $\diamond,\left\{x: \rho\left(\tau_{1}\right) \vee^{\diamond} \rho\left(\tau_{2}\right)\right\} \leqslant\left\{x: \rho\left(\tau_{1}\right)\right\} \vee^{\diamond}\left\{x: \rho\left(\tau_{2}\right)\right\}$, i.e., $\rho\left(\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\}\right) \leqslant \rho\left(\left\{x: \tau_{1}\right\} \vee^{\diamond}\left\{x: \tau_{2}\right\}\right)$.
Case S-RcoTop. By the definition of type substitution, $\rho(\mathrm{T})=\mathrm{T}$ and $\rho\left(\left\{x: \tau_{1}\right\} \vee \tau\right)=\rho(\{x$ : $\left.\left.\tau_{1}\right\}\right) \vee \rho(\tau)=\left\{x: \rho\left(\tau_{1}\right)\right\} \vee \rho(\tau)$. From the premise, we have $\rho(\tau) \in\left\{\rho\left(\left\{y^{\neq x}: \tau_{2}\right\}\right), \rho\left(\tau_{2} \rightarrow \tau_{3}\right)\right\}$, i.e., $\rho(\tau) \in\left\{\left\{y^{\neq x}: \rho\left(\tau_{2}\right)\right\} \rho\left(\tau_{2}\right) \rightarrow \rho\left(\tau_{3}\right)\right\}$ by the definition of type substitution. Then $\tau \leqslant\left\{x: \rho\left(\tau_{1}\right)\right\} \vee \rho(\tau)$ by S-RcoTop, i.e., $\rho(\tau) \leqslant \rho\left(\left\{x: \tau_{1}\right\} \vee \tau\right)$.
Case S-ClsSub. Note that the declaration context rooted in by the subtyping context contains all the information required to determine the superclass relation, i.e., $\mathcal{S}_{\mathcal{D} \cdot \Sigma}=\mathcal{S}_{\mathcal{D} \cdot \Sigma^{\prime}}$. Then the premise $C_{2} \in \mathcal{S}\left(C_{1}[\bar{\alpha}]\right)$ implies $C_{2} \in \mathcal{S}\left(C_{1}[\bar{\alpha}]\right)$. By the definition of type substitution, $\rho(\# C)=\# C$. Then $\rho(\Sigma) \vdash \# C_{1} \leqslant \# C_{2}$ by S-ClsSub, i.e., $\rho(\Sigma) \vdash \rho\left(\# C_{1}\right) \leqslant \rho\left(\# C_{2}\right)$.
Case S-ClsBot. As noted in the case above, $\mathcal{S}_{\mathcal{D} \cdot \Sigma}=\mathcal{S}_{\mathcal{D} \cdot \Sigma^{\prime}}$. By the definition of type substitution, $\rho\left(\# C_{1} \wedge \# C_{2}\right)=\rho\left(\# C_{1}\right) \wedge \rho\left(\# C_{2}\right)=\# C_{1} \wedge \# C_{2}$ and $\rho(\perp)=\perp$. Then the premise $C_{1} \notin \mathcal{S}\left(C_{2}[\bar{\alpha}]\right)$ and $C_{2} \notin \mathcal{S}\left(C_{1}[\bar{\beta}]\right)$ imply $C_{1} \notin \mathcal{S}\left(C_{2}[\bar{\alpha}]\right)$ and $C_{2} \notin \mathcal{S}\left(C_{1}[\bar{\beta}]\right)$. Then $\rho(\Sigma) \vdash \# C_{1} \wedge \# C_{2} \leqslant \perp$ by S-ClsBot, i.e., $\rho(\Sigma) \vdash \rho\left(\# C_{1} \wedge \# C_{2}\right) \leqslant \rho(\perp)$.
Case S-Exp . We show that if $\Sigma \vdash \tau \exp . \tau^{\prime}$, where $\mathcal{D} \boldsymbol{w}$ f, then $\rho(\Sigma) \vdash \rho(\tau) \exp . \rho\left(\tau^{\prime}\right)$. We consider rules that can derive the judgement $\Sigma \vdash \tau \exp . \tau^{\prime}$.
Case S-AlsExp. Note that the declaration context contains all declarations, i.e., $d \in \Sigma$ implies $d \in \mathcal{D} \cdot \Sigma^{\prime}$. Then the premise implies (type $\left.A\left[{\overline{\alpha_{i}}}^{i \in S}\right]=\tau\right) \in \rho(\Sigma)$. By the definition of type substitution, $\rho\left(A\left[\bar{\tau}_{i}{ }^{i \in S}\right]\right)=A\left[{\overline{\rho\left(\tau_{i}\right)}}^{i \in S}\right]$. By the well-formedness of $\mathcal{D}$, $T V(\tau) \subseteq\left\{{\overline{\alpha_{i}}}^{i \in S}\right\}$, which implies that all type variables in $\left[{\overline{\alpha_{i} \mapsto \tau_{i}}}^{i \in S}\right] \tau$ are introduced by the substitution $\left\{{\overline{\alpha_{i} \mapsto \tau_{i}}}^{i \in S}\right\}$, and $\left.\left.\rho\left({\overline{\alpha_{i} \mapsto \tau_{i}}}^{i \in S}\right] \tau\right)={\overline{\alpha_{i} \mapsto \rho\left(\tau_{i}\right)}}^{i \in S}\right] \tau$. Then $\rho(\Sigma) \vdash A\left[{\overline{\rho\left(\tau_{i}\right)}}^{i \in S}\right] \exp .\left[{\overline{\alpha_{i} \mapsto \rho\left(\tau_{i}\right)}}^{i \in S}\right] \tau$ by S-AlsExp, i.e., $\rho(\Sigma) \vdash \rho\left(A\left[{\overline{\tau_{i}}}^{i \in S}\right]\right) \operatorname{exp.} \rho\left(\left[{\overline{\alpha_{i}}}^{i} \tau_{i}{ }^{i \in S}\right] \tau\right)$.
Case S-ClsExp. Similar to the case above, noting that $\rho\left(\# C \wedge\left[{\overline{\alpha_{i}}} \tau_{i} i \in S\right] \tau\right)=\rho(\# C) \wedge \rho\left(\left[{\overline{\alpha_{i} \mapsto \tau_{i}}}^{i \in S}\right] \tau\right)=\# C \wedge \rho\left(\left[{\overline{\alpha_{i} \mapsto \tau_{i}}}^{i \in S}\right] \tau\right)$.
Then the premise $\Sigma \vdash \tau \exp . \tau^{\prime}$ implies $\rho(\Sigma) \vdash \rho(\tau) \exp . \rho\left(\tau^{\prime}\right)$, and $\rho(\Sigma) \vdash \rho(\tau) \geqslant^{\diamond} \rho\left(\tau^{\prime}\right)$ follows from S-Exp॰.

Corollary B. 37 (Preservation of subtyping entailment under substitution). If $\Sigma \models \Sigma^{\prime}$ and $\mathcal{D}$ wf, then $\rho(\Sigma) \vDash \rho\left(\Sigma^{\prime}\right)$.

Proof. By induction on the derivation of subtyping entailment judgement $\Sigma \models \Sigma^{\prime}$.
Case S-Empty. Immediate.
Case S-Cons. By the definition of subtyping context substitution, $\rho\left(\Sigma^{\prime} \cdot\left(\tau_{1} \leqslant \tau_{2}\right)\right)= \rho\left(\Sigma^{\prime}\right) \cdot\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right)$. By IH on the premise $\Sigma \models \Sigma^{\prime}$, we have $\rho(\Sigma) \vDash \rho\left(\Sigma^{\prime}\right)$. By preservation of subtyping under substitution (Lemma B.36) on the premise $\Sigma \vdash \tau_{1} \leqslant \tau_{2}$, we have $\rho(\Sigma) \vdash \rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)$. Then $\rho(\Sigma) \vDash \rho\left(\Sigma^{\prime}\right) \cdot\left(\rho\left(\tau_{1}\right) \leqslant \rho\left(\tau_{2}\right)\right)$ follows from S-Cons, i.e., $\rho(\Sigma) \vDash \rho\left(\Sigma^{\prime} \cdot\left(\tau_{1} \leqslant \tau_{2}\right)\right)$.

Lemma B. 38 (Congruence of substitution on types). If $\Sigma \vdash \pi \equiv \pi^{\prime}$, then $\Sigma \vdash[\alpha \mapsto \pi] \tau \equiv \left[\alpha \mapsto \pi^{\prime}\right] \tau$ for all $\tau$.

Proof. By straightforward induction on the syntax of $\tau$. The only non-trivial cases are:
Case $\tau=\tau_{1} \rightarrow \tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\Sigma \vdash \pi \equiv \pi^{\prime} \tag{1}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (1) implies:
$$
\begin{equation*}
\triangleleft \Sigma \vdash \pi \equiv \pi^{\prime} \tag{2}
\end{equation*}
$$

By IH on (2), we have:
$$
\begin{align*}
& \triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{1} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{1}  \tag{3}\\
& \triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{2} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{2} \tag{4}
\end{align*}
$$

Then by S-FunDepth on (3) and (4), we have:
$$
\begin{equation*}
\Sigma \vdash[\alpha \mapsto \pi]\left(\tau_{1} \rightarrow \tau_{2}\right) \equiv\left[\alpha \mapsto \pi^{\prime}\right]\left(\tau_{1} \rightarrow \tau_{2}\right) \tag{5}
\end{equation*}
$$

Case $\tau=\left\{x: \tau_{1}\right\}$. From the assumption, we have:
$$
\begin{equation*}
\Sigma \vdash \pi \equiv \pi^{\prime} \tag{6}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (6) implies:
$$
\begin{equation*}
\triangleleft \Sigma \vdash \pi \equiv \pi^{\prime} \tag{7}
\end{equation*}
$$

By IH on (7), we have:
$$
\begin{equation*}
\triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{1} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{1} \tag{8}
\end{equation*}
$$

Then by S-RcdDepth on (8) and (4), we have:
$$
\begin{equation*}
\Sigma \vdash[\alpha \mapsto \pi]\left\{x: \tau_{1}\right\} \equiv\left[\alpha \mapsto \pi^{\prime}\right]\left\{x: \tau_{1}\right\} \tag{9}
\end{equation*}
$$

Case $\tau=\alpha$. From the assumption, we have:
$$
\begin{align*}
\Sigma \vdash \pi & \equiv \pi^{\prime} \\
\text { i.e., } \quad \Sigma \vdash[\alpha \mapsto \pi] \alpha & \equiv\left[\alpha \mapsto \pi^{\prime}\right] \alpha \tag{10}
\end{align*}
$$

Lemma B. 39 (Congruence of substitution on guarded types). If $\Sigma \vdash \pi \equiv \pi^{\prime}$ and $\alpha \notin T T V(\tau)$, then $\triangleright \Sigma \vdash[\alpha \mapsto \pi] \tau \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau$.

Proof. By straightforward induction on the syntax of $\tau$. The only non-trivial cases are:
Case $\tau=\tau_{1} \rightarrow \tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\Sigma \vdash \pi \equiv \pi^{\prime} \tag{1}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (1) implies:
$$
\begin{equation*}
\triangleleft \Sigma \vdash \pi \equiv \pi^{\prime} \tag{2}
\end{equation*}
$$

By Lemma B. 38 on (2), we have:
$$
\begin{align*}
& \triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{1} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{1}  \tag{3}\\
& \triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{2} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{2} \tag{4}
\end{align*}
$$

Then by S-FunDepth on (3) and (4), we have:
$$
\begin{equation*}
\triangleright \Sigma \vdash[\alpha \mapsto \pi]\left(\tau_{1} \rightarrow \tau_{2}\right) \equiv\left[\alpha \mapsto \pi^{\prime}\right]\left(\tau_{1} \rightarrow \tau_{2}\right) \tag{5}
\end{equation*}
$$

Case $\tau=\left\{x: \tau_{1}\right\}$. From the assumption, we have:
$$
\begin{equation*}
\Sigma \vdash \pi \equiv \pi^{\prime} \tag{6}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (6) implies:
$$
\begin{equation*}
\triangleleft \Sigma \vdash \pi \equiv \pi^{\prime} \tag{7}
\end{equation*}
$$

By Lemma B. 38 on (7), we have:
$$
\begin{equation*}
\triangleleft \Sigma \vdash[\alpha \mapsto \pi] \tau_{1} \equiv\left[\alpha \mapsto \pi^{\prime}\right] \tau_{1} \tag{8}
\end{equation*}
$$

Then by S-RcdDepth on (8) and (4), we have:
$$
\begin{equation*}
\triangleright \Sigma \vdash[\alpha \mapsto \pi]\left\{x: \tau_{1}\right\} \equiv\left[\alpha \mapsto \pi^{\prime}\right]\left\{x: \tau_{1}\right\} \tag{9}
\end{equation*}
$$

Case $\tau=\alpha$. Impossible since $\alpha \notin \operatorname{TTV}(\tau)$.

Corollary B.40. $\Sigma \vdash \tau \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Sigma}(\alpha) \vee l b_{\Sigma}(\alpha)\right] \tau$ for all $\tau$.
Proof. By Lemma B. 38 on $\Sigma \vdash \alpha \equiv \alpha \wedge u b_{\Sigma}(\alpha) \vee l b_{\Sigma}(\alpha)$.
Corollary B.41. If $\alpha \notin T T V(\tau)$, then $\triangleright \Sigma \vdash \tau \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Sigma}(\alpha) \vee l b_{\Sigma}(\alpha)\right] \tau$.
Proof. By Lemma B. 39 on $\Sigma \vdash \alpha \equiv \alpha \wedge u b_{\Sigma}(\alpha) \vee l b_{\Sigma}(\alpha)$.
Lemma B. 42 (Inlining of bound). If $\Sigma \cdot\left(\alpha \leqslant^{\diamond} \pi\right) \vdash \tau \leqslant \tau^{\prime}$, then $\rho \Sigma \cdot \triangleright\left(\alpha \leqslant^{\diamond} \pi\right) \vdash \rho \tau \leqslant \rho \tau^{\prime}$, where $\rho=\left[\alpha \mapsto \alpha \wedge^{\diamond} \pi\right]$.

Proof. By straightforward induction on unassuming subtyping derivations. The only non-trivial case is S-Hyp when $\left(\tau \leqslant \tau^{\prime}\right)=\left(\alpha \leqslant^{\diamond} \pi\right)$.
Case S-Hyp when $\left(\tau \leqslant \tau^{\prime}\right)=(\alpha \leqslant \rho)$. Let cleanup $((\alpha \leqslant \rho))=\left(\alpha \leqslant \pi^{\prime}\right)$. By Lemma B.8, Lemma B.9, and Lemma B.10, we have:
$$
\begin{gather*}
\left(\alpha \leqslant^{\diamond} \pi\right) \models\left(\alpha \leqslant^{\diamond} \pi^{\prime}\right)  \tag{1}\\
\left(\alpha \leqslant^{\diamond} \pi^{\prime}\right) \text { guard. }  \tag{2}\\
\alpha \wedge^{\diamond} \pi \equiv \alpha \wedge^{\diamond} \pi^{\prime} \tag{3}
\end{gather*}
$$

By S-Trans on $\left(\alpha \leqslant^{\diamond} \pi^{\prime}\right) \vdash \alpha \equiv \alpha \wedge^{\diamond} \pi^{\prime}$ and (3), we have:
$$
\begin{equation*}
\left(\alpha \leqslant \pi^{\prime}\right) \vdash \alpha \equiv \alpha \wedge^{\diamond} \pi \tag{4}
\end{equation*}
$$

By Lemma B. 39 on (2) and (4), we have:
$$
\begin{equation*}
\triangleright\left(\alpha \leqslant \pi^{\prime}\right) \vdash \pi^{\prime} \equiv \rho \pi^{\prime} \tag{5}
\end{equation*}
$$

By Lemma B. 22 on (4) and (5), we have:
$$
\begin{align*}
& \triangleright\left(\alpha \leqslant \pi^{\prime}\right) \vdash \alpha \wedge^{\diamond} \pi^{\prime} \equiv\left(\alpha \wedge^{\diamond} \pi\right) \wedge^{\diamond} \rho \pi^{\prime} \\
\text { i.e., } & \triangleright\left(\alpha \leqslant^{\diamond} \pi^{\prime}\right) \vdash \alpha \wedge^{\diamond} \pi^{\prime} \equiv \rho\left(\alpha \wedge^{\diamond} \pi^{\prime}\right) \tag{6}
\end{align*}
$$

By S-Trans on (3) and S-AndOr12 $\overline{\text {, we }}$, have:
$$
\begin{equation*}
\alpha \wedge^{\diamond} \pi^{\prime} \leqslant \pi \tag{7}
\end{equation*}
$$

By Lemma B.36, (7) implies:
$$
\begin{equation*}
\rho\left(\alpha \wedge^{\diamond} \pi\right) \leqslant \rho \pi \tag{8}
\end{equation*}
$$

Then by S-Trans on (3), (6), and (8), we have:
$$
\begin{equation*}
\triangleright\left(\alpha \leqslant^{\diamond} \pi^{\prime}\right) \vdash \alpha \wedge^{\diamond} \pi \leqslant^{\diamond} \rho \pi \tag{9}
\end{equation*}
$$

Then by Lemma B. 30 with (1), (9) implies:
$$
\begin{align*}
& \quad \triangleright\left(\alpha \leqslant^{\diamond} \pi\right) \vdash \alpha \wedge^{\diamond} \pi \leqslant^{\diamond} \rho \pi \\
& \text { i.e., } \quad \triangleright\left(\alpha \leqslant^{\diamond} \pi\right) \vdash \rho \alpha \leqslant^{\diamond} \rho \pi \tag{10}
\end{align*}
$$

\section*{B. 6 Some Useful Lemmas on Consistency}

Lemma B. 43 (Congruence of substitution on consistency). If $[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot[\alpha \mapsto \tau] \Xi$; $\rho$ cons. and $\triangleright \Xi_{\triangleright} \vdash \tau \equiv \tau^{\prime}$, where $\tau$ and $\tau^{\prime}$ are not type variables, then $\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot\left[\alpha \mapsto \tau^{\prime}\right] \Xi$; $\rho^{\prime}$ cons. for some $\rho^{\prime}$, where $\operatorname{dom}\left(\rho^{\prime}\right)=\operatorname{dom}(\rho)$.

Proof. By induction on consistency derivations for the statement: if $\rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi$; $\rho$ cons. and $\triangleright \Xi_{\triangleright} \vdash \tau \equiv \tau^{\prime}$ and ${\overline{\Xi_{\triangleright}} \vdash \gamma \equiv \tau_{\gamma}}^{\left(\gamma \mapsto \tau_{\gamma}\right) \in \rho^{\prime \prime}}$, where $\tau$ and $\tau^{\prime}$ are not type variables and $\overline{\gamma=\gamma^{\prime}}\left(\gamma \mapsto \gamma^{\prime}\right) \in \rho^{\prime \prime}$ and $\operatorname{dom}(\rho) \cap \operatorname{dom}\left(\rho^{\prime \prime}\right)=\varnothing$, then $\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi ; \rho^{\prime}$ cons. for some $\rho^{\prime}$, where $\operatorname{dom}\left(\rho^{\prime}\right)=\operatorname{dom}(\rho)$.
Base case. For the base case, we have $\Xi=\epsilon$. Then by the base case of the definition of consistency, we have:
$$
\begin{equation*}
\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi ; \text { id cons. } \tag{1}
\end{equation*}
$$

Inductive case on $\alpha$. For the inductive case on $\alpha$, i.e., where $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$, where $\operatorname{dom}\left(\rho_{1}\right)=\{\alpha\}$, the preimses of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha}^{\prime} \cdot \rho_{1} \Xi_{\alpha}^{\prime} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \models \rho_{1} \Xi_{\alpha}^{\prime}  \tag{2}\\
\rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha}^{\prime} \cdot \rho_{1} \Xi_{\alpha}^{\prime} ; \rho_{2} \text { cons. } \tag{3}
\end{gather*}
$$
where $\operatorname{split}_{\alpha}\left(\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi, \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\alpha}^{\prime}, \Xi_{\alpha}^{\prime}\right)$ and $\rho_{1}=\left[\alpha \mapsto \alpha \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\alpha) \vee\right. \left.l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\alpha)\right]$. Since $\tau$ is not a type varialbe, we have:
$$
\begin{gather*}
\Xi_{\alpha}^{\prime}=\epsilon  \tag{4}\\
\Xi_{\not \alpha}^{\prime}=\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi  \tag{5}\\
\rho_{1}=[\alpha \mapsto \alpha] \tag{6}
\end{gather*}
$$

Then (3) implies:
$$
\begin{equation*}
\rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi ; \rho_{2} \text { cons. } \tag{7}
\end{equation*}
$$

Then by IH on (7), we have:
$$
\begin{gather*}
\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi ; \rho_{2} \text { cons. } \\
\text { i.e., } \quad \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi ; \rho_{2} \circ \rho_{1} \text { cons. } \tag{8}
\end{gather*}
$$
for some $\rho_{2}^{\prime}$, where $\operatorname{dom}\left(\rho_{2}^{\prime}\right)=\operatorname{dom}\left(\rho_{2}\right)$.
Inductive case not on $\alpha$. For the inductive case not on $\alpha$, i.e., where $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$ and $\beta \neq \alpha$, where $\operatorname{dom}\left(\rho_{1}\right)=\{\beta\}$, the premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta}^{\prime} \cdot \rho_{1} \Xi_{\not \beta}^{\prime} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \models \rho_{1} \Xi_{\beta}^{\prime}  \tag{9}\\
\rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta}^{\prime} \cdot \rho_{1} \Xi_{\beta}^{\prime} ; \rho_{2} \text { cons. } \tag{10}
\end{gather*}
$$
where $\operatorname{split}_{\beta}\left(\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi, \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\beta}^{\prime}, \Xi_{\beta}^{\prime}\right)$ and $\rho_{1}=\left[\beta \mapsto \beta \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta) \vee\right. \left.l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta)\right]$. Let $\operatorname{split}_{\beta}\left(\Xi \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\beta}, \Xi_{\beta}\right)$. Since $\tau$ is not a type variable and $\overline{\gamma=\gamma^{\prime}}\left(\gamma \mapsto \gamma^{\prime}\right) \in \rho^{\prime \prime}$, we have $\Xi_{\beta}^{\prime}=\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta}$ and $\Xi_{\beta}^{\prime}=\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta}$. Then (9) and (10) imply:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \models \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta}  \tag{11}\\
\rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} ; \rho_{2} \text { cons. } \tag{12}
\end{gather*}
$$

Expanding the composition, we have:
$$
\begin{equation*}
\rho_{1} \circ \rho^{\prime \prime}=\left[\overline{\gamma \mapsto \rho_{1} \tau_{\gamma}}\left(\gamma \mapsto \tau_{\gamma}\right) \in \rho^{\prime \prime}, \beta \mapsto \beta \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta) \vee l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta)\right] \tag{13}
\end{equation*}
$$

From the assumption, we have:
$$
\begin{equation*}
\overline{\triangleright \Xi_{\triangleright} \vdash \gamma \equiv \tau_{\gamma}}\left(\gamma \mapsto \tau_{\gamma}\right) \in \rho^{\prime \prime} \tag{14}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\quad \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta}}(\beta) \vee l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta}}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta) \vee l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \pi \equiv \rho_{1} \pi \quad \text { for all } \pi \tag{15}
\end{gather*}
$$

By S-Trans on (14) and (15), we have:
$$
\begin{equation*}
\overline{\triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \gamma \equiv \rho_{1} \tau_{\gamma}}\left(\gamma \mapsto \tau_{\gamma}\right) \in \rho^{\prime \prime} \tag{16}
\end{equation*}
$$

Taking $\pi=\beta$, (15) implies:
$$
\begin{equation*}
\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \beta \equiv \beta \wedge u b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta) \vee l b_{\rho^{\prime \prime}[\alpha \mapsto \tau] \Xi}(\beta) \tag{17}
\end{equation*}
$$

Then (16) and (17) imply:
$$
\begin{equation*}
\overline{\triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \vdash \gamma \equiv \tau_{\gamma}}\left(\gamma \mapsto \tau_{\gamma}\right) \in \rho_{1} \circ \rho^{\prime \prime} \tag{18}
\end{equation*}
$$

Then by IH on (12) and (18), we have:
$$
\begin{equation*}
\rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta \not} ; \rho_{2}^{\prime} \text { cons. } \tag{19}
\end{equation*}
$$
for some $\rho_{2}^{\prime}$, where $\operatorname{dom}\left(\rho_{2}^{\prime}\right)=\operatorname{dom}\left(\rho_{2}\right)$.
From the assumptions, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \vdash \tau \equiv \tau^{\prime} \tag{20}
\end{equation*}
$$

By Lemma B.38, (20) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \vdash[\alpha \mapsto \tau] \pi \equiv\left[\alpha \mapsto \tau^{\prime}\right] \pi \quad \text { for all } \pi \tag{21}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (21), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \models[\alpha \mapsto \tau] \Xi_{\beta} \tag{22}
\end{equation*}
$$

By Lemma B.36, (22) implies:
$$
\begin{equation*}
\triangleright \rho^{\prime \prime} \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \models \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \tag{23}
\end{equation*}
$$

By Lemma B.38, (14) implies:
$$
\begin{equation*}
\Xi_{\triangleright} \vdash \pi \equiv \rho^{\prime \prime} \pi \quad \text { for all } \pi \tag{24}
\end{equation*}
$$

By S-Trans on Lemma B. 30 and (24), we have
$$
\begin{equation*}
\Xi_{\triangleright} \models \rho^{\prime \prime} \Xi_{\triangleright} \tag{25}
\end{equation*}
$$

Then by Lemma B. 30 with (25), (23) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \models \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \tag{26}
\end{equation*}
$$

Then by Lemma B. 30 with (26), (19) implies:
$$
\begin{equation*}
\rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} ; \rho_{2}^{\prime} \text { cons. } \tag{27}
\end{equation*}
$$

Similarly, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \tag{28}
\end{equation*}
$$

By Lemma B.36, (28) implies:
$$
\begin{equation*}
\triangleright \rho_{1} \Xi_{\triangleright} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \tag{29}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (15), we have:
$$
\begin{equation*}
\Xi_{\triangleright} \cdot \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \models \rho_{1} \Xi_{\triangleright} \tag{30}
\end{equation*}
$$

Then by Lemma B. 30 with (30), (29) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Sigma \tag{31}
\end{equation*}
$$

Then by Lemma B. 30 with (31), (11) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \tag{32}
\end{equation*}
$$

Similarly, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \models \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{33}
\end{equation*}
$$

Then by Lemma B. 26 on (32) and (33), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}[\alpha \mapsto \tau] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{34}
\end{equation*}
$$

Then by Lemma B. 30 with (26), (34) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{35}
\end{equation*}
$$

Let $\rho_{1}^{\prime}=\left[\beta \mapsto \beta \wedge u b_{\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi}(\beta) \vee l b_{\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi}(\beta)\right]$. Since $\tau$ and $\tau^{\prime}$ are not type variables and $\overline{\gamma=\gamma^{\prime}}\left(\gamma \mapsto \gamma^{\prime}\right) \in \rho^{\prime \prime}$, we have:
$$
\begin{gather*}
\rho_{1}=\left[\beta \mapsto \beta \wedge \rho^{\prime \prime}[\alpha \mapsto \tau] u b_{\Xi}(\beta) \vee \rho^{\prime \prime}[\alpha \mapsto \tau] l b_{\Xi}(\beta)\right]  \tag{36}\\
\rho_{1}^{\prime}=\left[\beta \mapsto \beta \wedge \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] u b_{\Xi}(\beta) \vee \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] l b_{\Xi}(\beta)\right] \tag{37}
\end{gather*}
$$

By Lemma B.36, (21) implies:
$$
\begin{gather*}
\triangleright \rho^{\prime \prime} \Xi_{\triangleright} \vdash \rho^{\prime \prime}[\alpha \mapsto \tau] u b_{\Xi}(\beta) \equiv \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] u b_{\Xi}(\beta)  \tag{38}\\
\triangleright \rho^{\prime \prime} \Xi_{\triangleright} \vdash \rho^{\prime \prime}[\alpha \mapsto \tau] l b_{\Xi}(\beta) \equiv \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] l b_{\Xi}(\beta) \tag{39}
\end{gather*}
$$

By Lemma B. 30 with (25), (38) and (39) imply:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \vdash \rho^{\prime \prime}[\alpha \mapsto \tau] u b_{\Xi}(\beta) \equiv \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] u b_{\Xi}(\beta)  \tag{40}\\
\triangleright \Xi_{\triangleright} \vdash \rho^{\prime \prime}[\alpha \mapsto \tau] l b_{\Xi}(\beta) \equiv \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] l b_{\Xi}(\beta) \tag{41}
\end{gather*}
$$

Then by Lemma B. 22 on S-Refl, (40), and (41), we have:
$$
\begin{align*}
\triangleright \Xi_{\triangleright} \vdash \beta \wedge \rho^{\prime \prime}[\alpha \mapsto \tau] u b_{\Xi}(\beta) \vee \rho^{\prime \prime}[\alpha \mapsto \tau] l b_{\Xi}(\beta) & \\
& \equiv \beta \wedge \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] u b_{\Xi}(\beta) \vee \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] l b_{\Xi}(\beta) \tag{42}
\end{align*}
$$

Then by IH on (27) and (42), we have:
$$
\begin{equation*}
\rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta \not} ; \rho_{2}^{\prime \prime} \text { cons. } \tag{43}
\end{equation*}
$$
for some $\rho_{2}^{\prime \prime}$, where $\operatorname{dom}\left(\rho_{2}^{\prime \prime}\right)=\operatorname{dom}\left(\rho_{2}^{\prime}\right)$.
By Lemma B.38, (42) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \vdash \rho_{1} \pi \equiv \rho_{1}^{\prime} \pi \quad \text { for all } \pi \tag{44}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (44), we have:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta p} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma  \tag{45}\\
\triangleright \Xi_{\triangleright} \cdot \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \vDash \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{46}
\end{gather*}
$$

Then by Lemma B. 30 with (45), (35) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{47}
\end{equation*}
$$

Then by Lemma B. 26 on (47) and (46), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \models \rho_{1}^{\prime} \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta} \tag{48}
\end{equation*}
$$

Since $\tau^{\prime}$ is not a type variable and $\overline{\gamma=\gamma^{\prime}}\left(\gamma \mapsto \gamma^{\prime}\right) \in \rho^{\prime \prime}$, we have split ${ }_{\beta}\left(\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi \operatorname{dom}\left(\rho_{2}^{\prime \prime}\right)\right)= \left(\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta}, \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi_{\beta}\right)$. Then by the inductive case of the definition of consistency, (43) and (48) imply:
$$
\begin{equation*}
\rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \rho^{\prime \prime}\left[\alpha \mapsto \tau^{\prime}\right] \Xi ; \rho_{2}^{\prime \prime} \circ \rho_{1}^{\prime} \text { cons. } \tag{49}
\end{equation*}
$$

Lemma B. 44 (Inversion of consistency). If $\Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons., then for all $\alpha$, we have $\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} \cdot \rho_{\alpha} \Sigma \models \rho_{\alpha} \Xi_{\alpha}$ and $\rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} ; \rho^{\prime}$ cons. for some $\rho^{\prime}$, where split ${ }_{\alpha}\left(\Xi, \operatorname{dom}\left(\rho^{\prime}\right)\right)= \left(\Xi_{\alpha}, \Xi_{\mathscr{X}}\right), \rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right]$, and $\operatorname{dom}\left(\rho^{\prime}\right)=\operatorname{dom}(\rho) \backslash\{\alpha\}$.

Proof. By induction on consistency derivations. If $\Xi$ is not guarded, we can replace it with $\operatorname{cleanup}(\Xi)$ before applying the lemma, and restore it back to $\Xi$ in the conclusion. Therefore we can assume $\Xi$ guard..
Base case. For the base case, we have $\Xi=\epsilon$. Then we have $\Xi_{\alpha}=\epsilon, \Xi_{\alpha}=\epsilon$, and $\rho_{\alpha}=i d$. By S-Empty, we have:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \sum \models \epsilon \\
\text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} \cdot \rho_{\alpha} \Sigma \models \rho_{\alpha} \Xi_{\alpha} \tag{1}
\end{gather*}
$$

By the base case of the definition of consistency, we have:
$$
\begin{gather*}
\Sigma \vdash \triangleright \Xi_{\triangleright} ; \text { id cons. } \\
\text { i.e., } \quad \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} ; \text { id cons. } \tag{2}
\end{gather*}
$$

Inductive case on $\alpha$. For the inductive case on $\alpha$, i.e., where $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$, where $\operatorname{dom}\left(\rho_{1}\right)=\{\alpha\}$, we have the result immediately from the premises.
Inductive case not on $\alpha$. For the inductive case not on $\alpha$, i.e., where $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}$ and $\rho_{2}$, where $\operatorname{dom}\left(\rho_{1}\right)=\{\beta\}$ for some $\beta \neq \alpha$, the premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1} \Xi_{\beta} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\beta}  \tag{3}\\
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1} \Xi_{\beta} ; \rho_{2} \text { cons. } \tag{4}
\end{gather*}
$$
where $\operatorname{split}_{\beta}\left(\Xi \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\beta}, \Xi_{\beta}\right)$ and $\rho_{1}=\left[\beta \mapsto \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta)\right]$. By IH on (4), we have:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \Xi_{\alpha}^{\prime} \cdot \rho_{\alpha}^{\prime} \Xi_{\alpha}^{\prime} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \Xi_{\alpha}^{\prime}  \tag{5}\\
\rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \Xi_{\alpha}^{\prime} \cdot \rho_{\alpha}^{\prime} \Xi_{\alpha}^{\prime} ; \rho_{3} \text { cons. } \tag{6}
\end{gather*}
$$
for some $\rho_{3}$, where split $_{\alpha}\left(\rho_{1} \Xi_{\not, \beta} \operatorname{dom}\left(\rho_{3}\right)\right)=\left(\Xi_{\alpha}^{\prime}, \Xi_{\not \alpha}^{\prime}\right)$ and $\rho_{\alpha}^{\prime}=\left[\alpha \mapsto \alpha \wedge u b_{\rho_{1} \Xi_{\not p}}(\alpha) \vee\right. \left.l b_{\rho_{1} \Xi_{\not \phi^{\prime}}}(\alpha)\right]$ and $\operatorname{dom}\left(\rho_{3}\right)=\operatorname{dom}\left(\rho_{2}\right) \backslash\{\alpha\}$. It is easy to see that $\Xi_{\alpha}^{\prime}=\rho_{1} \Xi_{\alpha}$ and $\Xi_{\mathscr{\alpha}}^{\prime}=\rho_{1} \Xi_{\not \phi_{\mathscr{\alpha}}}$, where $\operatorname{split}_{\alpha}\left(\Xi_{\beta}\right.$, $\left.\operatorname{dom}\left(\rho_{3}\right)\right)=\left(\Xi_{\alpha}, \Xi_{\beta \alpha}\right)$. Then (5) and (6) imply:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\alpha}  \tag{7}\\
\rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \alpha} ; \rho_{3} \text { cons. } \tag{8}
\end{gather*}
$$

Since $\left(\alpha \leqslant^{\diamond} \rho_{1} \pi\right) \in \rho_{1} \Xi_{\beta}$ only if $\left(\alpha \leqslant^{\diamond} \pi\right) \in \Xi$, we have $u b_{\rho_{1} \Xi_{\beta}}(\beta)=\rho_{1} u b_{\Xi}(\beta)$ and $l b_{\rho_{1} \Xi_{\beta}}(\beta)=\rho_{1} l b_{\Xi}(\beta)$. Then we have:
$$
\begin{equation*}
\rho_{\alpha}^{\prime}=\left[\alpha \mapsto \alpha \wedge \rho_{1} u b_{\Xi}(\alpha) \vee \rho_{1} l b_{\Xi}(\alpha)\right] \tag{9}
\end{equation*}
$$

Expanding the composition, we have:
$$
\begin{equation*}
\rho_{\alpha}^{\prime} \circ \rho_{1}=\left[\alpha \mapsto \alpha \wedge \rho_{1} u b_{\Xi}(\alpha) \vee \rho_{1} l b_{\Xi}(\alpha), \beta \mapsto \beta \wedge \rho_{\alpha}^{\prime} u b_{\Xi}(\beta) \vee \rho_{\alpha}^{\prime} l b_{\Xi}(\alpha)\right] \tag{10}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\Xi_{\beta} \vdash \beta \equiv\left[\beta \mapsto u b_{\Xi_{\beta}}(\beta) \vee l b_{\Xi_{\beta}}(\beta)\right] \beta \\
\text { i.e., } \quad \Xi_{\beta} \vdash \beta \equiv\left[\beta \mapsto u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta)\right] \beta \\
\text { i.e., } \quad \Xi_{\beta} \vdash \beta \equiv \rho_{1} \beta \tag{11}
\end{gather*}
$$

Then by Lemma B.38, (11) implies:
$$
\begin{align*}
& \Xi_{\beta} \vdash[\beta \mapsto \beta]\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \equiv\left[\beta \mapsto \rho_{1} \beta\right]\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \\
& \text { i.e., } \quad \Xi_{\beta} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge \rho_{1} u b_{\Xi}(\alpha) \vee \rho_{1} l b_{\Xi}(\alpha) \\
& \text { i.e., } \quad \Xi_{\beta} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge u b_{\rho_{1} \Xi_{\beta}}(\alpha) \vee l b_{\rho_{1} \Xi_{\beta}}(\alpha) \tag{12}
\end{align*}
$$

Then by Lemma B.39, (12) implies:
$$
\begin{align*}
\triangleright \Xi_{\beta} \vdash \rho_{\alpha} u b_{\Xi}(\beta) & \equiv \rho_{\alpha}^{\prime} u b_{\Xi}(\beta)  \tag{13}\\
\triangleright \Xi_{\beta} \vdash \rho_{\alpha} l b_{\Xi}(\beta) & \equiv \rho_{\alpha}^{\prime} l b_{\Xi}(\beta) \tag{14}
\end{align*}
$$

By Lemma B. 22 on S-Refl, (13) and (14), we have:
$$
\begin{equation*}
\triangleright \Xi_{\beta} \vdash \beta \wedge \rho_{\alpha} u b_{\Xi}(\beta) \vee \rho_{\alpha} l b_{\Xi}(\beta) \equiv \beta \wedge \rho_{\alpha}^{\prime} u b_{\Xi}(\beta) \vee \rho_{\alpha}^{\prime} l b_{\Xi}(\beta) \tag{15}
\end{equation*}
$$

Let $\rho_{1}^{\prime}=\left[\beta \mapsto \beta \wedge u b_{\rho_{\alpha} \Sigma_{\alpha}}(\beta) \vee l b_{\rho_{\alpha} \Sigma_{\alpha}}(\beta)\right]$. By the same reasoning, we have:
$$
\begin{gather*}
\rho_{1}^{\prime} \circ \rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge \rho_{1}^{\prime} u b_{\Xi}(\alpha) \vee \rho_{1}^{\prime} l b_{\Xi}(\alpha), \beta \mapsto \beta \wedge \rho_{\alpha} u b_{\Xi}(\beta) \vee \rho_{\alpha} l b_{\Xi}(\alpha)\right]  \tag{16}\\
\triangleright \Xi_{\alpha} \vdash \alpha \wedge \rho_{1} u b_{\Xi}(\alpha) \vee \rho_{1} l b_{\Xi}(\alpha) \equiv \alpha \wedge \rho_{1}^{\prime} u b_{\Xi}(\alpha) \vee \rho_{1}^{\prime} l b_{\Xi}(\alpha) \tag{17}
\end{gather*}
$$

Then by Lemma B. 38 on (15) and (17), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \vdash \rho_{\alpha}^{\prime} \rho_{1} \pi \equiv \rho_{1}^{\prime} \rho_{\alpha} \pi \quad \text { for all } \pi \tag{18}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (18), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Delta \vDash \rho_{\alpha}^{\prime} \rho_{1} \Delta \quad \text { for all } \Delta \tag{19}
\end{equation*}
$$

By Corollary B.40, we have
$$
\begin{gather*}
\quad \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\Xi_{\beta}}(\beta) \vee l b_{\Xi_{\beta}}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\beta} \vdash \pi \equiv \rho_{1} \pi \quad \text { for all } \pi \tag{20}
\end{gather*}
$$

By S-Trans on Lemma B. 25 and (20), we have:
$$
\begin{equation*}
\Xi_{\alpha} \cdot \Xi_{\beta} \models \rho_{1} \Xi_{\alpha} \tag{21}
\end{equation*}
$$

By Lemma B.28, (21) implies:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \models \triangleright \rho_{1} \Xi_{\alpha} \tag{22}
\end{equation*}
$$

By the same reasoning, we have:
$$
\begin{gather*}
\triangleright \rho_{1} \Xi_{\alpha} \cdot \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \models \triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta}  \tag{23}\\
\triangleright \Xi_{\alpha} \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \models \triangleright \Xi_{\beta} \tag{24}
\end{gather*}
$$

By Lemma B.36, (3) implies:
$$
\begin{gather*}
\triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \\
\text { i.e., } \quad \triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \not \alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \tag{25}
\end{gather*}
$$

By Lemma B. 30 with (7), (25) implies:
$$
\begin{equation*}
\triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta} \cdot \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \tag{26}
\end{equation*}
$$

Let $\operatorname{split}_{\beta}\left(\Xi_{\chi}, \operatorname{dom}\left(\rho_{3}\right)\right)=\left(\Xi_{\beta}, \Xi_{\chi \beta}\right)$. It is easy to see that $\Xi_{\not \alpha \beta}=\Xi_{\beta \not,}$. Then (26) and (8) imply:
$$
\begin{gather*}
\triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta} \cdot \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\not \alpha \beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta}  \tag{27}\\
\rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\not \alpha \beta} ; \rho_{3} \text { cons. } \tag{28}
\end{gather*}
$$

By Lemma B. 30 with (23), (27) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\alpha \beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \tag{29}
\end{equation*}
$$

By Lemma B. 30 and Lemma B. 32 with (22), (29) and (28) imply:
$$
\begin{align*}
& \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\not \alpha \beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta}  \tag{30}\\
& \rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\alpha \beta} ; \rho_{3} \text { cons. } \tag{31}
\end{align*}
$$

By Lemma B. 30 and Lemma B. 26 with (19), (30) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha \beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \vDash \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} \tag{32}
\end{equation*}
$$

By Lemma B. 43 with (15) and (17), (31) implies:
$$
\begin{equation*}
\rho_{1}^{\prime} \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha \beta} ; \rho_{3}^{\prime} \text { cons. } \tag{33}
\end{equation*}
$$
for some $\rho_{3}^{\prime}$, where $\operatorname{dom}\left(\rho_{3}^{\prime}\right)=\operatorname{dom}\left(\rho_{3}\right)$. By Lemma B. 30 and Lemma B. 32 with (24), (32) and (33) imply:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha \not p} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta}  \tag{34}\\
\rho_{1}^{\prime} \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha \not p} ; \rho_{3}^{\prime} \text { cons. } \tag{35}
\end{gather*}
$$

It is easy to see that $\operatorname{split}_{\beta}\left(\rho_{\alpha} \Xi_{\alpha}, \operatorname{dom}\left(\rho_{3}^{\prime}\right)\right)=\left(\rho_{\alpha} \Xi_{\beta}, \rho_{\alpha} \Xi_{\alpha \beta}\right)$. Then by the inductive case of the definition of consistency, (34) and (35) imply:
$$
\begin{equation*}
\rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} ; \rho_{3}^{\prime} \circ \rho_{1}^{\prime} \text { cons. } \tag{36}
\end{equation*}
$$

By Lemma B. 30 with (22), (7) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \not \alpha} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\alpha} \tag{37}
\end{equation*}
$$

By Lemma B. 30 and Lemma B. 26 with (19), (37) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta \alpha} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha} \tag{38}
\end{equation*}
$$

By Lemma B. 30 with (24), (38) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta \not \alpha} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha} \tag{39}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (39) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta \alpha} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\alpha} \tag{40}
\end{equation*}
$$

By Corollary B.40, we have
$$
\begin{array}{ccc} 
& \rho_{\alpha} \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\rho_{\alpha} \Xi_{\beta}}(\beta) \vee l b_{\rho_{\alpha} \Xi_{\beta}}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \rho_{\alpha} \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\rho_{\alpha} \Xi_{\alpha}}(\beta) \vee l b_{\rho_{\alpha} \Xi_{\alpha}}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \rho_{\alpha} \Xi_{\beta} \vdash \pi \equiv \rho_{1}^{\prime} \pi \quad \text { for all } \pi \tag{41}
\end{array}
$$

By S-Trans on Lemma B. 25 and (41), we have:
$$
\begin{equation*}
\rho_{\alpha} \Xi_{\beta} \cdot \Delta \vDash \rho_{1}^{\prime} \Delta \quad \text { for all } \Delta \tag{42}
\end{equation*}
$$

Then by Lemma B. 30 and Lemma B. 26 with (42), (40) implies:
$$
\begin{align*}
& \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\beta} \cdot \rho_{\alpha} \Xi_{\beta \alpha} \cdot \rho_{\alpha} \Sigma \models \rho_{\alpha} \Xi_{\alpha} \\
& \text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\beta} \cdot \rho_{\alpha} \Xi_{\alpha \beta} \cdot \rho_{\alpha} \Sigma \models \rho_{\alpha} \Xi_{\alpha} \\
& \text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} \cdot \rho_{\alpha} \Sigma \models \rho_{\alpha} \Xi_{\alpha} \tag{43}
\end{align*}
$$

Lemma B. 45 (Inlining of consistent bounds). If $\Sigma \vdash \Xi ; \rho$ cons. and $\Xi \cdot \Sigma \vdash \tau \leqslant \tau^{\prime}$, then $\triangleright \Xi \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime}$.

Proof. By induction on consistency derivations for the statement: if $\sum \vdash \triangleright \Xi_{\triangleright} \cdot \Xi$; $\rho$ cons. and $\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \tau \leqslant \tau^{\prime}$, then $\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime}$.
Base case. The base case is trivial since we have $\Xi=\epsilon$ and $\rho=i d$.

Inductive case. For the inductive case, we have $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee\right. \left.l b_{\Xi}(\alpha)\right]$ and $\rho_{2}$ and $\alpha$. The premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\alpha}  \tag{1}\\
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} ; \rho_{2} \text { cons. } \tag{2}
\end{gather*}
$$
where $\operatorname{split}_{\alpha}\left(\Xi \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\alpha}, \Xi_{\chi}\right)$. From the assumption, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \tau \leqslant \tau^{\prime} \tag{3}
\end{equation*}
$$

By Lemma B.36, (3) implies:
$$
\begin{gather*}
\triangleright \rho_{1} \Xi_{\triangleright} \cdot \rho_{1} \Xi \cdot \rho_{1} \Sigma \vdash \rho_{1} \tau \leqslant \rho_{1} \tau^{\prime} \\
\text { i.e., } \quad \triangleright \rho_{1} \Xi_{\triangleright} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \vdash \rho_{1} \tau \leqslant \rho_{1} \tau^{\prime} \tag{4}
\end{gather*}
$$

By Lemma B. 30 with (1), (4) implies:
$$
\begin{equation*}
\triangleright \rho_{1} \Xi_{\triangleright} \cdot \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \vdash \rho_{1} \tau \leqslant \rho_{1} \tau^{\prime} \tag{5}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\quad \Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv \rho_{1} \pi \quad \text { for all } \pi \tag{6}
\end{gather*}
$$

By S-Trans on Lemma B. 25 and (6), we have:
$$
\begin{align*}
& \Xi_{\triangleright} \cdot \Xi_{\alpha} \vDash \rho_{1} \Xi_{\triangleright}  \tag{7}\\
& \Xi_{\alpha} \cdot \Xi_{\not \mathscr{X}} \vDash \rho_{1} \Xi_{\not \partial} \tag{8}
\end{align*}
$$

Then by Lemma B. 30 with (7), (5) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \vdash \rho_{1} \tau \leqslant \rho_{1} \tau^{\prime} \tag{9}
\end{equation*}
$$

Then by IH on (2) and (9), we have:
$$
\begin{align*}
& \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho_{2} \rho_{1} \Sigma \vdash \rho_{2} \rho_{1} \tau \leqslant \rho_{2} \rho_{1} \tau^{\prime} \\
& \text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \rho_{1} \Xi_{\alpha} \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime} \tag{10}
\end{align*}
$$

Then by Lemma B. 30 with (8), (10) implies:
$$
\begin{array}{r}
\quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{\alpha} \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime} \\
\text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime} \tag{11}
\end{array}
$$

Lemma B. 46 (Equivalence of inlining of consistent bounds). If $\Sigma \vdash \Xi ; \rho$ cons., then $\overline{\Xi \cdot \sum \vdash \alpha \equiv \tau}^{(\alpha \mapsto \tau) \in \rho}$.

Proof. By induction on consistency derivations for the statement: if $\Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons., then ${\overline{\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \alpha \equiv \tau}}^{(\alpha \mapsto \tau) \in \rho}$.
Base case. The base case holds vacuously since we have $\rho=i d$.
Inductive case. For the inductive case, we have $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee\right. \left.l b_{\Xi}(\alpha)\right]$ and $\rho_{2}$ and $\alpha$. The premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \models \rho_{1} \Xi_{\alpha}  \tag{1}\\
\rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} ; \rho_{2} \text { cons. } \tag{2}
\end{gather*}
$$
where $\operatorname{split}_{\alpha}\left(\Xi, \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\alpha}, \Xi_{\alpha}\right)$. Let $\rho_{2}=\left[{\overline{\alpha_{i}} \mapsto \tau_{i}}^{i}\right]$ for some ${\overline{\alpha_{i}}}^{i}$ and $\bar{\tau}_{i}^{i}$. Expanding the composition, we have:
$$
\begin{equation*}
\rho=\left[{\overline{\alpha_{i} \mapsto \tau_{i}}}^{i}, \alpha \mapsto \rho_{2}\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right)\right] \tag{3}
\end{equation*}
$$

By IH on (2), we have:
$$
\begin{equation*}
{\overline{\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{1} \Xi_{\alpha} \cdot \rho_{1} \Sigma \vdash \alpha_{i} \equiv \tau_{i}}}_{i}^{i} \tag{4}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\quad \Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv \rho_{1} \pi \quad \text { for all } \pi \tag{5}
\end{gather*}
$$

By S-Trans on Lemma B. 25 and (5), we have:
$$
\begin{equation*}
\Xi_{\alpha} \cdot \Xi_{\not \mathscr{X}} \cdot \Sigma \models \rho_{1} \Xi_{\not \mathscr{X}} \cdot \rho_{1} \Sigma \tag{6}
\end{equation*}
$$

Then by Lemma B. 30 with (6), (4) implies:
$$
\begin{array}{r}
\quad{\overline{\triangleright \Xi_{\triangleright} \cdot \Xi_{\alpha} \cdot \Xi_{\alpha} \cdot \Sigma \vdash \alpha_{i} \equiv \tau_{i}}}_{i}^{i} \\
\text { i.e., } \quad{\overline{\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \alpha_{i} \equiv \tau_{i}}}^{i} \tag{7}
\end{array}
$$

By Lemma B. 38 on (7), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \pi \equiv \rho_{2} \pi \quad \text { for all } \pi \tag{8}
\end{equation*}
$$

Then by S-Trans on (5) and (8), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \alpha \equiv \rho_{2}\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \tag{9}
\end{equation*}
$$

Then (7) and (9) imply:
$$
\begin{equation*}
\overline{\triangleright \Xi_{\triangleright} \cdot \Xi \cdot \Sigma \vdash \alpha \equiv \tau}(\alpha \mapsto \tau) \in \rho \tag{10}
\end{equation*}
$$

Lemma B. 47 (Congruence of inlining of consistent bounds on types). If $\Sigma \vdash \Xi ; \rho$ cons., then $\Xi \cdot \Sigma \vdash \tau \equiv \rho \tau$ for all $\tau$.

Proof. By induction on the syntax of $\tau$.
Case $\tau=\tau_{1} \rightarrow \tau_{2}$. By IH, we have:
$$
\begin{align*}
& \Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{1}\\
& \Xi \cdot \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{2}
\end{align*}
$$

By Lemma B. 30 with Lemma B.25, (1) and (2) imply:
$$
\begin{align*}
& \triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{3}\\
& \triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{4}
\end{align*}
$$

Then by S-FunDepth on (3) and (4), we have:
$$
\begin{array}{ll} 
& \Xi \cdot \Sigma \vdash \tau_{1} \rightarrow \tau_{2} \equiv \rho \tau_{1} \rightarrow \rho \tau_{2} \\
\text { i.e., } & \Xi \cdot \Sigma \vdash \tau_{1} \rightarrow \tau_{2} \equiv \rho\left(\tau_{1} \rightarrow \tau_{2}\right) \tag{5}
\end{array}
$$

Case $\tau=\left\{x: \tau_{1}\right\}$. By IH, we have:
$$
\begin{equation*}
\Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{6}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (6) implies:
$$
\begin{equation*}
\triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{7}
\end{equation*}
$$

Then by S-RcoDepth on (7), we have:
$$
\begin{align*}
& \Xi \cdot \Sigma \vdash\left\{x: \tau_{1}\right\} \\
\text { i.e., } \quad & \equiv\left\{x: \rho \rho_{1}\right\}  \tag{8}\\
& =\left\{x: \tau_{1}\right\} \\
& \equiv \rho\left\{x: \tau_{1}\right\}
\end{align*}
$$

Cases $\tau=\# C, \tau=\mathrm{T}^{\diamond}, \tau=\alpha \notin \operatorname{dom}(\rho)$. Then $\tau=\rho \tau$. By S-Refl, we have:
$$
\begin{equation*}
\tau \equiv \rho \tau \tag{9}
\end{equation*}
$$

Case $\tau=\alpha \in \operatorname{dom}(\rho)$. From the assumption, we have:
$$
\begin{equation*}
\Sigma \vdash \Xi ; \rho \text { cons. } \tag{10}
\end{equation*}
$$

By Lemma B. 46 on (10), we have:
$$
\begin{equation*}
\Xi \cdot \Sigma \vdash \alpha \equiv \rho \alpha \tag{11}
\end{equation*}
$$

Case $\tau=\tau_{1} \vee^{\diamond} \tau_{2}$. By IH, we have:
$$
\begin{align*}
& \Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{12}\\
& \Xi \cdot \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{13}
\end{align*}
$$

Then by Lemma B. $22 \diamond$ on (12) and (13), we have:
$$
\begin{array}{ll} 
& \Xi \cdot \Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \equiv \rho \tau_{1} \vee^{\diamond} \rho \tau_{2} \\
\text { i.e., } & \Xi \cdot \Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \equiv \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \tag{14}
\end{array}
$$

Case $\tau=\neg \tau_{1}$. By IH, we have:
$$
\begin{equation*}
\Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{15}
\end{equation*}
$$

Then by S-NegInv on (15), we have:
$$
\begin{array}{ll} 
& \Xi \cdot \Sigma \vdash \neg \tau_{1} \equiv \neg \rho \tau_{1} \\
\text { i.e., } & \Xi \cdot \Sigma \vdash \neg \tau_{1} \equiv \rho \neg \tau_{1} \tag{16}
\end{array}
$$

Lemma B. 48 (Congruence of inlining of consistent bounds on guarded types). If $\Sigma \vdash \Xi$; $\rho$ cons. and $\operatorname{TTV}(\tau)=\varnothing$, then $\triangleright \Xi \cdot \triangleright \Sigma \vdash \tau \equiv \rho \tau$.

Proof. By induction on the syntax of $\tau$.
Case $\tau=\tau_{1} \rightarrow \tau_{2}$. By Lemma B.47, we have:
$$
\begin{align*}
& \Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{1}\\
& \Xi \cdot \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{2}
\end{align*}
$$

By Lemma B. 30 with Lemma B.25, (1) and (2) imply:
$$
\begin{align*}
& \triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{3}\\
& \triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{4}
\end{align*}
$$

Then by S-FunDepth on (3) and (4), we have:
$$
\begin{array}{ll} 
& \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \rightarrow \tau_{2} \equiv \rho \tau_{1} \rightarrow \rho \tau_{2} \\
\text { i.e., } & \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \rightarrow \tau_{2} \equiv \rho\left(\tau_{1} \rightarrow \tau_{2}\right) \tag{5}
\end{array}
$$

Case $\tau=\left\{x: \tau_{1}\right\}$. By Lemma B.47, we have:
$$
\begin{equation*}
\Xi \cdot \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{6}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (6) implies:
$$
\begin{equation*}
\triangleleft \Xi \cdot \triangleleft \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{7}
\end{equation*}
$$

Then by S-RcoDepth on (7), we have:
$$
\begin{align*}
\triangleright \Xi \cdot \triangleright \Sigma \vdash\left\{x: \tau_{1}\right\} & \equiv\left\{x: \rho \tau_{1}\right\} \\
\text { i.e., } \quad \triangleright \Xi \cdot \triangleright \Sigma \vdash\left\{x: \tau_{1}\right\} & \equiv \rho\left\{x: \tau_{1}\right\} \tag{8}
\end{align*}
$$

Cases $\tau=\# C, \tau=\mathrm{T}^{\diamond}$. Then $\tau=\rho \tau$. By S-Refl, we have:
$$
\begin{equation*}
\tau \equiv \rho \tau \tag{9}
\end{equation*}
$$

Case $\tau=\alpha$. Impossible since $T T V(\tau)=\varnothing$.
Case $\tau=\tau_{1} \vee^{\diamond} \tau_{2}$. By IH, we have:
$$
\begin{align*}
& \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{10}\\
& \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{2} \equiv \rho \tau_{2} \tag{11}
\end{align*}
$$

Then by Lemma B. $22 \diamond$ on (10) and (11), we have:
$$
\begin{array}{ll} 
& \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \equiv \rho \tau_{1} \vee^{\diamond} \rho \tau_{2} \\
\text { i.e., } & \triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \vee^{\diamond} \tau_{2} \equiv \rho\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \tag{12}
\end{array}
$$

Case $\tau=\neg \tau_{1}$. By IH, we have:
$$
\begin{equation*}
\triangleright \Xi \cdot \triangleright \Sigma \vdash \tau_{1} \equiv \rho \tau_{1} \tag{13}
\end{equation*}
$$

Then by S-NegInv on (13), we have:
$$
\begin{array}{ll} 
& \triangleright \Xi \cdot \triangleright \Sigma \vdash \neg \tau_{1} \equiv \neg \rho \tau_{1} \\
\text { i.e., } & \triangleright \Xi \cdot \triangleright \Sigma \vdash \neg \tau_{1} \equiv \rho \neg \tau_{1} \tag{14}
\end{array}
$$

Lemma B. 49 (Inlining of consistent bounds on guarded derivations). If $\Sigma \vdash \Xi ; \rho$ cons. and $\Xi \cdot \Sigma \vdash \tau \leqslant \tau^{\prime}$ and $\operatorname{TTV}(\tau) \cup \operatorname{TTV}\left(\tau^{\prime}\right)=\varnothing$, then $\triangleright \Xi \cdot \triangleright \Sigma \cdot \rho \Sigma \vdash \tau \leqslant \tau^{\prime}$.

Proof. From the assumptions, we have:
$$
\begin{gather*}
\Sigma \vdash \Xi ; \rho \text { cons. }  \tag{1}\\
\Xi \cdot \Sigma \vdash \tau \leqslant \tau^{\prime} \tag{2}
\end{gather*}
$$

By Lemma B. 45 on (1) and (2), we have:
$$
\begin{equation*}
\triangleright \Xi \cdot \rho \Sigma \vdash \rho \tau \leqslant \rho \tau^{\prime} \tag{3}
\end{equation*}
$$

By Lemma B. 48 on (1), we have:
$$
\begin{align*}
\triangleright \Xi \cdot \triangleright \Sigma \vdash \tau & \equiv \rho \tau  \tag{4}\\
\triangleright \Xi \cdot \triangleright \Sigma \vdash \tau^{\prime} & \equiv \rho \tau^{\prime} \tag{5}
\end{align*}
$$

Then by S-Trans on (4), (3), and (5), we have:
$$
\begin{equation*}
\triangleright \Xi \cdot \triangleright \Sigma \cdot \rho \Sigma \vdash \tau \leqslant \tau^{\prime} \tag{6}
\end{equation*}
$$

Lemma B. 50 (Inlining of bound in consistency). If $\Sigma \cdot\left(\alpha \leqslant^{\diamond} \tau\right) \vdash \nabla \Xi_{\triangleright} \cdot \Xi ; \rho$ cons., where $\alpha \notin \operatorname{dom}(\rho)$, then $\rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha} \Xi ; \rho^{\prime}$ cons. for some $\rho^{\prime}$, where $\rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge^{\diamond} \tau\right]$ and $\operatorname{dom}\left(\rho^{\prime}\right)=\operatorname{dom}(\rho)$.

Proof. By induction on consistency derivations. If $\Xi$ is not guarded, we can replace it with $\operatorname{cleanup}(\Xi)$ before applying the lemma, and restore it back to $\Xi$ in the conclusion. Therefore we can assume $\Xi$ guard..
Base case. For the base case, we have $\Xi=\epsilon$. Then by the base case of the definition of consistency, we have:
$$
\begin{equation*}
\rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright(\alpha \leqslant \delta) ; \text { id cons. } \tag{1}
\end{equation*}
$$

Inductive case. For the inductive case, we have $\rho=\rho_{2} \circ \rho_{1}$ for some $\rho_{1}=\left[\beta \mapsto \beta \wedge u b_{\Xi}(\beta) \vee\right. \left.l b_{\Xi}(\beta)\right]$ and $\rho_{2}$ and $\beta \neq \alpha$. The premises of the rule are:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1} \Xi_{\beta} \cdot \rho_{1} \Sigma \cdot \rho_{1}\left(\alpha \leqslant \leqslant^{\diamond} \tau\right) \vDash \rho_{1} \Xi_{\beta}  \tag{2}\\
\rho_{1} \Sigma \cdot \rho_{1}(\alpha \leqslant \tau) \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \rho_{1} \Xi_{\beta} ; \rho_{2} \text { cons. } \tag{3}
\end{gather*}
$$
where $\operatorname{split}_{\beta}\left(\Xi \operatorname{dom}\left(\rho_{2}\right)\right)=\left(\Xi_{\beta}, \Xi_{\beta}\right)$. By IH on (3), we have:
$$
\begin{equation*}
\rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright\left(\alpha \leqslant \rho_{1} \tau\right) \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} ; \rho_{2}^{\prime} \text { cons. } \tag{4}
\end{equation*}
$$
for some $\rho_{2}^{\prime}$, where $\rho_{\alpha}^{\prime}=\left[\alpha \mapsto \alpha \wedge^{\diamond} \rho_{1} \tau\right]$ and $\operatorname{dom}\left(\rho_{2}^{\prime}\right)=\operatorname{dom}\left(\rho_{2}\right)$. Expanding the composition, we have:
$$
\begin{equation*}
\rho_{\alpha}^{\prime} \circ \rho_{1}=\left[\alpha \mapsto \alpha \wedge^{\diamond} \rho_{1} \tau, \beta \mapsto \beta \wedge \rho_{\alpha}^{\prime} u b_{\Xi}(\beta) \vee \rho_{\alpha}^{\prime} l b_{\Xi}(\beta)\right] \tag{5}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{align*}
& \left(\alpha \leqslant^{\diamond} \tau\right) \vDash \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta) \equiv\left[\alpha \mapsto \alpha \wedge^{\diamond} \tau\right]\left(\beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta)\right) \\
& \text { i.e., } \quad\left(\alpha \leqslant^{\diamond} \tau\right) \vDash \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta) \equiv \beta \wedge \rho_{\alpha} u b_{\Xi}(\beta) \vee \rho_{\alpha} l b_{\Xi}(\beta) \\
& \text { i.e., } \quad\left(\alpha \leqslant^{\diamond} \tau\right) \vDash \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta) \equiv \beta \wedge u b_{\rho_{\alpha} \Xi}(\beta) \vee l b_{\rho_{\alpha} \Xi}(\beta) \tag{6}
\end{align*}
$$

Then by Lemma B.39, (6) implies:
$$
\begin{equation*}
\triangleright(\alpha \leqslant \tau) \models \rho_{1} \tau \equiv \rho_{1}^{\prime} \tau \tag{7}
\end{equation*}
$$

Then by Lemma B. 22 on S-Refl and (7), we have:
$$
\begin{equation*}
\triangleright(\alpha \leqslant \tau) \models \alpha \wedge^{\diamond} \rho_{1} \tau \equiv \alpha \wedge^{\diamond} \rho_{1}^{\prime} \tau \tag{8}
\end{equation*}
$$

Let $\rho_{1}^{\prime}=\left[\beta \mapsto \beta \wedge u b_{\rho_{\alpha} \Xi}(\beta) \vee l b_{\rho_{\alpha} \Xi}(\beta)\right]$. By the same reasoning, we have:
$$
\begin{align*}
& \rho_{1}^{\prime} \circ \rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge^{\diamond} \rho_{1}^{\prime} \tau, \beta \mapsto \beta \wedge u b_{\rho_{\alpha} \Xi}(\beta) \vee l b_{\rho_{\alpha} \Xi}(\beta)\right] \\
&=\left[\alpha \mapsto \alpha \wedge^{\diamond} \rho_{1}^{\prime} \tau, \beta \mapsto \beta \wedge \rho_{\alpha} u b_{\Xi}(\beta) \vee \rho_{\alpha} l b_{\Xi}(\beta)\right]  \tag{9}\\
& \triangleright \Xi_{\beta} \vDash \beta \wedge \rho_{\alpha} u b_{\Xi}(\beta) \vee \rho_{\alpha} l b_{\Xi}(\beta) \equiv \beta \wedge \rho_{\alpha}^{\prime} u b_{\Xi}(\beta) \vee \rho_{\alpha}^{\prime} l b_{\Xi}(\beta) \tag{10}
\end{align*}
$$

Then by Lemma B. 38 on (8) and (10), we have:
$$
\begin{equation*}
\triangleright\left(\alpha \wedge^{\diamond} \tau\right) \cdot \triangleright \Xi_{\beta} \vDash \rho_{\alpha}^{\prime} \rho_{1} \pi \equiv \rho_{1}^{\prime} \rho_{\alpha} \pi \quad \text { for all } \pi \tag{11}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (11), we have:
$$
\begin{equation*}
\triangleright\left(\alpha \wedge^{\diamond} \tau\right) \cdot \triangleright \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Delta \vDash \rho_{\alpha}^{\prime} \rho_{1} \Delta \quad \text { for all } \Delta \tag{12}
\end{equation*}
$$

By Corollary B.40, we have
$$
\begin{array}{ccc} 
& \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\Xi_{\beta}}(\beta) \vee l b_{\Xi_{\beta}}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\beta} \vdash \pi \equiv\left[\beta \mapsto \beta \wedge u b_{\Xi}(\beta) \vee l b_{\Xi}(\beta)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\beta} \vdash \pi \equiv \rho_{1} \pi \quad \text { for all } \pi \tag{13}
\end{array}
$$

By S-Trans on Lemma B. 25 and (13), we have:
$$
\begin{equation*}
\left(\alpha \leqslant^{\diamond} \tau\right) \cdot \Xi_{\beta} \models\left(\alpha \leqslant^{\diamond} \rho_{1} \tau\right) \tag{14}
\end{equation*}
$$

By Lemma B.28, (14) implies:
$$
\begin{equation*}
\triangleright(\alpha \leqslant \tau) \cdot \triangleright \Xi_{\beta} \models \triangleright\left(\alpha \leqslant \rho_{1} \tau\right) \tag{15}
\end{equation*}
$$

By the same reasoning, we have:
$$
\begin{gather*}
\triangleright\left(\alpha \leqslant^{\diamond} \rho_{1} \tau\right) \cdot \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \models \triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta}  \tag{16}\\
\triangleright \Xi_{\beta} \cdot \triangleright(\alpha \leqslant \tau) \models \triangleright\left(\alpha \leqslant \rho_{1} \tau\right)  \tag{17}\\
\triangleright(\alpha \leqslant \tau) \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \models \triangleright \Xi_{\beta} \tag{18}
\end{gather*}
$$

By Lemma B.42, (2) implies:
$$
\begin{equation*}
\triangleright \rho_{\alpha}^{\prime} \Xi_{\triangleright} \cdot \triangleright \rho_{\alpha}^{\prime} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \cdot \triangleright\left(\alpha \leqslant \rho_{1} \tau\right) \vDash \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \tag{19}
\end{equation*}
$$

By Lemma B. 30 with (16), (19) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \cdot \triangleright\left(\alpha \leqslant^{\diamond} \rho_{1} \tau\right) \vDash \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} \tag{20}
\end{equation*}
$$

By Lemma B. 30 with (17), (20) and (4) implies:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright\left(\alpha \leqslant^{\diamond} \tau\right) \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta \gamma} \cdot \rho_{\alpha}^{\prime} \rho_{1} \Sigma \models \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta}  \tag{21}\\
\rho_{\alpha}^{\prime} \rho_{1} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright\left(\alpha \leqslant^{\diamond} \rho_{1} \tau\right) \cdot \rho_{\alpha}^{\prime} \rho_{1} \Xi_{\beta} ; \rho_{2}^{\prime} \text { cons. } \tag{22}
\end{gather*}
$$

By Lemma B. 30 and Lemma B. 26 with (12), (21) implies:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} \tag{23}
\end{equation*}
$$

By Lemma B. 43 with (8) and (10), (22) implies:
$$
\begin{equation*}
\rho_{1}^{\prime} \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\beta} \cdot \triangleright\left(\alpha \leqslant \rho_{1} \tau\right) \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} ; \rho_{2}^{\prime \prime} \text { cons. } \tag{24}
\end{equation*}
$$
for some $\rho_{2}^{\prime \prime}$, where $\operatorname{dom}\left(\rho_{2}^{\prime \prime}\right)=\operatorname{dom}\left(\rho_{2}^{\prime}\right)$. By Lemma B. 30 with (18), (23) and (24) implies:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright\left(\alpha \leqslant^{\diamond} \tau\right) \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Sigma \models \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta}  \tag{25}\\
\rho_{1}^{\prime} \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright\left(\alpha \leqslant \rho_{1} \tau\right) \cdot \triangleright \rho_{\alpha} \Xi_{\beta} \cdot \rho_{1}^{\prime} \rho_{\alpha} \Xi_{\beta} ; \rho_{2}^{\prime \prime} \text { cons. } \tag{26}
\end{gather*}
$$

It is easy to see that $\operatorname{split}_{\beta}\left(\rho_{\alpha} \Xi \operatorname{dom}\left(\rho_{2}^{\prime \prime}\right)\right)=\left(\rho_{\alpha} \Xi_{\beta}, \rho_{\alpha} \Xi_{\beta}\right)$. Then by the inductive case of the definition of consistency, (25) and (26) imply:
$$
\begin{equation*}
\rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright\left(\alpha \leqslant^{\diamond} \rho_{1} \tau\right) \cdot \rho_{\alpha} \Xi ; \rho_{2}^{\prime \prime} \circ \rho_{1}^{\prime} \text { cons. } \tag{27}
\end{equation*}
$$

\section*{B. 7 Reasoning Behind Proof Structure}

The structure of the remaining proofs is quite complex, with many additional syntax forms and relations introduced. We first shed some light on the reasoning behind them.

Our first goal is to prove subtyping consistency (Theorem B.88), which describes how the basic type constructors of the language should or should not relate by subtyping, and in particular prevents wrong relations, such as function types subtyping record types. However, its proof cannot proceed by the standard technique of induction on subtyping derivations. Due to the restriction of the type forms, the inductive hypothesis cannot be applied to the premises of S-Trans, as the middle type introduced may not adhere to the restriction. A quick inspection reveals that the problem lies within S-AndOr2. While some usages of S-AndOr2 can be removed by rewritting the derivation, not all usages can be removed. The solution we adopted was to split the full $\leqslant$ subtyping relation into two, with $\subseteq$ covering the pure Boolean-algebraic relation and $\leq$ covering the remaining relation between the atoms and coatoms in the form of elementary type forms, which will be introduced later. This allows us to state them separately in Lemma B.89.

The statement of Lemma B. 89 is quite complex. It helps to first look at the statement of our first attempt, which does not hold in general:
$$
\begin{aligned}
& \text { (1) If } \triangleright \Sigma \vdash \tau \leqslant \pi \text { and } \tau \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \text {, then there exists some }{\overline{\pi_{j}^{\prime}}}^{j} \text { and }{\overline{D_{j}}}^{j} \text { and }{\overline{V_{j}^{D_{j}}}}^{j} \\
& \text { such that } \pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \text { and } \triangleright \Sigma \vdash \bigwedge_{i \in S_{j}} U_{i}^{C_{i}} \leq V_{j}^{D_{j}} \text { for some }{\overline{S_{j}}}^{j} \text {. } \\
& \text { (2) If } \triangleright \Sigma \vdash \tau \leqslant \pi \text { and } \pi \cong \bigvee_{j}\left(\pi_{j}^{\prime} \wedge{\left.\frac{Y_{j}^{D_{j}}}{i}\right) \text {, then there exists some }{\overline{\tau_{i}^{\prime}}}^{i} \text { and }{\overline{C_{i}}}^{i} \text { and }{\overline{X_{i}^{C_{i}}}}^{i}}_{i}^{i} \text { for some }{\overline{S_{i}}}^{i} \text { such that } \tau \cong \bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right) \text { and } \triangleright \Sigma \vdash X_{i}^{C_{i}} \leq \bigvee_{j \in S_{i}} Y_{j}^{D_{j}}\right. \text { for }
\end{aligned}
$$

The proof of this lemma also cannot proceed by standard induction due to the interaction between S-AndOr2 and S-Distrib. As an example, consider the following derivation for some $\tau \in\left\{\perp, \top, \# C^{\prime}, \tau_{1} \rightarrow \tau_{2},\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}\right\}$ and unrelated classes $C$ and $D$ :
$$
\begin{gathered}
\text { S-Distrib. } \frac{}{\# C \wedge(\# D \vee \neg \# C) \leqslant \# C \wedge \# D \vee \# C \wedge \neg \# C} \quad \frac{\vdots}{\# C \wedge \# D \vee \# C \wedge \neg \# C \leqslant \perp} \\
\text { S-Trans } \\
\text { (1) \#C^ }(\# D \vee \neg \# C) \leqslant \perp \\
\text { S-AndOr2 } \frac{\frac{\vdots}{\tau \leqslant \# C} \quad \frac{\vdots}{\tau \leqslant \# D \vee \neg \# C}}{\tau \leqslant \# C \wedge(\# D \vee \neg \# C)} \quad \text { (1) } \\
\text { S-Trans } \frac{\text { (1) }}{\tau \leqslant \perp}
\end{gathered}
$$

According to our goal of Theorem B.88, $\tau$ can only be $\perp$. However, from the subderivations for $\tau \leqslant \# C$ and $\tau \leqslant \# D \vee \neg \# C$, nothing locally restricts $\tau$ to be ⟂ . This is because S-Distrib can split a complement into two separate subderivations to be later merged back together by S-AndOr2. To overcome this difficulty, we normalize the shape of subtyping derivations by introducing the CDNand DCN-normalized type forms and derivations. CDN- and DCN-normalized derivations require S-Distrib◇ to be followed immediately by S-AndOr2 ↓. We show that all types and subtyping derivations can be translated into an equivalent CDN-normalized one and an equivalent DCNnormalized one. This allows us to perform the proof of Lemma B. 89 by induction on CDN- and DCN-normalized subtyping derivations.

As we mentioned before, the above simplified version of Lemma B. 89 does not hold in general. The problematic cases arise when $\tau \equiv \perp$ for direction 1 and $\pi \equiv \top$ for direction 2 . Since the relation holds by S-Trans with S-ToB for any type on the other side, we should not be able to conclude anything about it. Fortunately, we do not need to care about such cases for proving Theorem B.88. Therefore, we can exclude them by adding side conditions on the elementary type forms, and making sure that they are preserved in the conclusion of the lemma, allowing us to apply it successively to a transitivity chain. For direction 1 , in order to reject cases where $\tau \cong \perp$, we require $\bigwedge_{i} U_{i}^{C_{i}}$ to be complement-free, then we have $\tau \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \supseteq \bigwedge_{i} U_{i}^{C_{i}} \subsetneq \perp$, which implies $\tau \nsubseteq \perp$ by the antisymmetry and boundedness of Boolean algebras. For direction 2, we symmetrically require $\bigvee_{j} Y_{j}^{D_{j}}$ to be complement-free. To reject cases where $\tau \equiv \perp$ but $\tau \nsubseteq \perp$ for direction 1 , we add restrictions on the set of elementary type constructors $\left\{{\overline{C_{i}}}^{i}\right\}$. For example, since we can derive $\tau_{1} \rightarrow \tau_{2} \leqslant \tau_{3} \rightarrow \tau_{4}$ for some $\bar{\tau}_{i}^{i \in 1 . .4}$, which implies $\tau_{1} \rightarrow \tau_{2} \wedge \neg\left(\tau_{3} \rightarrow \tau_{4}\right) \leqslant \perp$ by Theorem B.20, we reject cases where both $\rightarrow \in\left\{{\overline{C_{i}}}^{i}\right\}$ and $\rightarrow \in\left\{{\overline{C_{i}}}^{i}\right\}$. We can derive similar restrictions from other subtyping rules, and symmetric restrictions for direction 2.

So far, we have ignored the subtyping context by requiring it to be guarded. Our handling of type variables and the subtyping context relies on two key insights: for Theorem B.88, we do not care about type variables on the top level; and we do not care about all possible subtyping contexts, only the ones produced by type inference. We have previously defined the consistency of constraining contexts, and by ensuring type inference only produces consistent contexts, this allows us to guard the context in any subtyping derivations under consistent contexts and with no type variables on the top level by Lemma B.49, which are all we care about for the remaining soundness and completeness proofs.

\section*{B. 8 Pure Boolean-Algebraic Subtyping}

First, we define $\subseteq$ as the standard Boolean lattice order.
Definition B. 51 (Pure Boolean-Algebraic Subtyping). We define $\tau_{1} \subseteq \tau_{2}$ to mean that $\tau_{1} \leqslant \tau_{2}$ can be derived by using only "Boolean Lattice" subtyping rules, which are those that that are not specific to $\lambda\urcorner$ types and simply encode their Boolean-Algebraic structure. More specifically, these rules are: S-Refl, S-ToB, S-Compl, S-NegInv, S-AndOr11, S-AndOr12, S-AndOr2, S-Distrib, and S-Trans.

Theorem B. 52 (Standard Boolean Lattice Order). $\subseteq$ holds in every Boolean lattice, i.e., it does not introduce any extra relations between its atoms, which are $\lambda\urcorner$ types.

Since $\subseteq$ is itself a Boolean Algebra (see Section 4.4.4), this means our rules for $\subseteq$ are a proper axiomatization of Boolean Algebras.

Proof. We show that the $\subseteq$ rules follow from the pure Boolean algebra axioms. $\equiv$ is the pure Boolean algebra equivalence, defined by the following axioms [Huntington 1904]:
$$
\begin{aligned}
\text { B-IDEN } \diamond: & \tau \wedge^{\diamond} \mathrm{T}^{\diamond} & \equiv \tau \\
\text { B-COMMUT } \diamond: & \tau_{1} \vee^{\diamond} \tau_{2} & \equiv \tau_{2} \vee^{\diamond} \tau_{1} \\
\text { B-DISTRIB } \diamond: & \tau \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & \equiv\left(\tau \wedge^{\diamond} \tau_{1}\right) \vee^{\diamond}\left(\tau \wedge^{\diamond} \tau_{2}\right) \\
\text { B-COMPL } \diamond: & \tau \vee^{\diamond} \neg \tau & \equiv \mathrm{T}^{\diamond}
\end{aligned}
$$

The following laws follow from the axioms [Huntington 1904]:
$$
\begin{aligned}
\text { B-IDEM } \diamond: & \tau \vee^{\diamond} \tau & \equiv \tau \\
\text { B-BOUND } \diamond: & \tau \vee^{\diamond} \mathrm{T}^{\diamond} & \equiv \mathrm{T}^{\diamond} \\
\text { B-ABSORP } \diamond: & \tau_{1} \wedge^{\diamond}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & \equiv \tau_{1} \\
\text { B-DEMORGAN } \diamond: & \neg\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & \equiv\left(\neg \tau_{1} \wedge^{\diamond} \neg \tau_{2}\right) \\
\text { B-AssOC } \diamond: & \left(\tau_{1} \vee^{\diamond} \tau_{2}\right) \vee^{\diamond} \tau_{3} & \equiv \tau_{1} \vee^{\diamond}\left(\tau_{2} \vee^{\diamond} \tau_{3}\right)
\end{aligned}
$$

Recall that $\tau_{1} \subseteq \tau_{2}$ is taken to mean $\tau_{1} \equiv \tau_{1} \wedge \tau_{2}$ (Section 4.4.4).

\section*{S-Refl.}
$$
\tau \equiv \tau \wedge \tau \quad \text { by B-IDEM } 2
$$

\section*{S-ToB.}
$$
\tau \equiv \tau \wedge \top \quad \text { by B-IDen }
$$

\section*{S-ToB.}
$$
\begin{aligned}
\perp & \equiv \tau \wedge \perp & & \text { by B-Bound } \downarrow \\
& \equiv \perp \wedge \tau & & \text { by B-Commut } \circlearrowright
\end{aligned}
$$

\section*{S-COMPL•}
$$
\begin{aligned}
\top & \equiv \tau \vee \neg \tau & & \text { by B-Compl. } \\
& \equiv(\tau \vee \neg \tau) \wedge \top & & \text { by B-Iden. } \\
& \equiv \top \wedge(\tau \vee \neg \tau) & & \text { by B-Commut }>
\end{aligned}
$$

\section*{S-Compl.}
$$
\begin{aligned}
\tau \wedge \neg \tau & \equiv \perp & & \text { by B-Compl } 2 \\
& \equiv(\tau \wedge \neg \tau) \wedge \perp & & \text { by B-Bound } 2
\end{aligned}
$$

\section*{S-NegInv.}
$$
\begin{aligned}
\neg \tau_{2} & \equiv \neg \tau_{2} \wedge\left(\neg \tau_{2} \vee \neg \tau_{1}\right) & & \text { by B-ABSORP. } \\
& \equiv \neg \tau_{2} \wedge\left(\neg \tau_{1} \vee \neg \tau_{2}\right) & & \text { by B-COMMUT. } \\
& \equiv \neg \tau_{2} \wedge \neg\left(\tau_{1} \wedge \tau_{2}\right) & & \text { by B-DEMORGAN } \supset \\
& \equiv \neg \tau_{2} \wedge \neg \tau_{1} & & \text { by assumption } \tau_{1} \subseteq \tau_{2} \Leftrightarrow \tau_{1} \equiv \tau_{1} \wedge \tau_{2}
\end{aligned}
$$

\section*{S-AndOr11.}
$$
\tau_{1} \equiv \tau_{1} \wedge\left(\tau_{1} \vee \tau_{2}\right) \quad \text { by B-Absorp }
$$

\section*{S-AndOr11>.}
$$
\begin{aligned}
\tau_{1} \wedge \tau_{2} & \equiv\left(\tau_{1} \wedge \tau_{1}\right) \wedge \tau_{2} & & \text { by B-IDEM } \downarrow \\
& \equiv \tau_{1} \wedge\left(\tau_{1} \wedge \tau_{2}\right) & & \text { by B-Assoc } \downarrow \\
& \equiv\left(\tau_{1} \wedge \tau_{2}\right) \wedge \tau_{1} & & \text { by B-Commut }>
\end{aligned}
$$

\section*{S-AndOr12.}
$$
\begin{aligned}
\tau_{2} & \equiv \tau_{2} \wedge\left(\tau_{2} \vee \tau_{1}\right) & & \text { by B-AвSORP. } \\
& \equiv \tau_{2} \wedge\left(\tau_{1} \vee \tau_{2}\right) & & \text { by B-CоммUT. }
\end{aligned}
$$

\section*{S-AndOr12>.}
$$
\begin{aligned}
\tau_{1} \wedge \tau_{2} & \equiv \tau_{1} \wedge\left(\tau_{2} \wedge \tau_{2}\right) & & \text { by B-IDEM }> \\
& \equiv\left(\tau_{1} \wedge \tau_{2}\right) \wedge \tau_{2} & & \text { by B-Assoc }>
\end{aligned}
$$

\section*{S-AndOr2-}
$$
\begin{aligned}
\tau_{1} \vee \tau_{2} & \equiv\left(\tau_{1} \wedge \tau\right) \vee \tau_{2} & & \text { by assumption } \tau_{1} \subseteq \tau \Leftrightarrow \tau_{1} \equiv \tau_{1} \wedge \tau \\
& \equiv\left(\tau_{1} \wedge \tau\right) \vee\left(\tau_{2} \wedge \tau\right) & & \text { by assumption } \tau_{2} \subseteq \tau \Leftrightarrow \tau_{2} \equiv \tau_{2} \wedge \tau \\
& \equiv\left(\tau \wedge \tau_{1}\right) \vee\left(\tau_{2} \wedge \tau\right) & & \text { by B-Commut } \downarrow \\
& \equiv\left(\tau \wedge \tau_{1}\right) \vee\left(\tau \wedge \tau_{2}\right) & & \text { by B-Commut } \downarrow \\
& \equiv \tau \wedge\left(\tau_{1} \vee \tau_{2}\right) & & \text { by B-Distrib. } \\
& \equiv\left(\tau_{1} \vee \tau_{2}\right) \wedge \tau & & \text { by B-Commut } \downarrow
\end{aligned}
$$

\section*{S-AndOr2 .}
$$
\begin{aligned}
\tau & \equiv \tau \wedge \tau_{2} & & \text { by assumption } \tau \subseteq \tau_{2} \Leftrightarrow \tau \equiv \tau \wedge \tau_{2} \\
& \equiv\left(\tau \wedge \tau_{1}\right) \wedge \tau_{2} & & \text { by assumption } \tau \subseteq \tau_{1} \Leftrightarrow \tau \equiv \tau \wedge \tau_{1} \\
& \equiv \tau \wedge\left(\tau_{1} \wedge \tau_{2}\right) & & \text { by B-Assoc } \text { ? }
\end{aligned}
$$

\section*{S-Distrib.}
$$
\begin{aligned}
\tau \wedge\left(\tau_{1} \vee \tau_{2}\right) & \equiv\left(\tau \wedge\left(\tau_{1} \vee \tau_{2}\right)\right) \wedge\left(\tau \wedge\left(\tau_{1} \vee \tau_{2}\right)\right) & & \text { by B-IDEM } 2 \\
& \equiv\left(\tau \wedge\left(\tau_{1} \vee \tau_{2}\right)\right) \wedge\left(\left(\tau \wedge \tau_{1}\right) \vee\left(\tau \wedge \tau_{2}\right)\right) & & \text { by B-DISTRIB. }
\end{aligned}
$$

\section*{S-Distrib.}
$$
\begin{aligned}
\left(\tau \vee \tau_{1}\right) \wedge\left(\tau \vee \tau_{2}\right) & \equiv\left(\left(\tau \vee \tau_{1}\right) \wedge\left(\tau \vee \tau_{2}\right)\right) \wedge\left(\left(\tau \vee \tau_{1}\right) \wedge\left(\tau \vee \tau_{2}\right)\right) & & \text { by B-IDEM }> \\
& \equiv\left(\left(\tau \vee \tau_{1}\right) \wedge\left(\tau \vee \tau_{2}\right)\right) \wedge\left(\tau \vee\left(\tau_{1} \wedge \tau_{2}\right)\right) & & \text { by B-DISTRIB }>
\end{aligned}
$$

\section*{S-Trans.}
$$
\begin{aligned}
\tau_{0} & \equiv \tau_{0} \wedge \tau_{1} & & \text { by assumption } \tau_{0} \subseteq \tau_{1} \Leftrightarrow \tau_{0} \equiv \tau_{0} \wedge \tau_{1} \\
& \equiv \tau_{0} \wedge\left(\tau_{1} \wedge \tau_{2}\right) & & \text { by assumption } \tau_{1} \subseteq \tau_{2} \Leftrightarrow \tau_{1} \equiv \tau_{1} \wedge \tau_{2} \\
& \equiv\left(\tau_{0} \wedge \tau_{1}\right) \wedge \tau_{2} & & \text { by B-Assoc } \\
& \equiv \tau_{0} \wedge \tau_{2} & & \text { by assumption } \tau_{0} \subseteq \tau_{1} \Leftrightarrow \tau_{0} \equiv \tau_{0} \wedge \tau_{1}
\end{aligned}
$$

Contrary to full $\leqslant$-subtyping, $\subseteq$ only relates concrete type constructors (function, record, and nominal class tag types) in an obvious and syntactic way, making it easy to reason about. For example, notice that $\left\{x: \tau_{1}\right\} \subseteq\left\{y: \tau_{2}\right\}$ holds iff $x=y$ and $\tau_{1}=\tau_{2}$ (i.e., iff they are syntactically the same).

Definition B. 53 (Boolean algebra equivalence). We define ( $\cong$ ) as Boolean Algebra equivalence:
$$
\tau_{1} \cong \tau_{2} \Leftrightarrow \tau_{1} \subseteq \tau_{2} \text { and } \tau_{2} \subseteq \tau_{1}
$$

Remark: It is easy to show that $\tau_{1} \cong \tau_{1}^{\prime} \vee \tau_{2}$ implies $\tau_{2} \subseteq \tau_{1}$. Indeed, it implies $\tau_{1}^{\prime} \vee \tau_{2} \subseteq \tau_{1}$, which implies $\tau_{2} \subseteq \tau_{1}$. Similarly, $\tau_{1}^{\prime} \wedge \tau_{2} \cong \tau_{1}$ implies $\tau_{1} \subseteq \tau_{2}$.

Lemma B.54. If $\bigvee_{i} \tau_{i} \subseteq \bigwedge_{j} \pi_{j}$, then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$. Additionally, if $\bigvee_{i} \tau_{i}=\tau_{1}$ where $\tau_{1}$ is not an intersection; or if $\bigwedge_{j} \pi_{j}=\pi_{1}$ where $\pi_{1}$ is not a union, then the derivation for ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ has a size not larger than that of the assumption $\bigvee_{i} \tau_{i} \subseteq \bigwedge_{j} \pi_{j}$.

Proof. By induction on right-leaning $\subseteq$ derivations.

\section*{Case S-Refl.}

Case $\bigwedge_{j} \pi_{j}=\pi_{1}=\bigvee_{i} \tau_{i}$. By repeated applications of S-Trans with S-AndOr11•, followed by an application of S-Trans with S-AndOr12•, we have ${\overline{\tau_{i} \subseteq \bigvee_{i} \tau_{i}}}^{i}$, i.e., ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.

If $\bigvee_{i} \tau_{i}=\tau_{1}$ where $\tau_{1}$ is not an intersection, then $\bigvee_{i} \tau_{i}=\tau_{1}$. Then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau_{1} \subseteq \pi_{1}$, which is the assumption itself.

If $\bigwedge_{j} \pi_{j}=\pi_{1}$ where $\pi_{1}$ is not a union, then $\pi_{1}=\bigvee_{i} \tau_{i}$ is not a union, i.e., $\bigvee_{i} \tau_{i}=\tau_{1}$. Then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau_{1} \subseteq \pi_{1}$, which is the assumption itself.
Case $\bigvee_{i} \tau_{i}=\tau_{1}=\bigwedge_{j} \pi_{j}$. By repeated applications of S-Trans with S-AndOr11>, followed by an application of S-Trans with S-AndOr12d, we have $\bar{\bigwedge}_{j} \pi_{j} \subseteq \pi_{j}{ }^{j}$, i.e., ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.

If $\bigvee_{i} \tau_{i}=\tau_{1}$ where $\tau_{1}$ is not an intersection, then $\tau_{1}=\bigwedge_{j} \pi_{j}$ is not an intersection, i.e., $\bigwedge_{j} \pi_{j}=\pi_{1}$. Then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau_{1} \subseteq \pi_{1}$, which is the assumption itself.

If $\bigwedge_{j} \pi_{j}=\pi_{1}$ where $\pi_{1}$ is not a union, then $\bigwedge_{j} \pi_{j}=\pi_{1}$. Then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau_{1} \subseteq \pi_{1}$, which is the assumption itself.
Case S-ToB. $\bigwedge_{j} \pi_{j}=\top$. The result follows from S-ToB. on each of $\bar{\tau}_{i}{ }^{i}$.
Case S-ToB • $\bigvee_{i} \tau_{i}=\perp$. The result follows from S-ToB ↓ on each of ${\overline{\pi_{j}}}^{j}$.
Case S-Compl. $\bigvee_{i} \tau_{i}=\top$ and $\bigwedge_{j} \pi_{j}=\pi_{1}=\pi^{\prime} \vee \neg \pi^{\prime}$ for some $\pi^{\prime}$. The result follows immediately.
Case S-Compl. $\bigwedge_{j} \pi_{j}=\perp$ and $\bigvee_{i} \tau_{i}=\tau_{1}=\tau^{\prime} \wedge \neg \tau^{\prime}$ for some $\tau^{\prime}$. The result follows immediately.
Case S-NegInv. $\bigvee_{i} \tau_{i}=\tau_{1}=\neg \tau^{\prime}$ and $\bigwedge_{j} \pi_{j}=\pi_{1}=\neg \pi^{\prime}$ for some $\tau^{\prime}$ and $\pi^{\prime}$. The result follows immediately.
Case S-AndOr11. $\bigwedge_{j} \pi_{j}=\pi_{1}=\bigvee_{i} \tau_{i} \vee \pi^{\prime}$ for some $\pi^{\prime}$. By repeated applications of STrans with S-AndOr11•, followed by an application of S-Trans with S-AndOr12•, we have ${\overline{\tau_{i} \subseteq \bigvee_{i} \tau_{i} \vee \pi^{\prime}}}^{i}$, i.e., ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.

If $\bigvee_{i} \tau_{i}=\tau_{1}$ where $\tau_{1}$ is not an intersection, then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau \subseteq \tau \vee \pi^{\prime}$, which is the assumption itself.

It is impossible to have $\bigwedge_{j} \pi_{j}=\pi_{1}$ where $\pi_{1}$ is not a union since $\pi_{1}=\bigvee_{i} \tau_{i} \vee \pi^{\prime}$.
Case S-AndOr11>. $\bigvee_{i} \tau_{i}=\tau_{1}=\bigwedge_{j} \pi_{j} \wedge \tau^{\prime}$ for some $\tau^{\prime}$. By repeated applications of S-Trans with S-AndOr11>, followed by an application of S-Trans with S-AndOr12>, we have $\bar{\bigwedge}_{j} \pi_{j} \wedge \tau^{\prime} \subseteq \pi_{j}{ }^{j}$, i.e., ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.

It is impossible to have $\bigvee_{i} \tau_{i}=\tau_{1}$ where $\tau_{1}$ is not an intersection since $\pi_{1}=\bigvee_{i} \tau_{i} \vee \pi^{\prime}$.
If $\bigwedge_{j} \pi_{j}=\pi_{1}$ where $\pi_{1}$ is not a union, then ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$ is just $\tau \subseteq \tau \vee \pi^{\prime}$, which is the assumption itself.
Cases S-AndOr12 ↓ . Similar to the cases S-AndOr11 ◇.
Case S-AndOr2. Let the range of $i$ be $1 . . m$. We have $\bigvee_{i} \tau_{i}=\bigvee_{i \in 1 . . m-1} \tau_{i} \vee \tau_{m}$. The premises of the rule are $\bigvee_{i \in 1 . . m-1} \tau_{i} \subseteq \bigwedge_{j} \pi_{j}$ and $\tau_{m} \subseteq \bigwedge_{j} \pi_{j}$. By IH on the first premise, we have ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i \in 1 . . m-1, j}$. By IH on the second premise, we have ${\overline{\tau_{m} \subseteq \pi_{j}}}^{j}$. Then we have ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.
Case S-AndOr2 . Let the range of $j$ be $1 . . n$. We have $\bigwedge_{j} \pi_{j}=\bigwedge_{j \in 1 . . n-1} \pi_{j} \wedge \pi_{j}$. The premises of the rule are $\bigvee_{i} \tau_{i} \subseteq \bigwedge_{j \in 1 . . n-1} \pi_{j}$ and $\bigvee_{i} \tau_{i} \subseteq \pi_{n}$. By IH on the first premise, we have ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j \in 1 . . n-1}$. By IH on the second premise, we have ${\overline{\tau_{i} \subseteq \pi_{n}}}^{i}$. Then we have ${\overline{\tau_{i} \subseteq \pi_{j}}}^{i, j}$.
Case S-Distrib. $\bigvee_{i} \tau_{i}=\tau_{1}=\tau^{\prime} \wedge\left(\tau_{1}^{\prime} \vee \tau_{2}^{\prime}\right)$ and $\bigwedge_{j} \pi_{j}=\pi_{1}=\left(\tau^{\prime} \wedge \tau_{1}^{\prime}\right) \vee\left(\tau^{\prime} \wedge \tau_{2}^{\prime}\right)$ for some $\tau^{\prime}$ and $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. The result follows immediately.

Case S-Distrib ↓ $\bigwedge_{j} \pi_{j}=\pi_{1}=\tau^{\prime} \vee\left(\tau_{1}^{\prime} \wedge \tau_{2}^{\prime}\right)$ and $\bigvee_{i} \tau_{i}=\tau_{1}=\left(\tau^{\prime} \vee \tau_{1}^{\prime}\right) \wedge\left(\tau^{\prime} \vee \tau_{2}^{\prime}\right)$ for some $\tau^{\prime}$ and $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. The result follows immediately.
Case S-Trans. The premises of the rule are $\bigvee_{i} \tau_{i} \subseteq \tau^{\prime}$ and $\tau^{\prime} \subseteq \bigwedge_{j} \pi_{j}$ for some $\tau^{\prime}$. By IH on the former premise, we have ${\overline{\tau_{i} \subseteq \tau^{\prime}}}^{i}$. By IH on the latter premise, we have ${\overline{\tau^{\prime} \subseteq \pi_{j}}}^{j}$. The result follows from S-Trans on each of ${\overline{\tau_{i} \subseteq \tau^{\prime}}}^{i}$ with each of ${\overline{\tau_{i} \subseteq \tau^{\prime}}}^{i}$. \(\square\)

\section*{B. 9 Elementary type forms}

\section*{B.9.1 Definition.}

Definition B. 55 (Constructors and negated constructors). The syntax of constructors and negated constructors is presented in Figure 11.
$$
\begin{aligned}
B & ::=\rightarrow|x| \# C|\perp| \top \\
C, D & ::=B \mid B \\
\text { Notation: } & \ell= \begin{cases}B & \text { if } C=B \\
B & \text { if } C=B\end{cases}
\end{aligned}
$$

Fig. 11. Syntax of constructor and negated constructor.

Definition B. 56 (Elementary type forms). The "elementary" type forms are defined in Figure 12. These are conceptually the type forms we need to care about for the system to be sound.

Elementary union types $U^{C}, V^{C}$
$$
\begin{aligned}
U^{\rightarrow} & ::=\tau_{1} \rightarrow \pi_{1} \vee \cdots \vee \tau_{n} \rightarrow \pi_{n} \\
U^{x} & ::=\left\{x: \tau_{1}\right\} \vee \cdots \vee\left\{x: \tau_{n}\right\} \\
U^{\# C} & ::=\# C \\
U^{\top} & ::=\top \mid\left\{x_{1}: \tau_{1}\right\} \vee\left\{x_{2}: \tau_{2}\right\} \quad\left(\text { where } x_{1} \neq x_{2}\right) \\
& \mid\left\{x_{1}: \tau_{1}\right\} \vee(\tau \rightarrow \pi) \\
U^{B} & ::=\neg X^{B}
\end{aligned}
$$

\section*{Elementary intersection types $\quad X^{C}, Y^{C}$}
$X \rightarrow::=\left(\tau_{1} \rightarrow \pi_{1}\right) \wedge \cdots \wedge\left(\tau_{n} \rightarrow \pi_{n}\right)$
$X^{X}::=\left\{x: \tau_{1}\right\} \wedge \cdots \wedge\left\{x: \tau_{n}\right\}$
$X^{\# C}::=\# C$
$X^{\perp}::=\perp \mid \# C_{1} \wedge \# C_{2}$ (where $C_{1}$ and $C_{2}$ are unrelated)
$X^{B}::=\neg U^{B}$

Fig. 12. Elementary type form definition.

Lemma B. 57 (Inversion of negated elementary types).
(A) For all $C$ and $U^{C}$, we have $\neg^{C} \cong X^{\varnothing}$ for some $X^{\varnothing}$.
(B) For all $C$ and $X^{C}$, we have $\neg X^{C} \cong U^{\ell}$ for some $U^{\ell}$.

Proof. By case analysis on $C$.
(A) If $C=B$ for some B , then pick $X^{\bar{\ell}}=X^{B}=\neg U^{B}=\neg U^{C}$. If $C=B$ for some $B$, then $U^{C}=U^{B}=\neg X^{B}$ by the definition of $U^{B}$, so $\neg U^{C}=\neg \neg X^{B} \cong X^{B}=X^{\ell}$.
(B) If $C=B$ for some $B$, then pick $U^{\ell}=U^{B}=\neg X^{B}=\neg X^{C}$. If $C=B$ for some $B$, then $X^{C}=X^{B}=\neg U^{B}$ by the definition of $X^{B}$, so $\neg X^{C}=\neg \neg U^{B} \cong U^{B}=U^{\ell}$.

Definition B. 58 (Helper pseudo-subtyping relation). The rules of the helper pseudo-subtyping relation are defined in Figure 13. It is easy to show that $\leq$ implies $\leqslant$.
$$
\begin{aligned}
& \Sigma \vdash \bigwedge_{i} U_{i}^{C} \leq V^{D} \\
& \Sigma \vdash X^{C} \leq \bigvee_{i} Y_{i}^{D} \\
& \frac{\Sigma \vdash \bigwedge_{i} V_{i}^{D} \leq U^{C}}{\Sigma \vdash X^{\ell} \leq \bigvee_{i} Y_{i}^{\varnothing}} \quad \frac{\triangleleft \Sigma \vdash \tau^{\prime} \leqslant \tau \quad \triangleleft \Sigma \vdash \pi \leqslant \pi^{\prime}}{\Sigma \vdash \tau \rightarrow \pi \leq \tau^{\prime} \rightarrow \pi^{\prime}} \quad \frac{\Sigma \vdash U^{C} \leq\left(\bigwedge_{i} \tau_{i}\right) \rightarrow\left(\bigvee_{i} \pi_{i}\right)}{\Sigma \vdash U^{C} \leq \bigvee_{i} \tau_{i} \rightarrow \pi_{i}} \\
& \frac{\Sigma \vdash\left(\bigvee_{i} \tau_{i}\right) \rightarrow\left(\bigwedge_{i} \pi_{i}\right) \leq Y^{C}}{\Sigma \vdash \bigwedge_{i} \tau_{i} \rightarrow \pi_{i} \leq Y^{C}} \\
& \frac{\Sigma \vdash\left(\bigvee_{i} \bigwedge_{j} \tau_{i j}\right) \rightarrow\left(\bigwedge_{i} \bigvee_{j} \pi_{i j}\right) \leq U^{C}}{\Sigma \vdash \bigwedge_{i} \bigvee_{j} \tau_{i j} \rightarrow \pi_{i j} \leq U^{C}} \\
& \frac{\Sigma \vdash X^{C} \leq\left(\bigwedge_{i} \bigvee_{j} \tau_{i j}\right) \rightarrow\left(\bigvee_{i} \bigwedge_{j} \pi_{i j}\right)}{\Sigma \vdash X^{C} \leq \bigvee_{i} \bigwedge_{j} \tau_{i j} \rightarrow \pi_{i j}} \quad \frac{\triangleleft \Sigma \vdash \tau \leqslant \tau^{\prime}}{\Sigma \vdash\{x: \tau\} \leq\left\{x: \tau^{\prime}\right\}} \quad \frac{\Sigma \vdash U^{C} \leq\left\{x: \bigvee_{i} \tau_{i}\right\}}{\Sigma \vdash U^{C} \leq \bigvee_{i}\left\{x: \tau_{i}\right\}} \\
& \frac{\Sigma \vdash\left\{x: \bigwedge_{i} \tau_{i}\right\} \leq Y^{C}}{\Sigma \vdash \bigwedge_{i}\left\{x: \tau_{i}\right\} \leq Y^{C}} \quad \frac{\Sigma \vdash\left\{x: \bigwedge_{i} \bigvee_{j} \tau_{i j}\right\} \leq U^{C}}{\Sigma \vdash \bigwedge_{i} \bigvee_{j}\left\{x: \tau_{i j}\right\} \leq U^{C}} \quad \frac{\Sigma \vdash X^{C} \leq\left\{x: \bigvee_{i} \bigwedge_{j} \tau_{i j}\right\}}{\Sigma \vdash X^{C} \leq \bigvee_{i} \bigwedge_{j}\left\{x: \tau_{i j}\right\}} \\
& \frac{C_{2} \in \mathcal{S}\left(\# C_{1}\right)}{\Sigma \vdash \bigwedge_{i} \# C_{1} \leq \# C_{2}} \quad \frac{C_{2} \in \mathcal{S}\left(\# C_{1}\right)}{\Sigma \vdash \# C_{1} \leq \bigvee_{i} \# C_{2}} \quad \frac{x \neq y}{\Sigma \vdash \bigwedge_{i} U_{i}^{\chi} \leq V^{y}} \quad \frac{}{\Sigma \vdash \bigwedge_{i} U_{i}^{\chi} \leq V^{\rightarrow}} \\
& \overline{\Sigma \vdash \bigwedge_{i} U_{i}^{\widehat{r}} \leq V^{x}} \quad \frac{C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \quad C_{2} \notin \mathcal{S}\left(\# C_{1}\right)}{\Sigma \vdash X^{\# C_{1}} \leq \bigvee_{i} Y^{\# \ell_{2}}}
\end{aligned}
$$

Fig. 13. Helper pseudo-subtyping relation rules.

\section*{B.9.2 Some useful lemmas.}

Lemma B.59.
(A) If $\bigwedge_{i} U_{i}^{C} \leq V^{D}$, then either one of the following is true:
- $D \in\{C, \top, \nmid\}$
- $C=\# C_{1}$ and $D=\# C_{2}$ and $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$
- $C=\# \ell_{1}$ and $D=\# \ell_{2}$ and $C_{1} \in \mathcal{S}\left(\# C_{2}\right)$
- $C=\chi$ and $D=y \neq x$
- $C=\chi$ and $D=\rightarrow$
- $C=\longrightarrow$ and $D=x$
- $C=\# C_{1}$ and $D=\# \ell_{2}$ and $C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$ and $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$
(B) If $X^{C} \leq \bigvee_{i} Y_{i}^{D}$, then either one of the following is true:
- $C \in\{D, \perp, \bar{X}\}$
- $D=\# C_{1}$ and $C=\# C_{2}$ and $C_{1} \in \mathcal{S}\left(\# C_{2}\right)$
- $D=\# \ell_{1}$ and $C=\# \ell_{2}$ and $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$
- $D=x$ and $C=y \neq x$
- $D=\rightarrow$ and $C=\chi$
- $D=x$ and $C=\longrightarrow$
- $D=\# \ell_{1}$ and $C=\# C_{2}$ and $C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$ and $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$

Proof. By straightforward induction on $\leq$ rules.
Lemma B.60. For $\tau \in\left\{\tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$,
(A) If $U^{C} \subseteq \tau$, then $U^{C}=\bigvee_{i} \tau$.
(B) If $\tau \subseteq X^{C}$, then $X^{C}=\bigwedge_{i} \tau$.

Proof.
(A) By induction on right-leaning $\subseteq$ derivations. We only consider rules that can syntactically apply. Denote the size of the current derivation as $n$.
Case S-Refl. Immediate.
Case S-AndOr2. . $U^{C}=U_{1}^{C_{1}} \vee U_{2}^{C_{2}}$ for some $U_{1}^{C_{1}}$ and $U_{2}^{C_{2}}$, where $U_{2}^{C_{2}}$ is not a union. The premises of the rule are $U_{1}^{C_{1}} \subseteq \tau$ and $U_{2}^{C_{2}} \subseteq \tau$. By IH, we have $U_{1}^{C_{1}}=\bigvee_{k} \tau$ and $U_{2}^{C_{2}}=\bigvee_{l} \tau$. Since $U_{2}^{C_{2}}$ is not a union, $U_{2}^{C_{2}}=\tau$. Then $U^{C}=U_{1}^{C_{1}} \vee U_{2}^{C_{2}}=\bigvee_{k} \tau \vee \tau$.
Case S-Trans. Then the premises are $U^{C} \subseteq \tau^{\prime}$ and $\tau^{\prime} \subseteq \tau$ for some $\tau^{\prime}$, both of size $n-1$. By induction on the size of the subderivation for the former premise, denoted by $m$. Denote the inner induction hypothesis as $\mathrm{IH}^{\prime}$.
Cases (S-Refl, *), (*, S-Refl). By IH on the other premise.
Cases (S-ToB., *). Then $\tau^{\prime}=\top$. The latter premise is $\top \subseteq \tau$, which is impossible by Lemma B.87. Therefore this case is impossible.
Cases (S-Compl $\cdot$, *). Then $U^{C}=\top$. The conclusion is $\mathrm{T} \subseteq \tau$, which is impossible by Lemma B.87. Therefore this case is impossible.
Cases (S-AndOr11, *). Then $\tau^{\prime}=U^{C} \vee \tau_{1}^{\prime}$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $U^{C} \subseteq \tau$ with a derivation of size at most $n-1$. The result then follows from IH .
Cases (S-AndOr12, *). Then $\tau^{\prime}=\tau_{1}^{\prime} \vee U^{C}$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $U^{C} \subseteq \tau$ with a derivation of size at most $n-1$. The result then follows from IH .
Cases (S-AndOr2., *). Then $U^{C}=U_{1}^{C_{1}} \vee U_{2}^{C_{2}}$ for some $U_{1}^{C_{1}}$ and $U_{2}^{C_{2}}$, where $U_{2}^{C_{2}}$ is not a union. The premises of the former rule are $U_{1}^{C_{1}} \subseteq \tau^{\prime}$ and $U_{2}^{C_{2}} \subseteq \tau^{\prime}$, both of size $m-1$. By S-Trans with $\tau^{\prime} \subseteq \tau$, we have $U_{1}^{C_{1}} \subseteq \tau$ and $U_{2}^{C_{2}} \subseteq \tau$, both of size $n$ with a former premise of size $m-1$. Then by $\mathrm{IH}^{\prime}$, we have $U_{1}^{C_{1}} \cong \tau$ and $U_{2}^{C_{2}} \cong \tau$, which imply $U_{1}^{C_{1}} \vee U_{2}^{C_{2}} \cong \tau$.
Cases (S-AndOr2 , *). Then $\tau^{\prime}=\tau_{1}^{\prime} \wedge \tau_{2}^{\prime}$ for some $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. The premises of the former rule are $U^{C} \subseteq \tau_{1}^{\prime}$ and $U^{C} \subseteq \tau_{2}^{\prime}$, both of size $m-1$. By Lemma B. 82 on the latter premise, we have $\tau_{l}^{\prime} \subseteq \tau$ of size at most $n-1$ for some $l \in\{1,2\}$. By S-Trans on $U^{C} \subseteq \tau_{l}^{\prime}$ and
$\tau_{l}^{\prime} \subseteq \tau$, we have $U^{C} \subseteq \tau$ of size $n$ with a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}$.
(B) By induction on right-leaning $\subseteq$ derivations. We only consider rules that can syntactically apply. Denote the size of the current derivation as $n$.
Case S-Refl. Immediate.
Case S-AndOr2 . $X^{C}=X_{1}^{C_{1}} \wedge X_{2}^{C_{2}}$ for some $X_{1}^{C_{1}}$ and $X_{2}^{C_{2}}$, where $X_{2}^{C_{2}}$ is not a intersection. The premises of the rule are $\tau \subseteq X_{1}^{C_{1}}$ and $\tau \subseteq X_{2}^{C_{2}}$. By IH, we have $X_{1}^{C_{1}}=\bigwedge_{k} \tau$ and $X_{2}^{C_{2}}=\bigwedge_{l} \tau$. Since $X_{2}^{C_{2}}$ is not a intersection, $X_{2}^{C_{2}}=\tau$. Then $X^{C}=X_{1}^{C_{1}} \wedge X_{2}^{C_{2}}=\bigwedge_{k} \tau \wedge \tau$.
Case S-Trans. Then the premises are $\tau \subseteq \tau^{\prime}$ and $\tau^{\prime} \subseteq X^{C}$ for some $\tau^{\prime}$, both of size $n-1$. By induction on the size of the subderivation of the former premise, denoted by $m$. Denote the inner induction hypothesis as $\mathrm{IH}^{\prime}$.
Cases (S-Refl, *), (*, S-Refl). By IH on the other premise.
Cases (S-ToB., *). Then $\tau^{\prime}=\top$. The latter premise is $\dagger \subseteq X^{C}$, which implies $\dagger \subseteq X_{2}^{C_{2}}$ for some $X_{2}^{C_{2}} \in\left\{\pi_{1} \rightarrow \pi_{2},\left\{x^{\prime}: \pi_{1}\right\}, \# C^{\prime}\right\}$ by Lemma B.54, where $X^{C}=X_{1}^{C_{1}} \wedge X_{2}^{C_{2}}$, which is impossible by Lemma B.87. Therefore this case is impossible.
Cases (S-AndOr11 ⋅ , *). Then $\tau^{\prime}=\tau \vee \tau_{1}^{\prime}$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $\tau \subseteq X^{C}$ with a derivation of size at most $n-1$. The result then follows from IH .
Cases (S-AndOr12, *). Then $\tau^{\prime}=\tau_{1}^{\prime} \vee \tau$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $\tau \subseteq X^{C}$ with a derivation of size at most $n-1$. The result then follows from IH.
Cases (S-AndOr2 , *). Then $\tau^{\prime}=\tau_{1}^{\prime} \wedge \tau_{2}^{\prime}$ for some $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. The premises of the former rule are $\tau \subseteq \tau_{1}^{\prime}$ and $\tau \subseteq \tau_{2}^{\prime}$, both of size $m-1$. By Lemma B. 54 on the latter premise, we have $\overline{\tau_{1}^{\prime} \wedge \tau_{2}^{\prime} \subseteq X_{i}^{C_{i}}}{ }^{i}$, where $X^{C}=\bigwedge_{i} X_{i}^{C_{i}}$ and ${\overline{X_{i}^{C_{i}}}}^{i}$ are not intersections, each of size at most $n-1$. Then by Lemma B.82, we have ${\overline{\tau_{l_{i}}^{\prime} \subseteq X_{i}^{C_{i}}}}^{i}$ for some ${\overline{l_{i} \in\{1,2\}}}^{i}$, each of size at most $n-1$. By S-Trans on $\tau \subseteq \tau_{l}^{\prime}$ and $\tau_{l}^{\prime} \subseteq X_{i}^{C_{i}}$, we have $\overline{\tau \subseteq X_{i}^{C_{i}}}{ }^{i}$, each of size $n$ with a former premise of size $m-1$. Then ${\overline{X_{i}^{C_{i}}=\tau}}^{i}$ by $\mathrm{IH}^{\prime}$ (note that ${\overline{X_{i}^{C_{i}}}}^{i}$ are not intersections), i.e., $X^{C}=\bigwedge_{i} \tau$.

Corollary B.61. For $\tau \in\left\{\tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$,
(A) If $U^{C} \subseteq \neg \tau$, then $U^{C}=\bigvee_{i} \neg \tau$.
(B) If $\neg \tau \subseteq X^{C}$, then $X^{C}=\bigwedge_{i} \neg \tau$.

Proof.
(A) We have $U^{C}=\bigvee_{i} U_{i}^{C_{i}}$ for some ${\overline{U_{i}^{C_{i}}}}^{i}$, where ${\overline{U_{i}^{C_{i}}}}^{i}$ are not unions. Then by S-NegInv, Theorem B.12, Theorem B.13, and Theorem B.19, we have $\tau \subseteq \bigwedge_{i} U_{i}^{\not / i}$, which implies $\bigwedge_{i} U_{i}^{\mathscr{Y}_{i}}=\bigwedge_{i} \tau$ by Lemma B.60, i.e., $\overline{U_{i}^{\mathscr{Y}_{i}}}=\tau$. Then we have $U^{C}=\bigvee_{i} \neg \tau$.
(B) We have $X^{C}=\bigwedge_{i} X_{i}^{C_{i}}$ for some ${\overline{X_{i}^{C_{i}}}}^{i}$, where ${\overline{X_{i}^{C_{i}}}}^{i}$ are not intersections. Then by S-NegInv, Theorem B.12, Theorem B.13, and Theorem B.19, we have $\bigvee_{i} X_{i}^{/ i} \subseteq \tau$, which implies $\bigvee_{i} X_{i}^{\not / i}=\bigvee_{i} \tau$ by Lemma B.60, i.e., $\overline{X_{i}^{\not / i}}=\tau$. Then we have $X^{C}=\bigwedge_{i} \neg \tau$.

Lemma B.62.
(A) If $\top \leqslant \tau$, then $U^{C} \subseteq \tau$ for some $U^{C}$ and $C \in\{\top, \nvdash\}$.
(B) If $\tau \leqslant \perp$, then $\tau \subseteq X^{C}$ for some $X^{C}$ and $C \in\{\perp, \not X\}$.

Proof. By straightforward induction on subtyping derivations.

\section*{B. 10 CDN- and DCN-normalized type forms and derivations}

Since the intersection, union, and negation connectives can freely nest within and intertwine with each other, they introduce significant difficulty for the proof of subtyping consistency. We introduce the CDN- and DCN-normalized forms to order them one after the other, using only the Boolean-algebraic relation, i.e., not normalizing deeply under constructors as in RDNF.

We also present alternative sets of subtyping rules where only the respective normalized forms appear in the top level, and show that any subtyping derivations can be translated into a normalized one. Thus we can prove any property by induction on normalized derivations.

\section*{B.10.1 CDN-normalized type forms and derivations.}

Definition B. 63 (CDN-normalized form). The syntax of CDN-normalized (conjunction-disjunctionnegation) form is presented in Figure 14. We say that a CDN-normalized form $\tau^{\mathrm{cdn}}$ is complement-free if $\tau^{\mathrm{cdn}}=\bigwedge_{i} \bigvee_{j \in 1 . . n_{i}} \tau_{i j}^{\mathrm{n}}$, where $\forall{\overline{j_{i} \in 1 . . n_{i}}}^{i} . \bigwedge_{i} \tau_{i j_{i}}^{\mathrm{n}} \nsubseteq \perp$.
$$
\begin{aligned}
\tau^{0} & ::=\tau \rightarrow \tau|\{x: \tau\}| \# C|\alpha| \top \\
\tau^{\mathrm{n}} & ::=\tau^{0} \mid \neg \tau^{0} \\
\tau^{\mathrm{dn}} & ::=\tau^{\mathrm{n}} \mid \tau^{\mathrm{n}} \vee \tau^{\mathrm{dn}} \\
\tau^{\mathrm{cdn}} & ::=\tau^{\mathrm{dn}} \mid \tau^{\mathrm{dn}} \wedge \tau^{\mathrm{cdn}}
\end{aligned}
$$

Fig. 14. Syntax of CDN-normalized form.
In the proofs below, we sometimes abuse the notations $\tau_{1}^{\mathrm{dn}} \vee \tau_{2}^{\mathrm{dn}}$ and $\tau_{1}^{\mathrm{cdn}} \wedge \tau_{2}^{\mathrm{cdn}}$ to mean their properly associated versions, i.e., $\operatorname{dis}\left(\tau_{1}^{\mathrm{dn}}, \tau_{2}^{\mathrm{dn}}\right)$ and $\operatorname{con}\left(\tau_{1}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right)$ in Figure 16 respectively.

Definition B. 64 (CDN-normalized derivations). The CDN-normalized subtyping relation $\leqslant^{\mathrm{cdn}}$ is defined in Figure 15. The following are the difference compared to the full subtyping relation $\leqslant$ in Figure 4:
- On the top level, the relation is restricted to $\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant \tau^{\mathrm{cdn}}$.
- On the top level, all occurrences of ⟂ are replaced with ✓ ✓.
- The rule S-Distrib◇ is replaced by S-DistribCdn∘, which requires an application of SDistrib◇ to be followed immediately by an application of S-AndOr2• in a transitivity chain by merging the two rules into one.
- The negated-inverted versions of the algebraic rules are added.

Notice that the premises of S-FunDepth and S-RcdDepth still refer to the full $\leqslant$ relation, even though their conclusions are about the $\leqslant^{\mathrm{cdn}}$ relation.

The CDN-normalized boolean subtyping relation $\subseteq^{\mathrm{cdn}}$ is defined similarly.
Notice that Lemma B. 21 and Lemma B. 22 extend to CDN-normalized derivations. In the proofs below, we also make use of extended versions of commutativity $\left(\tau_{1} \vee^{\diamond} \tau_{2}\left(v^{\diamond} \tau_{3}\right) \leqslant^{\mathrm{cdn}} \tau_{2} v^{\diamond} \tau_{1}\left(v^{\diamond} \tau_{3}\right)\right)$ and idempotence $\left(\tau_{1} \vee^{\diamond} \tau_{1}\left(\vee^{\diamond} \tau_{2}\right) \leqslant^{\mathrm{cdn}} \tau_{1}\left(\vee^{\diamond} \tau_{2}\right)\right)$.

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-076.jpg?height=1486&width=1392&top_left_y=300&top_left_x=148}
\captionsetup{labelformat=empty}
\caption{Fig. 15. CDN-normalized subtyping rules.}
\end{figure}

Definition B. 65 (CDN-normalized form translation). The translation from arbitrary types into CDN-normalized types $\operatorname{cdn}(\cdot)$ is defined in Figure 16.

Lemma B.66. $\Sigma \vdash \tau_{1}^{\mathrm{cdn}} \leqslant \tau_{2}^{\mathrm{cdn}}$ if $\Sigma \vdash \tau_{1}^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau_{2}^{\mathrm{cdn}}$. Similarly, $\tau_{1}^{\mathrm{cdn}} \subseteq \tau_{2}^{\mathrm{cdn}}$ if $\tau_{1}^{\mathrm{cdn}} \subseteq^{\mathrm{cdn}} \tau_{2}^{\mathrm{cdn}}$.
Proof. It is easy to see that every rule of $\leqslant^{\mathrm{cdn}}$ is admissible in $\leqslant$. \(\square\)

Lemma B.67. For any $\tau, \operatorname{cdn}(\tau) \cong \tau$.
Proof. By straightforward induction. \(\square\)
$$
\begin{aligned}
\operatorname{cdn}(\tau) & : \tau^{\mathrm{cdn}} \\
\operatorname{cdn}\left(\tau^{0}\right) & =\tau^{0} \\
\operatorname{cdn}(\perp) & =\neg \top \\
\operatorname{cdn}(\neg \tau) & =\operatorname{neg}(\operatorname{cdn}(\tau)) \\
\operatorname{cdn}\left(\tau_{1} \vee \tau_{2}\right) & =\operatorname{dis}\left(\operatorname{cdn}\left(\tau_{1}\right), \operatorname{cdn}\left(\tau_{2}\right)\right) \\
\operatorname{cdn}\left(\tau_{1} \wedge \tau_{2}\right) & =\operatorname{con}\left(\operatorname{cdn}\left(\tau_{1}\right), \operatorname{cdn}\left(\tau_{2}\right)\right) \\
\operatorname{neg}\left(\tau^{\mathrm{cdn}}\right) & : \tau^{\mathrm{cdn}} \\
\operatorname{neg}\left(\tau^{0}\right) & =\neg \tau^{0} \\
\operatorname{neg}\left(\neg \tau^{0}\right) & =\tau^{0} \\
\operatorname{neg}\left(\tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}\right) & =\operatorname{con}\left(\operatorname{neg}\left(\tau_{1}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau_{2}^{\mathrm{dn}}\right)\right) \\
\operatorname{neg}\left(\tau_{1}^{\mathrm{dn}} \wedge \tau_{2}^{\mathrm{cdn}}\right) & =\operatorname{dis}\left(\operatorname{neg}\left(\tau_{1}^{\mathrm{dn}}\right), \operatorname{neg}\left(\tau_{2}^{\mathrm{cdn}}\right)\right) \\
\operatorname{dis}\left(\tau^{\mathrm{cdn}}, \tau^{\mathrm{cdn}}\right): & : \tau^{\mathrm{cdn}} \\
\operatorname{dis}\left(\tau_{11}^{\mathrm{dn}} \wedge \tau_{12}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right) & =\operatorname{con}\left(\operatorname{dis}\left(\tau_{11}^{\mathrm{dn}}, \tau_{2}^{\mathrm{cdn}}\right), \operatorname{dis}\left(\tau_{12}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right)\right) \\
\operatorname{dis}\left(\tau_{11}^{\mathrm{n}} \vee \tau_{12}^{\mathrm{dn}}, \tau_{2}^{\mathrm{cdn}}\right) & =\operatorname{dis}\left(\tau_{11}^{\mathrm{n}}, \operatorname{dis}\left(\tau_{12}^{\mathrm{dn}}, \tau_{2}^{\mathrm{cdn}}\right)\right) \\
\operatorname{dis}\left(\tau_{1}^{\mathrm{n}}, \tau_{21}^{\mathrm{dn}} \wedge \tau_{22}^{\mathrm{cdn}}\right) & =\operatorname{con}\left(\operatorname{dis}\left(\tau_{1}^{\mathrm{n}}, \tau_{21}^{\mathrm{dn}}\right), \operatorname{dis}\left(\tau_{1}^{\mathrm{n}}, \tau_{22}^{\mathrm{cdn}}\right)\right) \\
\operatorname{dis}\left(\tau_{1}^{\mathrm{n}}, \tau_{2}^{\mathrm{dn}}\right) & =\tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}} \\
\operatorname{Dis}{ }_{i \in m . . n} \tau_{i}^{\mathrm{cdn}} & =\operatorname{dis}\left(\tau_{m}^{\mathrm{cdn}}, \operatorname{Dis} i \in m+1 . . n \tau_{i}^{\mathrm{cdn}}\right) \\
\operatorname{Dis}{ }_{i \in n . . n} \tau_{i}^{\mathrm{cdn}} & =\tau_{n}^{\mathrm{cdn}} \\
\operatorname{con}\left(\tau^{\mathrm{cdn}}, \tau^{\mathrm{cdn}}\right) & : \tau^{\mathrm{cdn}} \\
\operatorname{con}\left(\tau_{11}^{\mathrm{dn}} \wedge \tau_{12}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right) & =\operatorname{con}\left(\tau_{11}^{\mathrm{dn}}, \operatorname{con}\left(\tau_{12}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right)\right) \\
\operatorname{con}\left(\tau_{1}^{\mathrm{dn}}, \tau_{2}^{\mathrm{cdn}}\right) & =\tau_{1}^{\mathrm{dn}} \wedge \tau_{2}^{\mathrm{cdn}} \\
\operatorname{Con}{ }_{i \in m . . n} \tau_{i}^{\mathrm{cdn}} & =\operatorname{con}\left(\tau_{m}^{\mathrm{cdn}}, \operatorname{Con}{ }_{i \in m+1 . . n} \tau_{i}^{\mathrm{cdn}}\right) \\
\operatorname{Con}{ }_{i \in n . . n} \tau_{i}^{\mathrm{cdn}} & =\tau_{n}^{\mathrm{cdn}}
\end{aligned}
$$

Fig. 16. CDN-normalized form translation

Definition B. 68 (CDN-normalized subtyping context). $\Sigma$ is CDN-normalized if for all $H \in \Sigma$, either one of the following is true:
(1) $H=\left(\top \leqslant \bigvee_{i} \tau_{i}^{n}\right)$, where $\forall \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
(2) $H=\left(\alpha \leqslant \bigvee_{i} \tau_{i}^{\mathrm{n}}\right)$, where the following are true:
- $\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \neg \beta \notin\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \exists\left(\bigwedge_{j} \pi_{j}^{\mathrm{n}} \leqslant \beta\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \beta}, \alpha\right\}$;
- $\forall \neg \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\beta \leqslant \bigvee_{j} \pi_{j}^{\mathrm{n}}\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \beta}, \neg \alpha\right\}$;
(3) $H=\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \alpha\right)$, where the following are true:
- $\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \neg \beta \notin\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \exists\left(\beta \leqslant \bigvee_{j} \pi_{j}^{\mathrm{n}}\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \beta}, \alpha\right\}$;
- $\forall \neg \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\bigwedge_{j} \pi_{j}^{\mathrm{n}} \leqslant \beta\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \beta}, \neg \alpha\right\}$;

Definition B. 69 (CDN-normalized subtyping context translation). The translation from arbitrary subtyping contexts into CDN-normalized subtyping contexts $\operatorname{cdn}(\cdot)$ is defined in Figure 17.
$$
\begin{gathered}
\operatorname{cdn}(\Sigma): \Sigma \\
\operatorname{cdn}(\Sigma)=\overline{\operatorname{cdn}(\top \leqslant \operatorname{cdn}(\neg \tau \vee \pi))}(\tau \leqslant \pi) \in \Sigma . \overline{\triangleright H} \triangleright H \in \Sigma \\
\operatorname{cdn}\left(\top \leqslant \tau^{\operatorname{cdn}}\right): \Sigma \\
\operatorname{cdn}\left(\top \leqslant \bigwedge_{i} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right)=\overline{\operatorname{cdn}\left(\top \leqslant \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right)} i \\
\operatorname{cdn}\left(\top \leqslant \bigvee_{i} \tau_{i}^{\mathrm{n}}\right)=\left\{\begin{array}{c}
\epsilon \quad \text { if } \exists \alpha .\{\alpha, \neg \alpha\} \subseteq\left\{\overline{\bar{\tau}_{i}^{\mathrm{n}}}\right\} \\
\overline{\left(\bigwedge_{i \mid \tau_{i}^{\mathrm{n}} \neq \alpha} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right) \leqslant \alpha\right)} \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \overline{\left(\alpha \leqslant \bigvee_{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \alpha} \tau_{i}^{\mathrm{n}}\right)} \alpha \mid \neg \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \\
\text { if }\left(\exists \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \neq \varnothing\right) \text { and }\left(\forall \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \neg \alpha \notin\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\}\right) \\
\left(\top \leqslant \bigvee_{i} \tau_{i}^{\mathrm{n}}\right) \quad \text { if } \forall \alpha .\{\alpha, \neg \alpha\} \cap\left\{\overline{\tau_{i}^{\mathrm{n}}}\right\}=\varnothing
\end{array}\right.
\end{gathered}
$$

Fig. 17. CDN-normalized subtyping context translation

Lemma B.70. For any $\Sigma$, we have $\Sigma \models \operatorname{cdn}(\Sigma)$ and $\operatorname{cdn}(\Sigma) \models \Sigma$.
Proof. Straightforward, notably making use of Theorem B. 20 and Lemma B.67.
Lemma B.71. If $\Sigma \vdash \tau \leqslant \pi$, then $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$. Similarly, if $\tau \subseteq \pi$, then $\operatorname{cdn}(\tau) \subseteq^{\operatorname{cdn}} \operatorname{cdn}(\pi)$.

Proof. By induction on unassuming subtyping derivations.
Case S-Refl. Then $\tau=\pi$, which implies $\operatorname{cdn}(\tau)=\operatorname{cdn}(\pi)$. Then we have $\operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$ by S-cdn.
Case S-ToB. Then $\pi=\top$ and $\operatorname{cdn}(\pi)=\top$. Then we have $\operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \top$ by S-ToB.
Case S-ToB . Then $\tau=\perp$ and $\operatorname{cdn}(\tau)=\neg$. Then we have $\neg \top \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$ by S-ToB .
Case S-Compl. Then $\tau=\top$ and $\pi=\pi^{\prime} \vee \neg \pi^{\prime}$ for some $\pi^{\prime}$. Let $\operatorname{cdn}\left(\pi^{\prime}\right)=\bigwedge_{i \in 1 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \pi_{i j_{i}}^{\mathrm{n}}$. Then $\operatorname{cdn}\left(\neg \pi^{\prime}\right)=\operatorname{neg}\left(\operatorname{cdn}\left(\pi^{\prime}\right)\right)=\bigwedge_{\overline{j_{i^{\prime}} \in 1 . . n_{i^{\prime}}}{ }^{\prime} \in 1 . . m} \bigvee_{i \in 1 . . m} \operatorname{neg}\left(\pi_{i j_{i}}^{\mathrm{n}}\right)$. Then $\operatorname{cdn}\left(\pi^{\prime} \vee \neg \pi^{\prime}\right)= \operatorname{dis}\left(\operatorname{cdn}(\pi), \operatorname{cdn}\left(\pi^{\prime}\right)\right)=\bigwedge_{i \in 1 . . m, \overline{j_{i^{\prime}} \in 1 . . n_{i^{\prime}}} i^{\prime} \in 1 . . m}\left(\bigvee_{j_{i}^{\prime} \in 1 . . n_{i}} \pi_{i j_{i}^{\prime}}^{\mathrm{n}} \vee \bigvee_{i^{\prime} \in 1 . . m} \operatorname{neg}\left(\pi_{i^{\prime} j_{i^{\prime}}}^{\mathrm{n}}\right)\right)$. For each $i,{\overline{j_{i^{\prime}}}}^{i^{\prime} \in 1 . . m}, \bigvee_{j_{i}^{\prime} \in 1 . . n_{i}} \pi_{i j_{i}^{\prime}}^{\mathrm{n}}$ contains the disjunct $\pi_{i j_{i}}^{\mathrm{n}}$, and $\bigvee_{i^{\prime} \in 1 . . m} \operatorname{neg}\left(\pi_{i^{\prime} j_{i^{\prime}}}^{\mathrm{n}}\right)$ contains the disjunct $\operatorname{neg}\left(\pi_{i j_{i}}^{\mathrm{n}}\right)$. Then by commutativity, we have $\bigvee_{j_{i}^{\prime} \in 1 . . n_{i}} \pi_{i j_{i}^{\prime}}^{\mathrm{n}} \vee \bigvee_{i^{\prime} \in 1 . . m} \operatorname{neg}\left(\pi_{i^{\prime} j_{i^{\prime}}}^{\mathrm{n}}\right) \geqslant^{\mathrm{cdn}} \bigvee_{j_{i}^{\prime} \in 1 . . n_{i} \backslash\left\{j_{i}\right\}} \pi_{i j_{i}^{\prime}}^{\mathrm{n}} \vee \bigvee_{i^{\prime} \in 1 . . m \backslash\{i\}} \operatorname{neg}\left(\pi_{i^{\prime} j_{i^{\prime}}}^{\mathrm{n}}\right) \vee \pi_{i j_{i}}^{\mathrm{n}} \vee \operatorname{neg}\left(\pi_{i j_{i}}^{\mathrm{n}}\right)$, which implies $\leqslant^{\mathrm{cdn}} \bigvee_{j_{i}^{\prime} \in 1 . . n_{i}} \pi_{i j_{i}^{\prime}}^{\mathrm{n}} \vee \bigvee_{i^{\prime} \in 1 . . m} \operatorname{neg}\left(\pi_{i^{\prime} j_{i^{\prime}}}^{\mathrm{n}}\right)$. Finally by S-AndOr22, we have $T \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\pi^{\prime} \vee \neg \pi^{\prime}\right)$.

Case S-Compl. Then $\tau=\tau^{\prime} \wedge \neg \tau^{\prime}$ and $\pi=\perp$ for some $\tau^{\prime}$. Let $\operatorname{cdn}\left(\tau^{\prime}\right)=\bigwedge_{i \in 1 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \tau_{i j_{i}}^{\mathrm{n}}$. Then $\operatorname{cdn}\left(\neg \tau^{\prime}\right)=\operatorname{neg}\left(\operatorname{cdn}\left(\tau^{\prime}\right)\right)=\bigwedge_{\overline{j_{i} \in 1 . . n_{i}}}{ }^{i \in 1 . . m} \bigvee_{i \in 1 . . m} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$. We want to show $\bigwedge_{i \in 1 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \tau_{i j_{i}}^{\mathrm{n}} \wedge \bigwedge_{\bar{j}_{i} \in 1 . . n_{i}}{ }^{i \in 1 . . m} \bigvee_{i \in 1 . . m} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \neg$. By S-DistribCDN ⋅ , it suffices to show $\overline{\tau_{1 j_{1}^{\prime}}^{\mathrm{n}} \wedge \bigwedge_{i \in 2 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \tau_{i j_{i}}^{\mathrm{n}} \wedge \bigwedge_{\bar{j}_{i} \in 1 . . n_{i}} i \in 1 . . m} \bigvee_{i \in 1 . . m} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \neg \top^{j}{ }^{\prime} \in 1 . . n_{1}$, i.e., ${\overline{\tau_{1 j_{1}^{\prime}}^{\mathrm{n}}} \wedge \bigwedge_{i \in 2 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \tau_{i j_{i}}^{\mathrm{n}} \wedge \bigwedge_{{\overline{j_{i} \in 1 . . n}}^{i \in 1 . . m}} \bigvee_{i \in 1 . . m \backslash\left\{i^{\prime} \in 1 . .1 \mid j_{i^{\prime}}=j_{i^{\prime}}^{\prime}\right\}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \neg \top^{j} j_{1}^{\prime} \in 1 . . n_{1}}$ since $\tau_{1 j_{1}^{\prime}}^{\mathrm{n}} \wedge\left(\operatorname{neg}\left(\tau_{1 j_{1}^{\prime}}^{\mathrm{n}}\right) \vee \tau^{\prime \prime}\right) \leqslant \tau_{1 j_{1}^{\prime}}^{\mathrm{n}} \wedge \tau^{\prime \prime}$ for any $\tau^{\prime \prime}$. Repeating the process, it suffices to show $\overline{\tau_{1 j_{1}^{\prime}}^{\mathrm{n}} \wedge \tau_{2 j_{2}^{\prime}}^{\mathrm{n}} \wedge \bigwedge_{i \in 3 . . m} \bigvee_{j_{i} \in 1 . . n_{i}} \tau_{i j_{i}}^{\mathrm{n}} \wedge \bigwedge_{\overline{j_{i} \in 1 . . n_{i}}} i \in 1 . . m} \bigvee_{i \in 1 . . m \backslash\left\{i^{\prime} \in 1 . .2 \mid j_{i^{\prime}}=j_{i^{\prime}}^{\prime}\right\}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \neg \top^{j_{1}^{\prime} \in 1 . . n_{1}, j_{2}^{\prime} \in 1 . . n_{2}}$. Repeating the process $m$ times, it suffices to show ${\bar{\bigwedge} \bigwedge_{i \in 1 . . m} \tau_{i j_{i}^{\prime}}^{\mathrm{n}} \wedge \bigwedge_{\bar{j}_{i} \in 1 . . n_{i}}{ }^{i \in 1 . . m}} \bigvee_{i \in 1 . . m \backslash\left\{i^{\prime} \in 1 . . m \mid j_{i^{\prime}}=j_{i^{\prime}}\right\}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \neg \top^{j_{i}^{\prime} \in 1 . . n_{i}}{ }^{i \in 1 . . m}$, which is indeed true since one of the conjuncts is an empty union, i.e., $\neg \top$, when ${\overline{j_{i}=j_{i}^{\prime}}}^{i \in 1 . . m}$.
Case S-NegInv. We define a function neg $(\cdot)$ that takes a CDN-normalized subtyping derivation for $\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}$, where $\Sigma$ is CDN-normalized, and returns a CDN-normalized subtyping derivation for $\Sigma \vdash \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right)$. We prove its correctness by induction.
Case S-Refl. neg $\left(\right.$ S-Refl $\left.\frac{}{\tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}}\right)=$ S-Refl $\frac{}{\operatorname{neg}\left(\tau^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)}$
Case S-ToB. neg $\left(\right.$ S-ToB. $\left.\frac{}{\tau^{\text {cdn }} \leqslant^{\text {cdn }} \text { T }}\right)=$ S-ToB $\supset \frac{}{\neg \top \leqslant^{\text {cdn }} \operatorname{neg}\left(\tau^{\text {cdn }}\right)}$
Case S-ToB • neg $\left(\right.$ S-ToB $\left.\supset \frac{}{\neg \top \leqslant^{\text {cdn }} \tau^{\text {cdn }}}\right)=$ S-ToB. $\frac{}{\operatorname{neg}\left(\tau^{\text {cdn }}\right) \leqslant^{\text {cdn }} \text { T }}$
Case S-Compl. $\operatorname{neg}\left(\right.$ S-Compl $\left.\cdot \frac{}{\top \leqslant^{\operatorname{cdn}} \tau^{0} \vee \neg \tau^{0}}\right)=$
$$
\text { S-Trans } \frac{\text { S-COMMUT } \supset \frac{\partial \tau^{0} \wedge \tau^{0} \leqslant^{\mathrm{cdn}} \tau^{0} \wedge \neg \tau^{0}}{\neg \tau^{0} \wedge \tau^{0} \leqslant^{\mathrm{cdn}} \top}}{\text { S-COMPL } \supset \overline{\tau^{0} \wedge \neg \tau^{0} \leqslant^{\mathrm{cdn}} \neg \top}}
$$

Case S-Compl. neg (S-Compl $\left.\frac{}{\tau^{0} \wedge \neg \tau^{0} \xi^{\mathrm{cdn}} \neg \top}\right)=$ S-Compl. $\overline{\mathrm{T} \leqslant^{\mathrm{cdn}} \tau^{0} \vee \neg \tau^{0}} \quad$ S-Commut. $\overline{\tau^{0} \vee \neg \tau^{0} \leqslant^{\mathrm{cdn}} \neg \tau^{0} \vee \tau^{0}}$
S-Trans $T \leqslant^{\mathrm{cdn}} \neg \tau^{0} \vee \tau^{0}$
Case S-NegInv. neg $\left(\right.$ S-NegInv $\left.\frac{\Sigma \vdash \pi^{0} \leqslant^{\mathrm{cdn}} \tau^{0}}{\Sigma \vdash \neg \tau^{0} \leqslant^{\mathrm{cdn}} \neg \pi^{0}}\right)=\Sigma \vdash \pi^{0} \leqslant^{\mathrm{cdn}} \tau^{0}$
Case S-AndOr1. neg $\left(\mathrm{S}\right.$-AndOr1 $\left.\cdot \frac{S \subseteq\{\bar{i}\}}{\bigvee_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \bigvee_{i} \tau_{i}^{\mathrm{n}}}\right)=$ S-AndOr1D $\frac{S \subseteq\{\bar{i}\}}{\operatorname{neg}\left(\bigvee_{i} \tau_{i}^{\mathrm{n}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\bigvee_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{n}}\right)}$

Case S-AndOr1 . neg $\left(\right.$ S-AndOr $\left.1 \supset \frac{S \subseteq\{\bar{i}\}}{\bigwedge_{i} \tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \bigwedge_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{dn}}}\right)$ :
We have ${\overline{\tau_{i}^{\mathrm{dn}}=\bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}}}^{i}$ for some ${\overline{{\overline{\tau_{i j_{i}}^{\mathrm{n}}}}^{j}}}^{i}$. Then $\overline{\operatorname{neg}\left(\tau_{i}^{\mathrm{dn}}\right)=\bigwedge_{j_{i}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)}$. Then neg $\left(\bigwedge_{i} \tau_{i}^{\mathrm{dn}}\right)=$
$\operatorname{Dis}_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{dn}}\right)=\bigwedge_{\bar{j}_{i}} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$ and $\operatorname{neg}\left(\bigwedge_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{dn}}\right)=\operatorname{Dis}_{i^{\prime} \in S} \operatorname{neg}\left(\tau_{i^{\prime}}^{\mathrm{dn}}\right)=\bigwedge_{\bar{j}_{i}}{ }^{i \in S} \bigvee_{i \in S} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$.
For each ${\overline{j_{i}}}^{i \in S}$, we have $\bar{\bigvee}_{i \in S} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant{ }^{\mathrm{cdn}} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)^{\overline{j_{i}}}{ }^{i \notin S}$ by S-AndOr1•, which imply $\bigvee_{i \in S} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \quad \leqslant^{\mathrm{cdn}} \quad \bigwedge_{\bar{j}_{i}}{ }^{i \notin S} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$ by S-AndOr2d. Then by Lemma B.22d, $\bigvee_{i \in S} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{\bar{j}_{i}}{ }^{i \notin S} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)^{\bar{j}_{i}}{ }^{i \in S}$ imply $\bigwedge_{\bar{j}_{i} \in S} \bigvee_{i \in S} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{\bar{j}_{i}} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$, i.e., $\operatorname{neg}\left(\bigwedge_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\bigwedge_{i} \tau_{i}^{\mathrm{dn}}\right)$.
Case S-AndOr2. $\operatorname{neg}\left(\mathrm{S}\right.$-AndOr2 $\left.\cdot \frac{{\overline{\Sigma \vdash} \tau_{i}^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}}^{i}}{\Sigma \vdash \bigvee_{i} \tau_{i}^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}}\right)=$ S-AndOr2 $\frac{\overline{\operatorname{neg}\left(\Sigma \vdash \tau_{i}^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right)}}{\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \bigvee_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)}$
Case S-AndOr2 . neg (S-AndOr2 $\left.\frac{{\overline{\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau_{i}^{\mathrm{dn}}}}^{i}}{\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \bigwedge_{i} \tau_{i}^{\mathrm{dn}}}\right)$ :
By Corollary B. 73 on $\overline{\operatorname{neg}\left(\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau_{i}^{\mathrm{dn}}\right)}{ }^{i}$.
Case S-DistribCin $\cdot \operatorname{neg}\left(\right.$ S-DistribCdn $\left.\cdot \frac{\overline{\Sigma \vdash \tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}}{\Sigma \vdash\left(\bigvee_{i} \tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right.}\right)$ :
Then for each $i$, $\operatorname{neg}\left(\Sigma \vdash \tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right)$ is a derivation for $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}}\right)$. Let neg $\left(\tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}}\right)=\bigwedge_{j_{i}} \tau_{i j_{i}}^{\mathrm{dn}}$. Then by Lemma B.21, we have $\overline{\sum \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \tau_{i j_{i}}^{\mathrm{dn}}}{ }^{j_{i}}$.
Then combining the results for $\bar{i}$, by S-AnDOR2> on ${\overline{\overline{\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \tau_{i j_{i}}^{\mathrm{dn}}}}}^{i}$, we have $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{i} \bigwedge_{j_{i}} \tau_{i j_{i}}^{\mathrm{dn}}$, where by definition:
$$
\begin{aligned}
\bigwedge_{i} \bigwedge_{j_{i}} \tau_{i j_{i}}^{\mathrm{dn}} & =\operatorname{Con}_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}} \wedge \tau^{\mathrm{cdn}}\right) \\
& =\operatorname{Con}_{i} \operatorname{dis}\left(\operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)\right) \\
& =\operatorname{dis}\left(\bigwedge_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)\right) \\
& =\operatorname{dis}\left(\operatorname{Con}_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)\right) \\
& =\operatorname{dis}\left(\operatorname{neg}\left(\bigvee_{i} \tau_{i}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)\right) \\
& =\operatorname{neg}\left(\left(\bigvee_{i} \tau_{i}^{\mathrm{n}}\right) \wedge \tau^{\mathrm{cdn}}\right)
\end{aligned}
$$

Case S-DistribCDN D. $\operatorname{neg}\left(\right.$ S-DistribCDN D $\left.\frac{\Sigma \vdash \tau^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}} \quad \Sigma \vdash \bigwedge_{i} \tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}{\Sigma \vdash \bigwedge_{i}\left(\tau^{\mathrm{n}} \vee \tau_{i}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}\right)$ : Then $\operatorname{neg}\left(\Sigma \vdash \tau^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right)$ and $\operatorname{neg}\left(\Sigma \vdash \bigwedge_{i} \tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right)$ are derivations for $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau^{\mathrm{n}}\right)$ and $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\bigwedge_{i} \tau_{i}^{\mathrm{dn}}\right)$ respectively. We have
${\overline{\tau_{i}^{\mathrm{dn}}=\bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}}}^{i}$ for some ${\overline{{\overline{\tau_{i j_{i}}^{\mathrm{n}}}}^{j}}}^{i}$. By definition, we have:
$$
\begin{aligned}
\operatorname{neg}\left(\bigwedge_{i} \tau_{i}^{\mathrm{dn}}\right) & =\operatorname{neg}\left(\bigwedge_{i} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \operatorname{neg}\left(\bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \operatorname{Con}_{j_{i}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \bigwedge_{j_{i}} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\operatorname{Con}_{\bar{j}_{i}} \operatorname{Dis}_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\bigwedge_{\bar{j}_{i}} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)
\end{aligned}
$$

Then by Lemma B.21, we have $\overline{\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant \operatorname{cdn} \bigvee \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)^{(j}} \bar{j}^{i}$. Let ${\overline{\tau_{i 0}^{\mathrm{n}}=\tau^{\mathrm{n}}}}^{i}$. Then we have $\overline{\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant \operatorname{cdn} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}^{\prime}}^{\mathrm{n}}\right.}{\overline{j_{i}^{\prime} \in\left\{0, \bar{j}_{i}\right\}}}^{i} \mid 0 \in\left\{{\overline{j_{i}^{\prime}}}^{i}\right\}$ by S-Trans on $\operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau^{\mathrm{n}}\right)$ and S-AndOr11•/S-AndOr12. Then by S-AndOr2d, we have $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{\bar{j}_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} i \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right)$, where by definition:
$$
\begin{aligned}
\bigwedge_{{\overline{j_{i}^{\prime}}}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} \bigvee_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) & =\operatorname{Con}_{\overline{j_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}}} i \operatorname{Dis}_{i} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \bigwedge_{j_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} \operatorname{neg}\left(\tau_{i j_{i}^{\prime}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \operatorname{Con}_{j_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} \operatorname{neg}\left(\tau_{i j_{i}^{\prime}}^{\mathrm{n}}\right) \\
& =\operatorname{Dis}_{i} \operatorname{neg}\left(\bigvee_{j_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} \tau_{i j_{i}^{\prime}}^{\mathrm{n}}\right) \\
& =\operatorname{neg}\left(\bigwedge_{i} \bigvee_{j_{i}^{\prime} \in\left\{0, \overline{j_{i}}\right\}} \tau_{i j_{i}^{\prime}}^{\mathrm{n}}\right) \\
& =\operatorname{neg}\left(\bigwedge_{i}\left(\tau^{\mathrm{n}} \vee \tau_{i}^{\mathrm{dn}}\right)\right)
\end{aligned}
$$

Case S-Trans. neg $\left(\right.$ S-Trans $\left.\frac{\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau^{\prime \mathrm{cdn}} \quad \Sigma \vdash \tau^{\prime \mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}{\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}\right)=$ S-Trans $\frac{\operatorname{neg}\left(\Sigma \vdash \tau^{\prime \mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}\right) \quad \operatorname{neg}\left(\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau^{\prime \mathrm{cdn}}\right)}{\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)}$
Case S-Hyp. neg $\left(\right.$ S-Hyp $\left.\frac{\left(\tau^{\mathrm{cdn}} \leqslant \pi^{\mathrm{cdn}}\right) \in \Sigma}{\Sigma \vdash \tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}}\right)$ :
Since $\Sigma$ is CDN-normalized, $\tau^{\mathrm{cdn}}=\bigwedge_{i} \tau_{i}^{\mathrm{n}}$ and $\pi^{\mathrm{cdn}}=\bigvee_{j} \pi_{j}^{\mathrm{n}}$ for some ${\overline{\tau_{i}^{\mathrm{n}}}}^{i}$ and ${\overline{\pi_{j}^{\mathrm{n}}}}^{j}$. By repeated applications of Theorem B. 20 on $\Sigma \vdash \bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant^{\mathrm{cdn}} \bigvee_{j} \pi_{j}^{\mathrm{n}}$, we have $\Sigma \vdash \bigwedge_{j} \operatorname{neg}\left(\pi_{j}^{\mathrm{n}}\right) \leqslant^{\mathrm{cdn}} \bigvee_{i} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)$, i.e., $\Sigma \vdash \operatorname{neg}\left(\pi^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \operatorname{neg}\left(\tau^{\mathrm{cdn}}\right)$.
Case S-ClsSub. neg $\left(\right.$ S-ClsSub $\left.\frac{C_{2} \in \mathcal{S}\left(\# C_{1}\right)}{\# C_{1} \leqslant^{\text {cdn }} \# C_{2}}\right)=$ S-NegInv $\frac{\text { S-ClsSub } \frac{C_{2} \in \mathcal{S}\left(\# C_{1}\right)}{\# C_{1} \leqslant^{\text {cdn }} \# C_{2}}}{\neg \# C_{2} \leqslant^{\text {cdn }} \neg \# C_{1}}$
Case S-ClsBot. neg $\left(\right.$ S-ClsBot $\left.\frac{C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \quad C_{2} \notin \mathcal{S}\left(\# C_{1}\right)}{\# C_{1} \wedge \# C_{2} \leqslant^{\text {cdn }} \neg \top}\right)=$
$$
\text { S-ClsBotNegInv } \frac{C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \quad C_{2} \notin \mathcal{S}\left(\# C_{1}\right)}{\top \leqslant^{\mathrm{cdn}} \neg \# C_{1} \vee \neg \# C_{2}}
$$

Case S-ClsBotNegInv. neg $\left(\right.$ S-ClsBotNegInv $\left.\frac{C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \quad C_{2} \notin \mathcal{S}\left(\# C_{1}\right)}{\mathrm{T} \leqslant^{\text {cdn }} \neg \# C_{1} \vee \neg \# C_{2}}\right)=$ S-CLSBot $\frac{C_{1} \notin \mathcal{S}\left(\# C_{2}\right) \quad C_{2} \notin \mathcal{S}\left(\# C_{1}\right)}{\# C_{1} \wedge \# C_{2} \leqslant^{\mathrm{cdn}} \neg \top}$
Case S-FunDepth. neg (S-FunDepth $\left.\frac{\triangleleft \Sigma \vdash \tau_{0} \leqslant \tau_{1} \quad \triangleleft \Sigma \vdash \tau_{2} \leqslant \tau_{3}}{\Sigma \vdash \tau_{1} \rightarrow \tau_{2} \leqslant^{\operatorname{cdn}} \tau_{0} \rightarrow \tau_{3}}\right)=$ S-NegInv $\frac{\text { S-FunDepth } \frac{\triangleleft \Sigma \vdash \tau_{0} \leqslant \tau_{1} \quad \triangleleft \Sigma \vdash \tau_{2} \leqslant \tau_{3}}{\Sigma \vdash \tau_{1} \rightarrow \tau_{2} \leqslant^{\mathrm{cdn}} \tau_{0} \rightarrow \tau_{3}}}{\Sigma \vdash \neg\left(\tau_{0} \rightarrow \tau_{3}\right) \leqslant^{\mathrm{cdn}} \neg\left(\tau_{1} \rightarrow \tau_{2}\right)}$
Case S-FunMrg $\diamond$ neg $\left(\right.$ S-FunMrg $\left.\diamond \overline{\tau_{1} \rightarrow \tau_{2} \wedge^{\diamond} \tau_{3} \rightarrow \tau_{4} \leqslant^{\mathrm{cdn}}\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)}\right)=$ S-FunMrgNegInv $\diamond \overline{\neg\left(\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)\right) \leqslant^{\mathrm{cdn}} \neg\left(\tau_{1} \rightarrow \tau_{2}\right) \vee^{\diamond} \neg\left(\tau_{3} \rightarrow \tau_{4}\right)}$

\section*{Case S-FunMrgNegInv ◇ .}
$$
\operatorname{neg}\left(\text { S-FUNMRGNEGINV } \diamond \overline{v_{1}\left(\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)\right) \leqslant \mathrm{cdn} \neg\left(\tau_{1} \rightarrow \tau_{2}\right) \vee^{\diamond} \neg\left(\tau_{3} \rightarrow \tau_{4}\right)}\right)=
$$

S-FUNMRG $\diamond \overline{\tau_{1} \rightarrow \tau_{2} \wedge^{\diamond} \tau_{3} \rightarrow \tau_{4} \leqslant{ }^{\mathrm{cdn}}\left(\tau_{1} \vee^{\diamond} \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge^{\diamond} \tau_{4}\right)}$
Case S-RcoDepth. neg (S-RcdDepth $\left.\frac{\triangleleft \Sigma \vdash \tau_{1} \leqslant \tau_{2}}{\Sigma \vdash\left\{x: \tau_{1}\right\} \leqslant^{\operatorname{cdn}}\left\{x: \tau_{2}\right\}}\right)=$
$$
\text { S-NegInv } \frac{\text { S-RcoDepth } \frac{\triangleleft \Sigma \vdash \tau_{1} \leqslant \tau_{2}}{\Sigma \vdash\left\{x: \tau_{1}\right\} \leqslant^{\operatorname{cdn}}\left\{x: \tau_{2}\right\}}}{\Sigma \vdash \neg\left\{x: \tau_{2}\right\} \leqslant^{\operatorname{cdn}} \neg\left\{x: \tau_{1}\right\}}
$$

Case S-RcoMRG $\diamond$ neg $\left(\right.$ S-RcoMRG $\left.\diamond{ }_{\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\} \leqslant^{\mathrm{cdn}}\left\{x: \tau_{1}\right\} \vee^{\diamond}\left\{x: \tau_{2}\right\}}\right)=$
$$
\text { S-RcoMRGNegInv } \diamond \overline{\neg\left\{x: \tau_{1}\right\} \wedge^{\diamond} \neg\left\{x: \tau_{2}\right\} \leqslant^{\mathrm{cdn}} \neg\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\}}
$$
$$
\begin{aligned}
& \text { Case S-RcoMRGNegInv } \text { ↓ } \\
& \quad \operatorname{neg}\left(\text { S-RcdMRGNegInv } \text { ◇ } \frac{}{\neg\left\{x: \tau_{1}\right\} \wedge^{\diamond} \neg\left\{x: \tau_{2}\right\} \leqslant^{\mathrm{cdn}} \neg\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\}}\right)=
\end{aligned}
$$

S-RcDMRG $\diamond \overline{\left\{x: \tau_{1} \vee^{\diamond} \tau_{2}\right\} \leqslant^{\diamond \text { cdn }}\left\{x: \tau_{1}\right\} \vee^{\diamond}\left\{x: \tau_{2}\right\}}$
Case S-RcoTop. neg $\left(\operatorname{S-RcdTop} \frac{\tau \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}}{\top \leqslant\left\{x: \tau_{1}\right\} \vee \tau}\right)=$ S-RcdTopNegInv $\frac{\tau \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}}{\neg\left\{x: \tau_{1}\right\} \wedge \neg \tau \leqslant \neg \top}$
Case S-RcdTopNegInv. neg(S-RcdTopNegInv $\frac{\tau \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}}{\neg\left\{x: \tau_{1}\right\} \wedge \neg \tau \leqslant \neg \top}$ )= S-RcdTop $\frac{\tau \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}}{\mathrm{T} \leqslant\left\{x: \tau_{1}\right\} \vee \tau}$

Then $\tau=\neg \tau^{\prime}$ and $\pi=\neg \pi^{\prime}$ for some $\tau^{\prime}$ and $\pi^{\prime}$. Then by IH on the premise, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\pi^{\prime}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\tau^{\prime}\right)$. The result follows from $\operatorname{neg}\left(\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\pi^{\prime}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\tau^{\prime}\right)\right)$.
Case S-AndOr11. Then $\pi=\tau \vee \pi^{\prime}$ for some $\pi^{\prime}$. Then $\operatorname{cdn}(\pi)=\operatorname{dis}\left(\operatorname{cdn}(\tau), \operatorname{cdn}\left(\pi^{\prime}\right)\right)$. Let $\operatorname{cdn}(\tau)=\bigwedge_{i} \tau_{i}^{\mathrm{dn}}$ and $\operatorname{cdn}\left(\pi^{\prime}\right)=\bigwedge_{j} \pi_{j}^{\mathrm{dn}}$. Then $\operatorname{dis}\left(\operatorname{cdn}(\tau), \operatorname{cdn}\left(\pi^{\prime}\right)\right)=\bigwedge_{i, j}\left(\tau_{i}^{\mathrm{dn}} \vee \pi_{j}^{\mathrm{dn}}\right)$. By S-AndOr1 , we have $\overline{\tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau_{i}^{\mathrm{dn}} \vee \pi_{j}^{\mathrm{dn}}}{ }^{i, j}$, which imply ${\overline{\tau_{i}^{\mathrm{dn}}} \leqslant^{\mathrm{cdn}} \bigwedge_{j}\left(\tau_{i}^{\mathrm{dn}} \vee \pi_{j}^{\mathrm{dn}}\right)}^{i}$ by SAndOR2 , which imply $\bigwedge_{i} \tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \bigwedge_{i, j}\left(\tau_{i}^{\mathrm{dn}} \vee \pi_{j}^{\mathrm{dn}}\right)$ by Lemma B.22 , i.e., $\operatorname{cdn}(\tau) \leqslant^{\mathrm{cdn}} \operatorname{cdn}(\pi)$.
Case S-AndOr11 D. Then $\tau=\pi \wedge \tau^{\prime}$ for some $\tau^{\prime}$. Then $\operatorname{cdn}(\tau)=\operatorname{con}\left(\operatorname{cdn}(\pi), \operatorname{cdn}\left(\tau^{\prime}\right)\right)= \operatorname{cdn}(\pi) \wedge \operatorname{cdn}\left(\tau^{\prime}\right)$. Let $\operatorname{cdn}\left(\tau^{\prime}\right)=\bigwedge_{i} \tau_{i}^{\mathrm{dn}}$ and $\operatorname{cdn}(\pi)=\bigwedge_{j} \pi_{j}^{\mathrm{dn}}$. By S-AndOr1 , we have $\bigwedge_{j} \pi_{j}^{\mathrm{dn}} \wedge \bigwedge_{i} \tau_{i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \bigwedge_{j} \pi_{j}^{\mathrm{dn}}$, i.e., $\operatorname{cdn}(\tau) \leqslant^{\mathrm{cdn}} \operatorname{cdn}(\pi)$.
Cases S-AndOr12。. Similar to the cases above.
Case S-AndOr2. Then $\tau=\tau_{1} \vee \tau_{2}$ for some $\tau_{1}$ and $\tau_{2}$. By IH on the premises, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\tau_{1}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$ and $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\tau_{2}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$. Then by Corollary B.73, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{dis}\left(\operatorname{cdn}\left(\tau_{1}\right), \operatorname{cdn}\left(\tau_{2}\right)\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$, i.e., $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\tau_{1} \vee \tau_{2}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$.
Case S-AndOr2 . Then $\pi=\pi_{1} \wedge \pi_{2}$ for some $\pi_{1}$ and $\pi_{2}$. By IH on the premises, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\pi_{1}\right)$ and $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\pi_{2}\right)$. Let $\operatorname{cdn}\left(\pi_{1}\right)= \bigwedge_{i} \pi_{1 i}^{\mathrm{dn}}$ and $\operatorname{cdn}\left(\pi_{2}\right)=\bigwedge_{j} \pi_{2 j}^{\mathrm{dn}}$. By Lemma B.21, we have $\overline{\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\mathrm{cdn}} \pi_{1 i}^{\mathrm{dn}}}{ }^{i}$ and $\overline{\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\mathrm{cdn}}{\pi_{2 j}^{\mathrm{dn}}}^{j}}$. Then by S-AnDOR2d, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\mathrm{cdn}} \bigwedge_{i} \pi_{1 i}^{\mathrm{dn}} \wedge \wedge_{j} \pi_{2 j}^{\mathrm{dn}}=\operatorname{cdn}\left(\pi_{1} \wedge \pi_{2}\right)$.
Case S-Distrib. Then $\tau=\tau_{0} \wedge\left(\tau_{1} \vee \tau_{2}\right)$ and $\pi=\left(\tau_{0} \wedge \tau_{1}\right) \vee\left(\tau_{0} \wedge \tau_{2}\right)$ for some $\tau_{0}$ and $\tau_{1}$ and $\tau_{2}$. Let $\operatorname{cdn}\left(\tau_{0}\right)=\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}}, \operatorname{cdn}\left(\tau_{1}\right)=\bigwedge_{i} \tau_{1 i}^{\mathrm{dn}}$, and $\operatorname{cdn}\left(\tau_{2}\right)=\bigwedge_{j} \tau_{2 j}^{\mathrm{dn}}$. Then we have:
$$
\begin{aligned}
\operatorname{cdn}(\tau) & =\operatorname{con}\left(\operatorname{cdn}\left(\tau_{0}\right), \operatorname{dis}\left(\operatorname{cdn}\left(\tau_{1}\right), \operatorname{cdn}\left(\tau_{2}\right)\right)\right) \\
& =\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}} \wedge \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right)
\end{aligned}
$$
$$
\begin{aligned}
\operatorname{cdn}(\pi) & =\operatorname{dis}\left(\operatorname{con}\left(\operatorname{cdn}\left(\tau_{0}\right), \operatorname{cdn}\left(\tau_{1}\right)\right), \operatorname{con}\left(\operatorname{cdn}\left(\tau_{0}\right), \operatorname{cdn}\left(\tau_{2}\right)\right)\right) \\
& =\operatorname{dis}\left(\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}} \wedge \bigwedge_{i} \tau_{1 i}^{\mathrm{dn}}, \bigwedge_{k} \tau_{0 k}^{\mathrm{dn}} \wedge \bigwedge_{j} \tau_{2 j}^{\mathrm{dn}}\right) \\
& =\operatorname{dis}\left(\bigwedge_{i^{\prime} \in\left\{\overline{0 k}^{k}, \overline{i i}^{i}\right\}} \tau_{i^{\prime}}^{\mathrm{dn}}, \bigwedge_{j^{\prime} \in\left\{\overline{0 k}^{k}, \overline{2 j}^{j}\right\}} \tau_{j^{\prime}}^{\mathrm{dn}}\right) \\
& =\bigwedge_{i^{\prime} \in\left\{\overline{0 k}^{k}, \overline{1 i}^{i}\right\}, j^{\prime} \in\left\{\overline{0 k}^{k}, \overline{2 j}^{j}\right\}}\left(\tau_{i^{\prime}}^{\mathrm{dn}} \vee \tau_{j^{\prime}}^{\mathrm{dn}}\right)
\end{aligned}
$$

For each $i^{\prime} \in\left\{\overline{0 k}^{k}, \overline{1 i}^{i}\right\}, j^{\prime} \in\left\{\overline{0 k}^{k}, \overline{2 j}^{j}\right\}$, we have the following: If $i^{\prime}=0 k_{1}$ for some $k_{1}$, then we have $\tau_{0 k_{1}}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau_{0 k_{1}}^{\mathrm{dn}} \vee \tau_{j^{\prime}}^{\mathrm{dn}}$ by S-AndOR1 . If $j^{\prime}=0 k_{2}$ for some $k_{2}$, then we have $\tau_{0 k_{2}}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau_{i^{\prime}}^{\mathrm{dn}} \vee \tau_{0 k_{2}}^{\mathrm{dn}}$ by S-AndOR1 . Otherwise, we have $\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}$ by S-Refl.

Then we have $\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}} \wedge \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{i^{\prime} \in\left\{\overline{0 k}^{k}, \overline{1 i}^{i}\right\}, j^{\prime} \in\left\{\overline{0 k}^{k}, \overline{2 j}^{j}\right\}}\left(\tau_{i^{\prime}}^{\mathrm{dn}} \vee \tau_{j^{\prime}}^{\mathrm{dn}}\right)$ by Lemma B.22 , commutativity, and idempotence, i.e., $\operatorname{cdn}\left(\tau_{0} \wedge\left(\tau_{1} \vee \tau_{2}\right)\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\left(\tau_{0} \wedge \tau_{1}\right) \vee\left(\tau_{0} \wedge \tau_{2}\right)\right)$.
Case S-Distrib . Then $\tau=\left(\tau_{0} \vee \tau_{1}\right) \wedge\left(\tau_{0} \vee \tau_{2}\right)$ and $\pi=\tau_{0} \vee\left(\tau_{1} \wedge \tau_{2}\right)$ for some $\tau_{0}$ and $\tau_{1}$ and $\tau_{2}$. Let $\operatorname{cdn}\left(\tau_{0}\right)=\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}}, \operatorname{cdn}\left(\tau_{1}\right)=\bigwedge_{i} \tau_{1 i}^{\mathrm{dn}}$, and $\operatorname{cdn}\left(\tau_{2}\right)=\bigwedge_{j} \tau_{2 j}^{\mathrm{dn}}$. Then we have $\operatorname{cdn}\left(\left(\tau_{0} \vee \tau_{1}\right) \wedge\left(\tau_{0} \vee \tau_{2}\right)\right)=\bigwedge_{k, i}\left(\tau_{0 k}^{\mathrm{dn}} \vee \tau_{1 i}^{\mathrm{cdn}}\right) \wedge\left(\tau_{0 k}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right)$ and $\operatorname{cdn}\left(\tau_{0} \vee\left(\tau_{1} \wedge \tau_{2}\right)\right)= \wedge_{k, i}\left(\tau_{0 k}^{\mathrm{dn}} \vee \tau_{1 i}^{\mathrm{cdn}}\right) \wedge\left(\tau_{0 k}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right)$. Then we have $\operatorname{cdn}\left(\left(\tau_{0} \vee \tau_{1}\right) \wedge\left(\tau_{0} \vee \tau_{2}\right)\right) \leqslant^{\mathrm{cdn}} \operatorname{cdn}\left(\tau_{0} \vee\left(\tau_{1} \wedge\right.\right. \tau_{2}$ )) by S-Refl.
Case S-Trans. By IH on the premises, followed by S-Trans.

Case S-Hyp. Then the premise of the rule is $(\tau \leqslant \pi) \in \Sigma$. Let $\operatorname{cdn}(\neg \tau \vee \pi)=\bigwedge_{i} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$. Then we have $\overline{\operatorname{cdn}\left(\top \leqslant \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right) \subseteq \operatorname{cdn}(\Sigma)}{ }^{i}$. For each $i$, we have:
Case $\exists \alpha$. $\{\alpha, \neg \alpha\} \subseteq\left\{{\overline{\tau_{i j_{i}}^{\mathrm{n}}}}^{j_{i}}\right\}$. Then we have $\mathrm{T} \leqslant^{\mathrm{cdn}} \alpha \vee \neg \alpha$ by S-COMPL ⋅ and $\alpha \vee \neg \alpha \leqslant^{\mathrm{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by S-AndOr1 for some $\alpha$, which imply ${ }^{\mathrm{T}} \leqslant^{\mathrm{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by S-Trans.
Case $\left(\exists \alpha . \alpha \in\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}}} j_{i}\right\}\right)$ and $\left(\forall \alpha \in\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}}} j_{i}\right\} . \neg \alpha \notin\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}}} j_{i}\right\}\right)$. Then $\left(\bigwedge_{j_{i} \mid \tau_{i j_{i}}^{\mathrm{n}} \neq \alpha} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant \alpha\right) \in \operatorname{cdn}(\Sigma)$ for some $\alpha$ and we have $\operatorname{cdn}(\Sigma) \vdash \bigwedge_{j_{i} \mid \tau_{i j_{i}}^{\mathrm{n}} \neq \alpha} \operatorname{neg}\left(\tau_{i j_{i}}^{\mathrm{n}}\right) \leqslant^{\operatorname{cdn}} \alpha$ by S-Hyp, which implies $\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\mathrm{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by Theorem B.20.
Case $\left(\exists \alpha . \neg \alpha \in\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}} j_{i}}\right\}\right)$ and $\left(\forall \alpha \in\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}}} j_{i}\right\} . \neg \alpha \notin\left\{\overline{\tau_{i j_{i}}^{\mathrm{n}}} j_{i}\right\}\right)$. Then $\left(\alpha \leqslant \bigvee_{j_{i} \mid \tau_{i j_{i}}^{\mathrm{n}} \neq \neg \alpha} \tau_{i j_{i}}^{\mathrm{n}}\right) \in \operatorname{cdn}(\Sigma)$ for some $\alpha$ and we have $\operatorname{cdn}(\Sigma) \vdash \alpha \leqslant^{\mathrm{cdn}} \bigvee_{j_{i} \mid \tau_{i j_{i}}^{\mathrm{n}} \neq \neg \alpha} \tau_{i j_{i}}^{\mathrm{n}}$ by S-Hyp, which implies $\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\operatorname{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by Theorem B.20.
Case $\forall \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i j_{i}}^{\mathrm{n}}}}^{j_{i}}\right\}=\varnothing$. Then $\left(\top \leqslant \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}\right) \in \operatorname{cdn}(\Sigma)$ and we have $\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\mathrm{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by S-Hyp.
Then $\overline{\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\operatorname{cdn}} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}}{ }^{i}$ imply $\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\mathrm{cdn}} \bigwedge_{i} \bigvee_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}}$ by S-AndOr2d, i.e., $\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\neg \tau \vee \pi)$. Let $\operatorname{cdn}(\tau)=\bigwedge_{p} \bigvee_{q_{p}} \tau_{p q_{p}}^{\prime \prime}$ and $\operatorname{cdn}(\pi)=\bigwedge_{r} \bigvee_{s_{r}} \pi_{r s_{r}}^{\prime \prime}$. Then by definition, $\operatorname{cdn}(\neg \tau \vee \pi)=\bigwedge_{\overline{q_{p}}{ }^{p}, r}\left(\bigvee_{p} \operatorname{neg}\left(\tau_{p q_{p}}^{\prime \mathrm{n}}\right) \vee \bigvee_{s_{r}} \pi_{r s_{r}}^{\prime \mathrm{n}}\right) \cdot \operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\mathrm{cdn}} \bigwedge_{\bar{q}_{p}{ }^{p}, r}\left(\bigvee_{p} \operatorname{neg}\left(\tau_{\mu_{p},}^{\mathrm{n}}\right) \vee \bigvee_{s_{r}} \pi_{r s_{r}}^{\prime \mathrm{n}}\right)$ implies $\overline{\operatorname{cdn}(\Sigma) \vdash \top \leqslant^{\mathrm{cdn}} \bigvee_{p} \operatorname{neg}\left(\tau_{p q_{p}}^{\prime \mathrm{n}}\right) \vee \bigvee_{s_{r}} \pi^{\prime \mathrm{n}}{\overline{q_{p}}}_{r}{ }^{p}, r}$ by Lemma B.21, which imply $\overline{\operatorname{cdn}(\Sigma) \vdash \bigwedge_{p} \tau^{\prime \mathrm{n}}{ }_{p q_{p}} \leqslant{ }^{\mathrm{cdn}} \bigvee_{s_{r}} \pi^{\prime \prime}{ }_{r s_{r}}{\overline{q_{p}}}^{p}, r}$ by Theorem B.20, which imply $\overline{\operatorname{cdn}(\Sigma) \vdash \bigwedge_{p} \tau_{p q_{p}}^{\prime \mathrm{n}} \leqslant^{\mathrm{cdn}} \bigwedge_{r} \bigvee_{s_{r}} \pi_{r s_{r}}^{\prime \mathrm{n}}{\overline{q_{p}}}^{p}}$ by S-AnDOR2 , which imply $\operatorname{cdn}(\Sigma) \vdash \bigwedge_{p} \bigvee_{q_{p}} \tau_{p q_{p}}^{\prime \mathrm{n}} \leqslant^{\mathrm{cdn}} \bigwedge_{r} \bigvee_{s_{r}} \pi_{r s_{r}}^{\prime \mathrm{n}}$ by repeated applications of S-DistribCDN ⋅ and commutativity i.e., $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}(\tau) \leqslant^{\operatorname{cdn}} \operatorname{cdn}(\pi)$.
Case S-FunDepth. Then $\tau=\tau_{1} \rightarrow \tau_{2}$ and $\pi=\pi_{1} \rightarrow \pi_{2}$ for some $\tau_{1}, \tau_{2}, \pi_{1}, \pi_{2}$. The premises of the rule are $\triangleleft \Sigma \vdash \pi_{1} \leqslant \tau_{1}$ and $\triangleleft \Sigma \vdash \tau_{2} \leqslant \pi_{2}$. Each application of S-Hyp in the subderivations of the premises has a premise $H \in \triangleleft \Sigma$ for some $H=\left(\tau^{\prime} \leqslant \pi^{\prime}\right)$, which implies either $\triangleright H \in \Sigma$ or $H \in \Sigma$. If $\triangleright H \in \Sigma$, we have $\triangleright H \in \operatorname{cdn}(\Sigma)$, which implies $H \in \triangleleft \operatorname{cdn}(\Sigma)$, which implies $\triangleleft \operatorname{cdn}(\Sigma) \vdash H$ by S-Hyp. If $H \in \Sigma$, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\tau^{\prime}\right) \leqslant^{\operatorname{cdn}} \operatorname{cdn}\left(\pi^{\prime}\right)$ by the same reasoning as case S-Hyp, which implies $\operatorname{cdn}(\Sigma) \vdash \tau^{\prime} \leqslant \pi^{\prime}$ by Lemma B. 66 and Lemma B.67, which implies $\triangleleft \operatorname{cdn}(\Sigma) \vdash \tau^{\prime} \leqslant \pi^{\prime}$. Then by replacing each application of S-Hyp for $\triangleleft \Sigma \vdash H$ in the subderivations of the premises with the derivation for $\triangleleft \operatorname{cdn}(\Sigma) \vdash H$, we obtain derivations for $\triangleleft \operatorname{cdn}(\Sigma) \vdash \pi_{1} \leqslant \tau_{1}$ and $\triangleleft \operatorname{cdn}(\Sigma) \vdash \tau_{2} \leqslant \pi_{2}$, which imply $\operatorname{cdn}(\Sigma) \vdash \tau_{1} \rightarrow \tau_{2} \leqslant^{\operatorname{cdn}} \pi_{1} \rightarrow \pi_{2}$ by S-FunDepth.
Case S-RcoDepth. Then $\tau=\left\{x: \tau_{1}\right\}$ and $\pi=\left\{x: \pi_{1}\right\}$ for some $\tau_{1}, \pi_{1}$, and $x$. The premise of the rule are $\triangleleft \Sigma \vdash \tau_{1} \leqslant \pi_{1}$. Each application of S-Hyp in the subderivations of the premise has a premise $H \in \triangleleft \Sigma$ for some $H=\left(\tau^{\prime} \leqslant \pi^{\prime}\right)$, which implies either $\triangleright H \in \Sigma$ or $H \in \Sigma$. If $\triangleright H \in \Sigma$, we have $\triangleright H \in \operatorname{cdn}(\Sigma)$, which implies $H \in \triangleleft \operatorname{cdn}(\Sigma)$, which implies $\triangleleft \operatorname{cdn}(\Sigma) \vdash H$ by S-Hyp. If $H \in \Sigma$, we have $\operatorname{cdn}(\Sigma) \vdash \operatorname{cdn}\left(\tau^{\prime}\right) \leqslant^{\mathrm{cdn}} \operatorname{cdn}\left(\pi^{\prime}\right)$ by the same reasoning as case S-Hyp, which implies $\operatorname{cdn}(\Sigma) \vdash \tau^{\prime} \leqslant \pi^{\prime}$ by Lemma B. 66 and Lemma B.67, which implies $\triangleleft \operatorname{cdn}(\Sigma) \vdash \tau^{\prime} \leqslant \pi^{\prime}$. Then by replacing each application of S-Hyp for $\triangleleft \Sigma \vdash H$ in the subderivations of the premise with the derivation for $\triangleleft \operatorname{cdn}(\Sigma) \vdash H$, we obtain a derivation for $\triangleleft \operatorname{cdn}(\Sigma) \vdash \tau_{1} \leqslant \pi_{1}$, which imply $\operatorname{cdn}(\Sigma) \vdash\left\{x: \tau_{1}\right\} \leqslant^{\operatorname{cdn}}\left\{x: \pi_{1}\right\}$ by S-RcdDepth.
Other cases. Immediate since they are already in the desired form. \(\square\)

Lemma B.72. If $\Sigma \vdash \bigwedge_{i} \tau_{1 i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}$ and $\Sigma \vdash \bigwedge_{j} \tau_{2 j}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}$, then $\Sigma \vdash \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}$.

Proof. For each $i$, we have

Then we have
Lemma B.22 $\frac{\overline{(i)}^{i}}{\bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \bigwedge_{i}\left(\tau^{\mathrm{dn}} \vee \tau_{1 i}^{\mathrm{dn}}\right)} \underset{\text { S-Trans }}{\bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}} \frac{\text { S-Refl } \overline{\tau^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}} \bigwedge_{i} \tau_{1 i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}}{\bigwedge_{i}\left(\tau^{\mathrm{dn}} \vee \tau_{1 i}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \tau^{\mathrm{dn}}}$ \(\square\)

Corollary B.73. If $\Sigma \vdash \bigwedge_{i} \tau_{1 i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$ and $\Sigma \vdash \bigwedge_{j} \tau_{2 j}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$, then $\Sigma \vdash \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$. In other words, if $\Sigma \vdash \tau_{1}^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$ and $\Sigma \vdash \tau_{2}^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$, then $\Sigma \vdash \operatorname{dis}\left(\tau_{1}^{\mathrm{cdn}}, \tau_{2}^{\mathrm{cdn}}\right) \leqslant^{\mathrm{cdn}} \tau^{\mathrm{cdn}}$.

Proof. We have $\tau^{\mathrm{cdn}}=\bigwedge_{k} \tau_{0 k}^{\mathrm{dn}}$ for some ${\overline{\tau_{0 k}^{\mathrm{dn}}}}^{k}$. By Lemma B.21, we have $\overline{\Sigma \vdash \bigwedge_{i} \tau_{1 i}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}} \tau_{0 k}^{\mathrm{dn}}} k$ and $\overline{\Sigma \vdash \bigwedge_{j} \tau_{2 j}^{\mathrm{dn}} \leqslant^{\mathrm{cdn}}{\tau_{0 k}^{\mathrm{dn}}}^{k}}$, which imply $\overline{\Sigma \vdash \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant^{\mathrm{cdn}} \tau_{0 k}^{\mathrm{dn}}}{ }^{k}$ by Lemma B.72, which imply $\Sigma \vdash \bigwedge_{i, j}\left(\tau_{1 i}^{\mathrm{dn}} \vee \tau_{2 j}^{\mathrm{dn}}\right) \leqslant{ }^{\mathrm{cdn}} \bigwedge_{k} \tau_{0 k}^{\mathrm{dn}}=\tau^{\mathrm{cdn}}$ by S-AndOr2 . \(\square\)

\section*{B.10.2 DCN-normalized type forms and derivations.}

Definition B. 74 (DCN-normalized form). The syntax of DCN-normalized (disjunction-conjunctionnegation) form is presented in Figure 18. We say that a DCN-normalized form $\tau^{\mathrm{dcn}}$ is complement-free if $\tau^{\mathrm{dcn}}=\bigvee_{i} \bigwedge_{j \in 1 . . n_{i}} \tau_{i j}^{\mathrm{n}}$, where $\forall{\overline{j_{i} \in 1 . . n_{i}}}^{i} . \top \Phi \bigvee_{i} \tau_{i j_{i}}^{\mathrm{n}}$.
$$
\begin{aligned}
\tau^{0} & ::=\tau \rightarrow \tau|\{x: \tau\}| \# C|\alpha| \perp \\
\tau^{\mathrm{n}} & ::=\tau^{0} \mid \neg \tau^{0} \\
\tau^{\mathrm{cn}} & ::=\tau^{\mathrm{n}} \mid \tau^{\mathrm{n}} \wedge \tau^{\mathrm{cn}} \\
\tau^{\mathrm{dcn}} & ::=\tau^{\mathrm{cn}} \mid \tau^{\mathrm{cn}} \vee \tau^{\mathrm{dcn}}
\end{aligned}
$$

Fig. 18. Syntax of DCN-normalized form.

In the proofs below, we sometimes abuse the notations $\tau_{1}^{\mathrm{cn}} \wedge \tau_{2}^{\mathrm{cn}}$ and $\tau_{1}^{\mathrm{dcn}} \vee \tau_{2}^{\mathrm{dcn}}$ to mean their properly associated versions, i.e., $\operatorname{con}\left(\tau_{1}^{\mathrm{cn}}, \tau_{2}^{\mathrm{cn}}\right)$ and $\operatorname{dis}\left(\tau_{1}^{\mathrm{dcn}}, \tau_{2}^{\mathrm{dcn}}\right)$ in Figure 20 respectively.

Definition B. 75 (DCN-normalized derivations). The DCN-normalized subtyping relation $\leqslant^{\mathrm{dcn}}$ is defined in Figure 19. The following are the difference compared to the full subtyping relation $\leqslant$ in Figure 4:
- On the top level, the relation is restricted to $\Sigma \vdash \tau^{\mathrm{dcn}} \leqslant \tau^{\mathrm{dcn}}$.
- On the top level, all occurrences of $\top$ are replaced with $\neg \perp$.

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-086.jpg?height=1456&width=1386&top_left_y=304&top_left_x=150}
\captionsetup{labelformat=empty}
\caption{Fig. 19. DCN-normalized subtyping rules.}
\end{figure}
- The rule S-Distrib◇ is replaced by S-DistribDcn\&, which requires an application of SDistrib◇ to be preceeded immediately by an application of S-AndOr2> in a transitivity chain by merging the two rules into one.
- The negated-inverted versions of the algebraic rules are added.

Notice that the premises of S-FunDepth and S-RcdDepth still refer to the full $\leqslant$ relation, even though their conclusions are about the $\leqslant^{\mathrm{dcn}}$ relation.

The DCN-normalized boolean subtyping relation $\subseteq^{\mathrm{dcn}}$ is defined similarly.

Notice that Lemma B. 21 Lemma B. 22 extends to DCN-normalized derivations. In the proofs below, we also make use of extended versions of commutativity $\left(\tau_{1} \vee^{\diamond} \tau_{2}\left(v^{\diamond} \tau_{3}\right) \leqslant^{\mathrm{dcn}} \tau_{2} v^{\diamond} \tau_{1}\left(v^{\diamond} \tau_{3}\right)\right)$ and idempotence $\left(\tau_{1} \vee^{\diamond} \tau_{1}\left(\vee^{\diamond} \tau_{2}\right) \leqslant{ }^{\text {dcn }} \tau_{1}\left(v^{\diamond} \tau_{2}\right)\right)$.

Definition B. 76 (DCN-normalized form translation). The translation from arbitrary types into DCN-normalized types $\operatorname{dcn}(\cdot)$ is defined in Figure 20.
$$
\begin{aligned}
\operatorname{dcn}(\tau) & : \tau^{\mathrm{dcn}} \\
\operatorname{dcn}\left(\tau^{0}\right) & =\tau^{0} \\
\operatorname{dcn}(\top) & =\neg \perp \\
\operatorname{dcn}(\neg \tau) & =\operatorname{neg}(\operatorname{dcn}(\tau)) \\
\operatorname{dcn}\left(\tau_{1} \wedge \tau_{2}\right) & =\operatorname{con}\left(\operatorname{dcn}\left(\tau_{1}\right), \operatorname{dcn}\left(\tau_{2}\right)\right) \\
\operatorname{dcn}\left(\tau_{1} \vee \tau_{2}\right) & =\operatorname{dis}\left(\operatorname{dcn}\left(\tau_{1}\right), \operatorname{dcn}\left(\tau_{2}\right)\right) \\
\operatorname{neg}\left(\tau^{\mathrm{dcn}}\right) & : \tau^{\mathrm{dcn}} \\
\operatorname{neg}\left(\tau^{0}\right) & =\neg \tau^{0} \\
\operatorname{neg}\left(\neg \tau^{0}\right) & =\tau^{0} \\
\operatorname{neg}\left(\tau_{1}^{\mathrm{n}} \wedge \tau_{2}^{\mathrm{cn}}\right) & =\operatorname{dis}\left(\operatorname{neg}\left(\tau_{1}^{\mathrm{n}}\right), \operatorname{neg}\left(\tau_{2}^{\mathrm{cn}}\right)\right) \\
\operatorname{neg}\left(\tau_{1}^{\mathrm{cn}} \vee \tau_{2}^{\mathrm{dcn}}\right) & =\operatorname{con}\left(\operatorname{neg}\left(\tau_{1}^{\mathrm{cn}}\right), \operatorname{neg}\left(\tau_{2}^{\mathrm{dcn}}\right)\right) \\
\operatorname{con}\left(\tau^{\mathrm{dcn}}, \tau^{\mathrm{dcn}}\right) & : \tau^{\mathrm{dcn}} \\
\operatorname{con}\left(\tau_{11}^{\mathrm{cn}} \vee \tau_{12}^{\mathrm{dcn}}, \tau_{2}^{\mathrm{dcn}}\right) & =\operatorname{dis}\left(\operatorname{con}\left(\tau_{11}^{\mathrm{cn}}, \tau_{2}^{\mathrm{dcn}}\right), \operatorname{con}\left(\tau_{12}^{\mathrm{dcn}}, \tau_{2}^{\mathrm{dcn}}\right)\right) \\
\operatorname{con}\left(\tau_{11}^{\mathrm{n}} \wedge \tau_{12}^{\mathrm{cn}}, \tau_{2}^{\mathrm{dcn}}\right) & =\operatorname{con}\left(\tau_{11}^{\mathrm{n}}, \operatorname{con}\left(\tau_{12}^{\mathrm{cn}}, \tau_{2}^{\mathrm{dcn}}\right)\right) \\
\operatorname{con}\left(\tau_{1}^{\mathrm{n}}, \tau_{21}^{\mathrm{cn}} \vee \tau_{22}^{\mathrm{dcn}}\right) & =\operatorname{dis}\left(\operatorname{con}\left(\tau_{1}^{\mathrm{n}}, \tau_{21}^{\mathrm{cn}}\right), \operatorname{con}\left(\tau_{1}^{\mathrm{n}}, \tau_{22}^{\mathrm{dcn}}\right)\right) \\
\operatorname{con}\left(\tau_{1}^{\mathrm{n}}, \tau_{2}^{\mathrm{cn}}\right) & =\tau_{1}^{\mathrm{n}} \wedge \tau_{2}^{\mathrm{cn}} \\
\operatorname{Con}{ }_{i \in m . . n} \tau_{i}^{\mathrm{dcn}} & =\operatorname{con}\left(\tau_{m}^{\mathrm{dcn}}, \operatorname{Con}{ }_{i \in m+1 . . n} \tau_{i}^{\mathrm{dcn}}\right) \\
\operatorname{Con}{ }_{i \in n . . n} \tau_{i}^{\mathrm{dcn}} & =\tau_{n}^{\mathrm{dcn}} \\
\operatorname{dis}\left(\tau^{\mathrm{dcn}}, \tau^{\mathrm{dcn}}\right) & : \tau^{\mathrm{dcn}} \\
\operatorname{dis}\left(\tau_{11}^{\mathrm{cn}} \vee \tau_{12}^{\mathrm{dcn}}, \tau_{2}^{\mathrm{dcn}}\right) & =\operatorname{dis}\left(\tau_{11}^{\mathrm{cn}}, \operatorname{dis}\left(\tau_{12}^{\mathrm{dcn}}, \tau_{2}^{\mathrm{dcn}}\right)\right) \\
\operatorname{dis}\left(\tau_{1}^{\mathrm{cn}}, \tau_{2}^{\mathrm{dcn}}\right) & =\tau_{1}^{\mathrm{cn}} \vee \tau_{2}^{\mathrm{dcn}} \\
\operatorname{Dis} s_{i \in m . . n} \tau_{i}^{\mathrm{dcn}} & =\operatorname{dis}\left(\tau_{m}^{\mathrm{dcn}}, \operatorname{Dis}{ }_{i \in m+1 . . n} \tau_{i}^{\mathrm{dcn}}\right) \\
\operatorname{Dis}{ }_{i \in n . . n} \tau_{i}^{\mathrm{dcn}} & =\tau_{n}^{\mathrm{dcn}}
\end{aligned}
$$

Fig. 20. DCN-normalized form translation

Lemma B.77. $\Sigma \vdash \tau_{1}^{\mathrm{dcn}} \leqslant \tau_{2}^{\mathrm{dcn}}$ if $\Sigma \vdash \tau_{1}^{\mathrm{dcn}} \leqslant^{\mathrm{dcn}} \tau_{2}^{\mathrm{dcn}}$. Similarly, $\tau_{1}^{\mathrm{dcn}} \subseteq \tau_{2}^{\mathrm{dcn}}$ if $\tau_{1}^{\mathrm{dcn}} \subseteq^{\mathrm{dcn}} \tau_{2}^{\mathrm{dcn}}$.
Proof. It is easy to see that every rule of $\leqslant^{\mathrm{dcn}}$ is admissible in $\leqslant$. \(\square\)

Lemma B.78. For any $\tau, \operatorname{dcn}(\tau) \cong \tau$.

Proof. By straightforward induction.
Definition B. 79 (DCN-normalized subtyping context). $\Sigma$ is DCN-normalized if for all $H \in \Sigma$, either one of the following is true:
(1) $H=\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \perp\right)$, where $\forall \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
(2) $H=\left(\alpha \leqslant \bigvee_{i} \tau_{i}^{\mathrm{n}}\right)$, where the following are true:
- $\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \neg \beta \notin\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\bigwedge_{j} \pi_{j}^{\mathrm{n}} \leqslant \beta\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \beta}, \alpha\right\}$;
- $\forall \neg \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\beta \leqslant \bigvee_{j} \pi_{j}^{\mathrm{n}}\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \beta}, \neg \alpha\right\}$;
(3) $H=\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \alpha\right)$, where the following are true:
- $\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}=\varnothing$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{n}}}^{i}\right\} . \neg \beta \notin\left\{{\overline{\tau_{i}^{n}}}^{i}\right\}$;
- $\forall \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\beta \leqslant \bigvee_{j} \pi_{j}^{\mathrm{n}}\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \beta}, \alpha\right\}$;
- $\forall \neg \beta \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \exists\left(\bigwedge_{j} \pi_{j}^{\mathrm{n}} \leqslant \beta\right) \in \Sigma .\left\{{\overline{\pi_{j}^{\mathrm{n}}}}^{j}\right\}=\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \beta}, \neg \alpha\right\}$;
$$
\begin{aligned}
& \operatorname{dcn}(\Sigma): \Sigma \\
& \operatorname{dcn}(\Sigma)=\overline{\operatorname{dcn}(\operatorname{dcn}(\tau \wedge \neg \pi) \leqslant \perp)}(\tau \leqslant \pi) \in \Sigma . \overline{\triangleright H} \triangleright H \in \Sigma \\
& \operatorname{dcn}\left(\tau^{\mathrm{dcn}} \leqslant \perp\right): \Sigma \\
& \operatorname{dcn}\left(\bigvee_{i} \bigwedge_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}} \leqslant \perp\right)={\overline{\operatorname{dcn}\left(\bigwedge_{j_{i}} \tau_{i j_{i}}^{\mathrm{n}} \leqslant \perp\right)}}^{i} \\
& \operatorname{dcn}\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \perp\right)=\left\{\begin{array}{l}
\epsilon \quad \text { if } \exists \alpha .\{\alpha, \neg \alpha\} \subseteq\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \\
\left.\frac{\left(\alpha \leqslant \bigvee_{i \mid \tau_{i}^{\mathrm{n}} \neq \alpha} \operatorname{neg}\left(\tau_{i}^{\mathrm{n}}\right)\right)}{\left(\alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\}\right.}\right)^{i} \cdot \overline{\left(\bigwedge_{i \mid \tau_{i}^{\mathrm{n}} \neq \neg \alpha} \tau_{i}^{\mathrm{n}} \leqslant \alpha\right)} \alpha \mid \neg \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \\
\text { if }\left(\exists \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} \neq \varnothing\right) \text { and }\left(\forall \alpha \in\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\} . \neg \alpha \notin\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\}\right) \\
\left(\bigwedge_{i} \tau_{i}^{\mathrm{n}} \leqslant \perp\right) \quad \text { if } \forall \alpha .\{\alpha, \neg \alpha\} \cap\left\{{\overline{\tau_{i}^{\mathrm{n}}}}^{i}\right\}=\varnothing
\end{array}\right.
\end{aligned}
$$

Fig. 21. DCN-normalized subtyping context translation

Lemma B.80. For any $\Sigma$, we have $\Sigma \models \operatorname{dcn}(\Sigma)$ and $\operatorname{dcn}(\Sigma) \models \Sigma$.
Proof. Straightforward, notably making use of Theorem B. 20 and Lemma B.78.
Lemma B.81. If $\Sigma \vdash \tau \leqslant \pi$, then $\operatorname{dcn}(\Sigma) \vdash \operatorname{dcn}(\tau) \leqslant^{\operatorname{dcn}} \operatorname{dcn}(\pi)$. Similarly, if $\tau \subseteq \pi$, then $\operatorname{dcn}(\tau) \subseteq{ }^{\operatorname{dcn}} \operatorname{dcn}(\pi)$.

Proof. Symmetric to Lemma B.71.

\section*{B.10.3 Some useful lemmas.}

Lemma B.82.
(A) For $\tau \in\left\{\mathrm{T}, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$ and $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}$ in complement-free CDN-normalized form, if $\bigwedge_{i} \pi_{i}^{\mathrm{dn}} \subseteq \tau$ with a derivation of size $n$, then $\pi_{k}^{\mathrm{dn}} \subseteq \tau$ for some $k$ with a derivation of size $n$.
(B) For $\tau \in\left\{\perp, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$ and $\bigvee_{i} \pi_{i}^{\mathrm{cn}}$ in complement-free DCN-normalized form, if $\tau \subseteq \bigvee_{i} \pi_{i}^{\mathrm{cn}}$ with a derivation of size $n$, then either $\tau \subseteq \pi_{k}^{\mathrm{cn}}$ for some $k$ with a derivation of size $n$.

Only the proof for (A) is shown below. The proof for (B) is symmetric.
Proof. By induction on right-leaning $\subseteq$ derivations.
Case S-Refl. Immediate.
Case S-ToB. Then $\tau=\top$ and we have ${\overline{\pi_{i}^{\mathrm{dn}} \subseteq \top}}^{i}$ by S-ToB., with a derivation of size 1 .
Case S-ToB. Then $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}=\pi_{1}^{\mathrm{dn}}=\perp$. The result is immediate.
Case S-Compl. Impossible since $\tau$ is not a union.
Case S-Compl. Impossible since $\tau \neq \perp$.
Case S-NegInv. Impossible since $\tau$ is not a negation.
Case S-AndOr11. Impossible since $\tau$ is not a union.
Case S-AndOr11>. Then $\pi_{1}^{\mathrm{dn}}=\tau$ and we have $\pi_{1}^{\mathrm{dn}} \subseteq \tau$ by S-Refl, with a derivation of size 1 .
Cases S-AndOr12. Impossible since $\tau$ is not a union.
Cases S-AndOr12>. Then $\bigwedge_{i>1} \pi_{i}^{\mathrm{dn}}=\pi_{2}^{\mathrm{dn}}=\tau$ and we have $\pi_{2}^{\mathrm{dn}} \subseteq \tau$ by S-Refl, with a derivation of size 1.
Case S-AndOr2. Then $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}=\pi_{1}^{\mathrm{dn}}=\pi_{11}^{\mathrm{n}} \vee \pi_{12}^{\mathrm{dn}}$ for some $\pi_{11}^{\mathrm{n}}$ and $\pi_{12}^{\mathrm{dn}}$. The result is immediate.
Case S-AndOr2. Impossible since $\tau$ is not an intersection.
Case S-Trans. Then the premises are $\bigwedge_{i} \pi_{i}^{\mathrm{dn}} \subseteq \tau^{\prime}$ and $\tau^{\prime} \subseteq \tau$ for some $\tau^{\prime}$, both with a derivation of size $n-1$. By induction on the size of the subderivation for the former premise, denoted by $m$. Denote the inner induction hypothesis as $\mathrm{IH}^{\prime}$.
Cases (S-Refl, *), (*, S-Refl). By IH on the other premise.
Cases (S-ToB., *). Then $\tau^{\prime}=\top$. By S-ToB., we have ${\overline{\pi_{i}^{\mathrm{dn}} \subseteq \top^{\top}}}^{i}$. By S-Trans with $\dagger \subseteq \tau$, we have ${\overline{\pi_{i}^{\mathrm{dn}} \subseteq \tau}}^{i}$ with a derivation of size $n$.
Cases (S-ToB $>$, *). Then $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}=\pi_{1}^{\mathrm{dn}}=\perp$. The result is immediate.
Cases (S-Compl $\cdot$ *). Then $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}=\pi_{1}^{\mathrm{dn}}=\mathrm{T}$. The result is immediate.
Cases (S-Compl , *). Impossible since $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}$ is a complement-free CDN-normalized form.
Cases (S-AndOr11, *). Then $\tau^{\prime}=\bigwedge_{i} \pi_{i}^{\mathrm{dn}} \vee \tau_{1}^{\prime}$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $\bigwedge_{i} \pi_{i}^{\mathrm{dn}} \subseteq \tau$ with a derivation of size $n-1$. The result then follows from IH.
Cases (S-AndOr11>, *). Then $\tau^{\prime}=\pi_{1}^{\mathrm{dn}}$. The result is immediate from the latter premise.
Cases (S-AndOr12, *). Then $\tau^{\prime}=\tau_{1}^{\prime} \vee \bigwedge_{i} \pi_{i}^{\mathrm{dn}}$ for some $\tau_{1}^{\prime}$. By Lemma B. 54 on the latter premise, we have $\bigwedge_{i} \pi_{i}^{\mathrm{dn}} \subseteq \tau$ with a derivation of size $n-1$. The result then follows from IH.
Cases (S-AndOr12 , *). Then $\tau^{\prime}=\bigwedge_{i>1} \pi_{i}^{\mathrm{dn}}$. By IH on the latter rule, we have $\pi_{k}^{\mathrm{dn}} \subseteq \tau$ for some $k>1$.
Cases (S-AndOr2, *). Then $\bigwedge_{i} \pi_{i}^{\mathrm{dn}}=\pi_{1}^{\mathrm{dn}}=\pi_{11}^{\mathrm{n}} \vee \pi_{12}^{\mathrm{dn}}$ for some $\pi_{11}^{\mathrm{n}}$ and $\pi_{12}^{\mathrm{dn}}$. The result is immediate.
Cases (S-AndOr2 , *). Then $\tau^{\prime}=\tau_{1}^{\prime} \wedge \tau_{2}^{\prime}$ for some $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. Since $\tau$ is not an intersection, it is easy to see that the intersection must be consumed by an application of S-AndOr112, S-AndOr12>, or S-Distrib◇ in the transitivity chain. Then it is possible to rewrite the derivation into a smaller one by dropping the application of S-AndOr2d. The result then follows from IH.
Cases (S-Distrib, *). Then $\pi_{2}^{\mathrm{dn}}=\pi_{21}^{\mathrm{n}} \vee \pi_{22}^{\mathrm{dn}}$ and $\tau^{\prime}=\left(\pi_{1}^{\mathrm{dn}} \wedge \pi_{21}^{\mathrm{n}}\right) \vee\left(\pi_{1}^{\mathrm{dn}} \wedge \pi_{22}^{\mathrm{dn}}\right)$ for some $\pi_{21}^{\mathrm{n}}$ and $\pi_{22}^{\mathrm{dn}}$. By Lemma B. 54 on the latter rule, we have $\pi_{1}^{\mathrm{dn}} \wedge \pi_{21}^{\mathrm{n}} \subseteq \tau$ and $\pi_{1}^{\mathrm{dn}} \wedge \pi_{22}^{\mathrm{dn}} \subseteq \tau$,
both with a derivation of size $n-1$. By IH on $\pi_{1}^{\mathrm{dn}} \wedge \pi_{21}^{\mathrm{n}} \subseteq \tau$, we have $\pi_{1}^{\mathrm{dn}} \subseteq \tau$ or $\pi_{21}^{\mathrm{n}} \subseteq \tau$, both with a derivation of size $n-1$. By IH on $\pi_{1}^{\mathrm{dn}} \wedge \pi_{22}^{\mathrm{dn}} \subseteq \tau$, we have $\pi_{1}^{\mathrm{dn}} \subseteq \tau$ or $\pi_{22}^{\mathrm{dn}} \subseteq \tau$, both with a derivation of size $n-1$. If $\pi_{1}^{\mathrm{dn}} \subseteq \tau$, then we have the result immediately. Otherwise, we have $\pi_{21}^{\mathrm{n}} \subseteq \tau$ and $\pi_{22}^{\mathrm{dn}} \subseteq \tau$, which imply $\pi_{2}^{\mathrm{dn}}=\pi_{21}^{\mathrm{n}} \vee \pi_{22}^{\mathrm{dn}} \subseteq \tau$ by S-AndOr2., with a derivation of size $n$.
Cases (S-Distrib 2 , *). Then $\pi_{1}^{\mathrm{dn}}=\pi_{0}^{\mathrm{n}} \vee \pi_{12}^{\mathrm{dn}}$ and $\pi_{2}^{\mathrm{dn}}=\pi_{0}^{\mathrm{n}} \vee \pi_{22}^{\mathrm{dn}}$ for some $\pi_{0}^{\mathrm{n}}$ and $\pi_{12}^{\mathrm{dn}}$ and $\pi_{22}^{\mathrm{dn}}$, and $\tau^{\prime}=\pi_{0}^{\mathrm{n}} \vee\left(\tau_{1}^{\prime} \wedge \tau_{2}^{\prime}\right)$. By Lemma B. 54 on the latter rule, we have $\pi_{12}^{\mathrm{dn}} \wedge \pi_{22}^{\mathrm{dn}} \subseteq \tau$ and $\pi_{0}^{\mathrm{n}} \subseteq \tau$, both with a derivation of size $n-1$. By IH, we have $\pi_{12}^{\mathrm{dn}} \subseteq \tau$ or $\pi_{22}^{\mathrm{dn}} \subseteq \tau$, which implies $\pi_{1}^{\mathrm{dn}}=\pi_{0}^{\mathrm{n}} \vee \pi_{12}^{\mathrm{dn}} \subseteq \tau$ or $\pi_{2}^{\mathrm{dn}}=\pi_{0}^{\mathrm{n}} \vee \pi_{22}^{\mathrm{dn}} \subseteq \tau$ with a derivation of size $n$ by S-AndOr2 ⋅ with $\pi_{0}^{\mathrm{n}} \subseteq \tau$.

Lemma B.83.
(A) For $\tau \in\left\{\top, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}\right.$, \# $\left.C\right\}$, if $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau$, then either $\pi_{1}^{\mathrm{n}} \subseteq \tau$ or $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ or $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$.
(B) For $\tau \in\left\{\perp, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$, if $\tau \subseteq \pi_{1}^{\mathrm{n}} \vee \pi_{2}^{\mathrm{dn}}$, then either $\tau \subseteq \pi_{1}^{\mathrm{n}}$ or $\tau \subseteq \pi_{2}^{\mathrm{dn}}$ or $\mathrm{T} \subseteq \pi_{1}^{\mathrm{n}} \vee \pi_{2}^{\mathrm{dn}}$.

Only the proof for (A) is shown below. The proof for (B) is symmetric.
Proof. By induction on right-leaning $\subseteq^{\mathrm{dcn}}$ derivations for the following statements, where S-AndOr2• does not occur as the first premise of S-Trans in any of the judgements (in both the assumptions and conclusions). It is easy to see that we can rewrite any subderivations with SAndOr2• as the first premise of S-Trans into an equivalent one by applying S-Trans to the premises of S-AndOr2• and the second premise of S-Trans, followed by an application of S-AndOr2•.
(1) For $\tau \in\left\{\neg \perp, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$, if $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n$, then either $\pi_{1}^{\mathrm{n}} \subseteq \tau$ or $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n$, or $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$.
(2) For $\tau_{c} \in\left\{\tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$, if $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c}$ with a derivation of size $n$, then ${\overline{\pi_{i}^{\mathrm{cn}}} \subseteq \tau_{c}}^{i}$, all with a derivation of size $n-1$.
In the remainder of this proof, we abbreviate $\subseteq^{\text {dcn }}$ as $\subseteq$.
Case S-Refl. Impossible

\section*{Case S-ToB.}
(1) Then $\tau=\neg \perp$ and we have both $\pi_{1}^{\mathrm{n}} \subseteq \tau$ and $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ by S-ToB.
(2) Impossible.

Cases S-ToB > S-Compl ↓ , S-NegInv, S-AndOr1. Impossible.
Case S-AndOr1D.
(1) Then $\tau=\pi_{k}^{\mathrm{n}}$ for some $k$, where $\pi_{2}^{\mathrm{cn}}=\bigwedge_{i>1} \pi_{i}^{\mathrm{n}}$ for some ${\overline{\pi_{i}^{\mathrm{n}}}}^{i>1}$. If $k=1$, then we have $\pi_{1}^{\mathrm{n}} \subseteq \tau$ by S-Refl. Otherwise, we have $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ by S-AndOr1>.
(2) Impossible.

Case S-AndOr2-
(1) Impossible.
(2) The premises of the rule are ${\overline{\pi_{i}^{\mathrm{cn}} \subseteq \tau_{c}}}^{i}$, all of size $n-1$.

Cases S-AndOr2>, S-DistribDcn∘. Impossible.
Case S-Trans.
(1) Then the premises of the rule are $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau^{\mathrm{dcn}}$ and $\tau^{\mathrm{dcn}} \subseteq \tau$ for some $\tau^{\mathrm{dcn}}$, both of size $n-1$.
(2) Then the premises of the rule are $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau^{\mathrm{dcn}}$ and $\tau^{\mathrm{dcn}} \subseteq \tau_{c}$ for some $\tau^{\mathrm{dcn}}$, both of size $n-1$.
By induction on the size of the former premise of S-Trans, denoted by $m$. Denote the inner induction hypothesis as $\mathrm{IH}^{\prime}$.
Cases (S-Refl, *). By IH on the latter premise.
Cases (S-ToB., *).
(1) Then $\tau^{\mathrm{dcn}}=\neg \perp$. We have both $\pi_{1}^{\mathrm{n}} \subseteq \neg \perp$ and $\pi_{2}^{\mathrm{cn}} \subseteq \neg \perp$ by S-ToB. Then we have both $\pi_{1}^{\mathrm{n}} \subseteq \tau$ and $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ by S-Trans with the latter premise, both with a derivation of size $n$.
(2) Impossible since $\neg \perp \subseteq \tau_{c}$ cannot be derived (Lemma B.87).

Cases (S-ToB > , *), (S-Compl , *). Impossible.
Cases (S-Compl , *).
(1) Then $\tau^{\mathrm{dcn}}=\perp . \pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$ is immediate from the former premise.
(2) Impossible.

Cases (S-NegInv, *). Impossible.
Cases (S-AndOr1, *).
(1) Then $\tau^{\mathrm{dcn}}=\left(\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}}\right) \vee \tau_{2}^{\mathrm{dcn}}$ for some $\tau_{2}^{\mathrm{dcn}}$. If $\tau=\mathrm{T}$, then we have both $\pi_{1}^{\mathrm{n}} \subseteq \tau$ and $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size 1 by S-ToB. Otherwise, the latter premise $\left(\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}}\right) \vee \tau_{2}^{\mathrm{dcn}} \subseteq \tau$ implies $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n-2$ by IH (2). The result then follows from IH (1).
(2) Then $\tau^{\mathrm{dcn}}=\left(\bigvee_{i} \pi_{i}^{\mathrm{cn}}\right) \vee \tau_{2}^{\mathrm{dcn}}$ for some $\tau_{2}^{\mathrm{dcn}}$. The latter premise $\left(\bigvee_{i} \pi_{i}^{\mathrm{cn}}\right) \vee \tau_{2}^{\mathrm{dcn}} \subseteq \tau_{c}$ implies $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c}$ with a derivation of size $n-2$ by IH (2), which implies ${\overline{\pi_{i}^{\mathrm{cn}}} \subseteq \tau_{c}}_{i}^{i}$, all with a derivation of size $n-3$ by IH (2).
Cases (S-AndOr1>, *).
(1) Then $\tau^{\mathrm{dcn}}=\bigwedge_{i^{\prime} \in S} \pi_{i^{\prime}}^{\mathrm{n}}$ for some $S \subseteq\{\bar{i}\}$, where $\pi_{2}^{\mathrm{cn}}=\bigwedge_{i>1} \pi_{i}^{\mathrm{n}}$ for some ${\overline{\pi_{i}^{\mathrm{n}}}}^{i>1}$.

Case $1 \in S$. By IH (1) on the latter premise, we have either $\pi_{1}^{n} \subseteq \tau$ or $\bigwedge_{i^{\prime} \in S \backslash\{1\}} \pi_{i^{\prime}}^{\mathrm{n}} \subseteq \tau$ with a derivation of size $n-1$, or $\bigwedge_{i^{\prime} \in S} \pi_{i^{\prime}}^{\mathrm{n}} \subseteq \perp$. If $\pi_{1}^{\mathrm{n}} \subseteq \tau$, the result is immediate. If $\bigwedge_{i^{\prime} \in S \backslash\{1\}} \pi_{i^{\prime}}^{\mathrm{n}} \subseteq \tau$, then we have $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n$ by S-Trans with S-AndOr1>. If $\bigwedge_{i^{\prime} \in S} \pi_{i^{\prime}}^{\mathrm{n}} \subseteq \perp$, then we have $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$ by S-Trans with S-AndOr1>.
Case $1 \notin S$. Then $\pi_{2}^{\mathrm{cn}} \subseteq \tau$ follows by IH (1) on the latter premise, followed by S-Trans with S-AndOr1>, with a derivation of size $n$.
(2) Impossible.

Cases (S-AndOr2, *). Impossible by assumption.
Cases (S-AndOr2 , *).
(1) Then $\tau^{\mathrm{dcn}}=\bigwedge_{j} \tau_{j}^{\mathrm{n}}$ for some $\bar{\tau}_{j}^{j}$. The premises of the former rule are ${\overline{\pi_{1}^{\mathrm{n}}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau_{j}^{\mathrm{n}}}^{j}$, all of size $m-1$. By repeated applications of $\mathrm{IH}(1)$, the latter premise $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \tau$ implies $\tau_{k}^{\mathrm{n}} \subseteq \tau$ for some $k$ with a derivation of size $n-1$, or $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$.
Case $\tau_{k}^{\mathrm{n}} \subseteq \tau$. Then by S-Trans with one of the premises of the former rule, we have $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}(1)$.
Case $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$. Then we have $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$ by S-Trans with the former premise.
(2) Then $\tau^{\mathrm{dcn}}=\bigwedge_{j} \tau_{j}^{\mathrm{n}}$ for some ${\overline{\tau_{j}^{\mathrm{n}}}}^{j}$. The premises of the former rule are ${\overline{\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{j}^{\mathrm{n}}}}^{j}$, all of size $m-1$. By IH (1), the latter premise $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \tau_{c}$ implies $\tau_{k}^{\mathrm{n}} \subseteq \tau_{c}$ for some $k$ with a derivation of size $n-1$, or $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$.

Case $\tau_{k}^{\mathrm{n}} \subseteq \tau_{c}$. Then by S-Trans with one of the premises of the former rule, we have $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c}$ with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}(2)$.
Case $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$. Then it is easy to see that the transitivity chain in the derivation for one of ${\overline{\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{j}^{\mathrm{n}}}}^{j}$ must pass through ⟂ , i.e., $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \perp$ can be derived with size $n-2$. Then we have $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c}$ with a derivation of size $n-1$ by S-Trans with S-ToB. The result then follows from IH (2).
Cases (S-DistribDcn ⋅ , *).
(1) Then $\tau^{\mathrm{dcn}}=\bigvee_{j}\left(\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}}\right)$ for some $\tau_{0}^{\mathrm{n}}$ and ${\overline{\tau_{j}^{\mathrm{cn}}}}^{j}$. The premises of the former rule are:
$$
\begin{gather*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau_{0}^{\mathrm{n}}  \tag{1}\\
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{j} \tau_{j}^{\mathrm{cn}} \tag{2}
\end{gather*}
$$
both of size $m-1$. The latter premise is:
$$
\begin{equation*}
\bigvee_{j}\left(\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}}\right) \subseteq \tau \tag{3}
\end{equation*}
$$

By IH (2), (3) implies:
$$
\begin{equation*}
{\overline{\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \tau}}^{j} \tag{4}
\end{equation*}
$$
all with derivations of size $n-2$. For each $j$, by IH (1), (4) implies $\tau_{0}^{\mathrm{n}} \subseteq \tau$ or $\tau_{j}^{\mathrm{cn}} \subseteq \tau$ with a derivation of size $n-2$, or $\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp$.
Case $\tau_{0}^{\mathrm{n}} \subseteq \tau$. Then by S-Trans with (1), we have:
$$
\begin{equation*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau \tag{5}
\end{equation*}
$$
with a derivation of size $n-1$. The result then follows from IH (1).
Case $\tau_{0}^{\mathrm{n}} \nsubseteq \tau$. Then for each $j$, we have $\tau_{j}^{\mathrm{cn}} \subseteq \tau$ or $\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp$. Let $S=\left\{j \mid \tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp\right\}$. By S-AndOr2•, we have
$$
\begin{equation*}
\bigvee_{j \notin S} \tau_{j}^{c n} \subseteq \tau \tag{6}
\end{equation*}
$$
with a derivation of size $n-1$. From the definiton of $S$, we have:
$$
\begin{equation*}
{\overline{\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp}}^{j \in S} \tag{7}
\end{equation*}
$$

By Theorem B.20, (7) implies:
$$
\begin{equation*}
{\overline{\tau_{j}^{\mathrm{cn}} \subseteq \tau_{0^{\prime}}^{\mathrm{n}}}}^{j \in S} \tag{8}
\end{equation*}
$$
where $\tau_{0^{\prime}}^{\mathrm{n}}=\operatorname{neg}\left(\tau_{0}^{\mathrm{n}}\right)$. By Lemma B.22. on (8) and S-Refl, we have:
$$
\begin{equation*}
\bigvee_{j} \tau_{j}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \vee \tau_{0^{\prime}}^{\mathrm{n}} \tag{9}
\end{equation*}
$$

Then by S-Trans on (2) and (9), we have:
$$
\begin{equation*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \vee \tau_{0^{\prime}}^{\mathrm{n}} \tag{10}
\end{equation*}
$$

By Theorem B.20, (10) implies:
$$
\begin{equation*}
\tau_{0}^{\mathrm{n}} \wedge \pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \tag{11}
\end{equation*}
$$

By S-Trans with S-AndOr2 on (1) and S-Refl, (11) implies:
$$
\begin{equation*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \tag{12}
\end{equation*}
$$

Since we have (2) with a derivation of size $m-1$ and (12), it is easy to see that (12) can be derived with size $m-1$. Then by S-Trans with (6), we have:
$$
\begin{equation*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau \tag{13}
\end{equation*}
$$
with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}$ (1).
(2) Then $\tau^{\mathrm{dcn}}=\bigvee_{j}\left(\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}}\right)$ for some $\tau_{0}^{\mathrm{n}}$ and ${\overline{\tau_{j}^{\mathrm{cn}}}}^{j}$. The premises of the former rule are:
$$
\begin{gather*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{0}^{\mathrm{n}}  \tag{14}\\
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{j} \tau_{j}^{\mathrm{cn}} \tag{15}
\end{gather*}
$$
both with a derivation of size $m-1$. The latter premise is:
$$
\begin{equation*}
\bigvee_{j}\left(\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}}\right) \subseteq \tau_{c} \tag{16}
\end{equation*}
$$

By IH (2), (16) implies:
$$
\begin{equation*}
{\overline{\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \tau_{c}}}^{j} \tag{17}
\end{equation*}
$$
all with a derivation of size $n-2$. For each $j$, by IH (1), (17) implies $\tau_{0}^{\mathrm{n}} \subseteq \tau_{c}$ or $\tau_{j}^{\mathrm{cn}} \subseteq \tau_{c}$ with a derivation of size $n-2$, or $\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp$.
Case $\tau_{0}^{\mathrm{n}} \subseteq \tau_{c}$. Then by S-Trans with (14), we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c} \tag{18}
\end{equation*}
$$
with a derivation of size $n-1$. The result then follows from IH .
Case $\tau_{0}^{\mathrm{n}} \nsubseteq \tau_{c}$. Then for each $j$, we have $\tau_{j}^{\mathrm{cn}} \subseteq \tau_{c}$ or $\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp$. Let $S=\left\{j \mid \tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp\right\}$. By S-AndOr2•, we have:
$$
\begin{equation*}
\bigvee_{j \notin S} \tau_{j}^{c n} \subseteq \tau_{c} \tag{19}
\end{equation*}
$$
with a derivation of size $n-1$. From the definiton of $S$, we have:
$$
\begin{equation*}
{\overline{\tau_{0}^{\mathrm{n}} \wedge \tau_{j}^{\mathrm{cn}} \subseteq \perp}}^{j \in S} \tag{20}
\end{equation*}
$$

By Theorem B.20, (20) implies:
$$
\begin{equation*}
{\overline{\tau_{j}^{\mathrm{cn}} \subseteq \tau_{0^{\prime}}^{\mathrm{n}}}}^{j \in S} \tag{21}
\end{equation*}
$$
where $\tau_{0^{\prime}}^{\mathrm{n}}=\operatorname{neg}\left(\tau_{0}^{\mathrm{n}}\right)$. By Lemma B.22. on (21) and S-RefL, we have:
$$
\begin{equation*}
\bigvee_{j} \tau_{j}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \vee \tau_{0^{\prime}}^{\mathrm{n}} \tag{22}
\end{equation*}
$$

Then by S-Trans on (15) and (22), we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \vee \tau_{0^{\prime}}^{\mathrm{n}} \tag{23}
\end{equation*}
$$

By Theorem B.20, (23) implies:
$$
\begin{equation*}
\tau_{0}^{\mathrm{n}} \wedge \bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \tag{24}
\end{equation*}
$$

By S-Trans with S-AndOr2 on (14) and S-Refl, (24) implies:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{j \notin S} \tau_{j}^{\mathrm{cn}} \tag{25}
\end{equation*}
$$

Since we have (15) with a derivation of size $m-1$ and (25), it is easy to see that (25) can be derived with size $m-1$. Then by S-Trans with (19), we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c} \tag{26}
\end{equation*}
$$
with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}$ (2).
Cases (S-DistribDcn » , *).
(1) Then $\tau^{\mathrm{dcn}}=\left(\bigwedge_{j} \tau_{j}^{\mathrm{n}}\right) \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}}$ for some ${\overline{\tau_{j}^{\mathrm{n}}}}^{j}$ and ${\overline{\tau_{k}^{\mathrm{cn}}}}^{k}$. The premises of the former rule are:
$$
\begin{equation*}
{\overline{\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau_{j}^{\mathrm{n}} \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}}}}_{j}^{j} \tag{27}
\end{equation*}
$$
all with a derivation of size $m-1$. The latter premise is:
$$
\begin{equation*}
\left(\bigwedge_{j} \tau_{j}^{\mathrm{n}}\right) \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}} \subseteq \tau \tag{28}
\end{equation*}
$$

By IH (2), (28) implies:
$$
\begin{align*}
& \bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \tau  \tag{29}\\
& \bar{\tau}_{k}^{\mathrm{cn}} \subseteq \tau \tag{30}
\end{align*}
$$
all with a derivation of size $n-2$. By repeated applications of IH (1), (29) implies $\tau_{l}^{\mathrm{n}} \subseteq \tau$ for some $l \in\{\bar{j}\}$ with a derivation of size $n-2$, or $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$.
Case $\tau_{l}^{\mathrm{n}} \subseteq \tau$. Then by S-AndOr2. with (30), we have:
$$
\begin{equation*}
\tau_{l}^{\mathrm{n}} \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}} \subseteq \tau \tag{31}
\end{equation*}
$$
with a derivation of size $n-1$. Then by S-Trans on (27) for $j=k$ and (31), we have:
$$
\begin{equation*}
\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \tau \tag{32}
\end{equation*}
$$
with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}$ (1).
Case $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$. Then it is easy to see that the transitivity chain in the derivation for one of (27) must pass through either $\bigvee_{k} \tau_{k}^{\mathrm{cn}}$ or ⟂ , i.e., $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{k} \tau_{k}^{\mathrm{cn}}$ or $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$ can be derived with size $m-1$.
Case $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \bigvee_{k} \tau_{k}^{\mathrm{cn}}$. Then by S-Trans with S-AndOr2. on (30), we have (32) with a derivation of size $n$ and a former derivation of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}(1)$.
Case $\pi_{1}^{\mathrm{n}} \wedge \pi_{2}^{\mathrm{cn}} \subseteq \perp$. then we have the result immediately.
(2) Then $\tau^{\mathrm{dcn}}=\left(\bigwedge_{j} \tau_{j}^{\mathrm{n}}\right) \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}}$ for some ${\overline{\tau_{j}^{\mathrm{n}}}}^{j}$ and ${\overline{\tau_{k}^{\mathrm{cn}}}}^{k}$. The premises of the former rule are:
$$
\begin{equation*}
{\overline{\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{j}^{\mathrm{n}} \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}}}}_{j}^{j} \tag{33}
\end{equation*}
$$
all with a derivation of size $m-1$. The latter premise is:
$$
\begin{equation*}
\left(\bigwedge_{j} \tau_{j}^{\mathrm{n}}\right) \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}} \subseteq \tau_{c} \tag{34}
\end{equation*}
$$

By IH (2), (34) implies:
$$
\begin{equation*}
\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \tau_{c} \tag{35}
\end{equation*}
$$
all with a derivation of size $n-2$. By repeated applications of IH (1), (35) implies $\tau_{l}^{\mathrm{n}} \subseteq \tau_{c}$ for some $l \in\{\bar{j}\}$ with a derivation of size $n-2$, or $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$.

Case $\tau_{l}^{\mathrm{n}} \subseteq \tau_{c}$. Then by S-AndOr2. with (36), we have:
$$
\begin{equation*}
\tau_{l}^{\mathrm{n}} \vee \bigvee_{k} \tau_{k}^{\mathrm{cn}} \subseteq \tau_{c} \tag{37}
\end{equation*}
$$
with a derivation of size $n-1$. Then by S-Trans on (33) for $j=l$ and (37), we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c} \tag{38}
\end{equation*}
$$
with a derivation of size $n$ and a former premise of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}$ (1).
Case $\bigwedge_{j} \tau_{j}^{\mathrm{n}} \subseteq \perp$. Then it is easy to see that the transitivity chain in the derivation for one of (33) must pass through either $\bigvee_{k} \tau_{k}^{\mathrm{cn}}$ or ⟂ , i.e., $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{k} \tau_{k}^{\mathrm{cn}}$ or $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \perp$ can be derived with size $m-1$.
Case $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \bigvee_{k} \tau_{k}^{\mathrm{cn}}$. Then by S-Trans with S-AndOr2. on (36), we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c} \tag{39}
\end{equation*}
$$
with a derivation of size $n$ and a former derivation of size $m-1$. The result then follows from $\mathrm{IH}^{\prime}(2)$.
Case $\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \perp$. Then by S-Trans with S-ToB >, we have:
$$
\begin{equation*}
\bigvee_{i} \pi_{i}^{\mathrm{cn}} \subseteq \tau_{c} \tag{40}
\end{equation*}
$$
with a derivation of size $m \leqslant n-1$. The result then follows from IH (2).

Corollary B.84. For $\tau \in\left\{\mathrm{T}^{\diamond}, \tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$, if $\bigwedge_{i}^{\diamond} \pi_{i}^{\mathrm{n}} \subseteq^{\diamond} \tau$, then either $\pi_{k}^{\mathrm{n}} \subseteq^{\diamond} \tau$ for some $k$ or $\bigwedge_{i}^{\diamond} \pi_{i}^{\mathrm{n}} \subseteq^{\diamond} \perp^{\diamond}$.

Proof. By repeated applications of Lemma B.83.
Lemma B.85.
(A) If $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$, where $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}$ is a complement-free CDNnormalized form, then either $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ or $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$.
(B) If $\pi^{\mathrm{cn}} \subseteq \bigvee_{i \in 1 . . n} \tau_{i}^{\mathrm{cn}}$ with a derivation of size $n$, where $\bigvee_{i \in 1 . . n} \tau_{i}^{\mathrm{cn}}$ is a complement-free DCNnormalized form, then either $\pi^{\mathrm{cn}} \subseteq \tau_{1}^{\mathrm{cn}}$ or $\pi^{\mathrm{cn}} \subseteq \bigvee_{i \in 2 . . n} \tau_{i}^{\mathrm{cn}}$ with a derivation of size $n$.

Only the proof for (A) is shown below. The proof for (B) is symmetric.
Proof. By induction on right-leaning $\subseteq^{\text {cdn }}$ derivations, where S-DistribCDN ◇ does not occur as the first premise of S-Trans in any of the judgements (in both the assumptions and conclusions). It is easy to see that we can rewrite any subderivations with S-DistribCdn∘ as the first premise of S-Trans into an equivalent one by applying S-Trans to the premises of S-DistribCdn∘ and the second premise of S-Trans, followed by an application of S-DistribCdn∘.

In the remainder of this proof, we abbreviate $\subseteq^{\mathrm{cdn}}$ as $\subseteq$.
Case S-Refl. Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\pi^{\mathrm{dn}}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-ToB. Then $\pi^{\mathrm{dn}}=\top$ and we have both $\tau_{1}^{\mathrm{dn}} \subseteq \top$ and $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \top$ by S-ToB.
Case S-ToB ↓ . Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\neg$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-Compl. Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\mathrm{T}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-Compl. Impossible since $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}$ is a complement-free CDN-normalized form.
Case S-NegInv. Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\neg \tau^{0}$ for some $\tau^{0}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-AndOr1. Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}$ is not an intersection, i.e., $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}$ and we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.

Case S-AndOr1d. Then $\pi^{\mathrm{dn}}=\tau_{k}^{\mathrm{dn}}$ for some $k \in\{\bar{i}\}$. If $k=1$, then we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ by S-Refl. Otherwise, we have $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ by S-AndOr1 D.
Case S-AndOr2. Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-AndOr2. Impossible since $\pi^{\mathrm{dn}}$ is not an intersection.
Case S-DistribCin . Then $\tau_{1}^{\mathrm{dn}}=\bigvee_{j} \tau_{j}^{\mathrm{n}}$ for some $\bar{\tau}_{j}^{\mathrm{n}}$. The premises of the rule are ${\overline{\tau_{j}^{\mathrm{n}} \wedge \bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}}}^{j}$, all with a derivation of size $n-1$. By IH on the premises, we have $\overline{\tau_{j}^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}} \text { or } \bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}}{ }^{j}$. If $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, then we have the result immediately. Otherwise, we have ${\overline{\tau_{j}^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}}}}^{j}$, which imply $\bigvee_{j} \tau_{j}^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$ by S-AndOr2, i.e., $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Case S-DistribCDN D. Then ${\overline{\tau_{i}^{\mathrm{dn}}=\tau^{\mathrm{n}} \vee \tau_{i^{\prime}}^{\mathrm{dn}}}}^{i \in 1 . . n}$ for some $\tau^{\mathrm{n}}$ and ${\overline{\tau_{i^{\prime}}^{\mathrm{dn}}}}^{i \in 1 . . n}$. The premises of the rule are $\tau^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}}$ and $\bigwedge_{i \in 1 . . n} \tau_{i^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$. By IH on the latter premise, we have $\tau_{1^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ or $\bigwedge_{i \in 2 . . n} \tau_{i^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n-1$. If $\tau_{1^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, then by S-AndOr2 ⋅ with $\tau^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}}$, we have $\tau_{1}^{\mathrm{dn}}=\tau^{\mathrm{n}} \vee \tau_{1^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$. If $\bigwedge_{i \in 2 . . n} \tau_{i^{\prime}}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, then by S-DistribCDN ? with $\tau^{\mathrm{n}} \subseteq \pi^{\mathrm{dn}}$, we have $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}}=\bigwedge_{i \in 2 . . n}\left(\tau^{\mathrm{n}} \vee \tau_{i^{\prime}}^{\mathrm{dn}}\right) \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$.
Case S-Trans. Then the premises of the rule are $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \tau^{\mathrm{cdn}}$ and $\tau^{\mathrm{cdn}} \subseteq \pi^{\mathrm{dn}}$ for some $\tau^{\mathrm{cdn}}$. By induction on the size of the former premise of S-Trans, denoted by $m$. Denote the inner induction hypothesis by $\mathrm{IH}^{\prime}$.
Cases (S-Refl, *). By IH on the latter premise.
Cases (S-ToB.). Then $\tau^{\mathrm{cdn}}=\top$. By S-ToB., we have both $\tau_{1}^{\mathrm{dn}} \subseteq \top$ and $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \top$. Then we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ and $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ by S-Trans with the latter premise $\top \subseteq \pi^{\mathrm{dn}}$.
Cases (S-ToB $>$, *). Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\neg \top$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Cases (S-Compl, *). Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\top$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Cases (S-Compl 2 , *). Impossible since $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}$ is a complement-free CDN-normalized form.
Cases (S-NegInv, *). Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}=\neg \tau^{0}$ for some $\tau^{0}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Cases (S-AndOr1, *). Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}$ is not an intersection, i.e., $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}$ and we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Cases (S-AndOr1), *). Then $\tau^{\mathrm{cdn}}=\bigwedge_{i^{\prime} \in S} \tau_{i^{\prime}}^{\mathrm{dn}}$ for some $S \subseteq\{\bar{i}\}$. If $1 \in S$, by IH on the latter premise, we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ or $\bigwedge_{i^{\prime} \in S \backslash\{1\}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n-1$. If $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, the result is immediate. If $\bigwedge_{i^{\prime} \in S \backslash\{1\}} \subseteq \pi^{\mathrm{dn}}$, then we have $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$ by S-Trans with S-AndOr1>. If $1 \notin S$, then $\bigwedge_{i \in 2 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$ follows from IH on the latter premise, followed by S-Trans with S-AndOr1>.
Cases (S-AndOr2, *). Then $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}}=\tau_{1}^{\mathrm{dn}}$, i.e., we have $\tau_{1}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$.
Cases (S-AndOr2 , *). Then $\tau^{\mathrm{cdn}}=\bigwedge_{j} \pi_{j}^{\mathrm{dn}}$ for some ${\overline{\pi_{j}^{\mathrm{dn}}}}^{j}$. The premises of the former rule are $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi_{j}^{\mathrm{dn}}{ }^{j}$. The latter premise is $\bigwedge_{j} \pi_{j}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, which implies $\pi_{k}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ for some $k \in\{\bar{j}\}$ with a derivation of size $n-1$ by repeated applications of IH , which implies $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ with a derivation of size $n$ and a former premise of size $m-1$ by S-Trans with $\bigwedge_{i \in 1 . . n} \tau_{i}^{\mathrm{dn}} \subseteq \pi_{k}^{\mathrm{dn}}$. The result then follows from $\mathrm{IH}^{\prime}$.
Cases (S-DistribCin∘, *). Impossible by assumption.

Corollary B.86.
(A) If $\bigwedge_{i} \tau_{i}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$, where $\bigwedge_{i} \tau_{i}^{\mathrm{dn}}$ is a complement-free CDN-normalized form, then $\tau_{k}^{\mathrm{dn}} \subseteq \pi^{\mathrm{dn}}$ for some $k \in\{\bar{i}\}$.
(B) If $\pi^{\mathrm{cn}} \subseteq \bigvee_{i} \tau_{i}^{\mathrm{cn}}$, where $\bigvee_{i} \tau_{i}^{\mathrm{cn}}$ is a complement-free DCN-normalized form, then $\pi^{\mathrm{cn}} \subseteq \tau_{k}^{\mathrm{cn}}$ for some $k \in\{\bar{i}\}$.
Proof. By repeated application of Lemma B.85.
Lemma B.87. $T^{\diamond} \subseteq^{\diamond} \tau$ is not derivable for $\tau \in\left\{\tau_{1} \rightarrow \tau_{2},\left\{x: \tau_{1}\right\}, \# C\right\}$.
Proof. By induction on $\subseteq^{\mathrm{cdn}}$ and $\subseteq^{\mathrm{dcn}}$ derivations respectively.

\section*{B. 11 Consistency of Subtyping}

The reason we can soundly define rules such as S-FunMrg, S-RcdMrg, and S-RcdTop is that they do not threaten any of the properties we actually need for the type soundness proofs. As a first step towards showing that, and in order to support the next important lemmas, we prove that subtyping is consistent.

Theorem B. 88 (Subtyping consistency). If $\Xi$ cons. and $\Xi \vdash \tau \leqslant \pi$, where:
$$
\begin{gathered}
\tau \in\left\{\perp, \top, \# C, \tau_{1} \rightarrow \tau_{2},\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}\right\} \\
\pi \in\left\{\perp, \top, \# C^{\prime}, \pi_{1} \rightarrow \pi_{2},\left\{x^{\prime}: \pi_{1}\right\}\right\}
\end{gathered}
$$
then exactly one of the following is true:
(a) $\tau=\perp$ or $\pi=\top$;
(b) $\tau=\# C$ and $\pi=\# C^{\prime}$ and $C^{\prime} \in \mathcal{S}(\# C)$;
(c) $\tau=\tau_{1} \rightarrow \tau_{2}$ and $\pi=\pi_{1} \rightarrow \pi_{2}$ and $\Xi \vdash \pi_{1} \leqslant \tau_{1}$ and $\Xi \vdash \tau_{2} \leqslant \pi_{2}$;
(d) $\tau=\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}$ and $\pi=\left\{x_{k}: \pi_{1}\right\}$ and $\Xi \vdash \tau_{k} \leqslant \pi_{1}$ for some $k$.

Proof. By Lemma B. 49 on the assumption, we have:
$$
\begin{equation*}
\triangleright \Xi \vdash \tau \leqslant \pi \tag{1}
\end{equation*}
$$

Then proceed by case analysis on $\tau$.
Case $\tau=\perp$. Then (a) is true and (b), (c), (d) are false.
Case $\tau=\mathrm{T}$. Then (b), (c), (d) are false. Since $\tau \cong \perp \vee \mathrm{T}$, by Lemma B. 89 on (1), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)}{\triangleright \Xi \vdash \top \preceq V_{j}^{D_{j}}}{ }^{j} \tag{2}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime}}}^{j}$ and ${\overline{D_{j}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$, where $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. By Lemma B.59, (3) implies:
$$
\begin{equation*}
{\overline{D_{j} \in\{\top, \nmid}}^{j} \tag{4}
\end{equation*}
$$

By Lemma B.22> on S-AndOr12•, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{5}
\end{equation*}
$$

By S-Trans on (5) and (2), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \pi \tag{6}
\end{equation*}
$$

Since $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subsetneq \perp \tag{7}
\end{equation*}
$$

Then (6) and (7) imply:
$$
\begin{equation*}
\pi \Phi \perp \tag{8}
\end{equation*}
$$

By Lemma B.82, (6) implies:
$$
\begin{equation*}
V_{k}^{D_{k}} \subseteq \pi \tag{9}
\end{equation*}
$$
for some $k$. By case analysis on the syntax of $V_{k}^{D_{k}}$ and the assumption on the form of $\pi$, (9) can only be derived when $\pi=\mathrm{T}$. Then we have $\pi=\mathrm{T}$, i.e., (a) is true.
Case $\tau=\# C$. Then (c), (d) are false. Since $\tau \cong \perp \vee \# C$, by Lemma B. 89 on (1), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)}{\triangleright \Xi \vdash \# C \leq V_{j}^{D_{j}}}{ }^{j} \tag{10}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime}}}^{j}$ and ${\overline{D_{j}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$, where $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. By Lemma B.59, (11) implies:
$$
\begin{equation*}
{\overline{D_{j} \in\left\{\# C_{1}, \# \ell_{2}, \top, \nmid\right.}}^{j} \tag{12}
\end{equation*}
$$
where $C_{1} \in \mathcal{S}(\# C)$ and $C_{2} \notin \mathcal{S}(\# C)$ and $C \notin \mathcal{S}\left(\# C_{2}\right)$. By Lemma B.22॰ on S-AndOr12•, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{13}
\end{equation*}
$$

By S-Trans on (13) and (10), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \pi \tag{14}
\end{equation*}
$$

Since $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subsetneq \perp \tag{15}
\end{equation*}
$$

Then (14) and (15) imply:
$$
\begin{equation*}
\pi \Phi \perp \tag{16}
\end{equation*}
$$

By Lemma B.82, (14) implies:
$$
\begin{equation*}
V_{k}^{D_{k}} \subseteq \pi \tag{17}
\end{equation*}
$$
for some $k$. By Lemma B.60, (17) implies either $\pi=\top$ or $V_{k}^{D_{k}}=\bigvee_{l} \pi$.
Case $\pi=\mathrm{T}$. Then (a) is true and (b) is false.
Case $\pi \neq \top$. Then we have:
$$
\begin{equation*}
\pi \cong \bigvee_{l} \pi=V_{k}^{D_{k}} \tag{18}
\end{equation*}
$$

By the syntax of $U^{\top}$ and $U^{\chi}$, we have:
$$
\begin{equation*}
D_{k} \notin\{\top, \notin\} \tag{19}
\end{equation*}
$$

Then (12) and (19) imply:
$$
\begin{equation*}
D_{k} \in\left\{\# C_{1}, \# \ell_{2}\right\} \tag{20}
\end{equation*}
$$

By case analysis on the assumption on the form of $\pi$, we have:
$$
\begin{equation*}
\pi=\# C_{1} \tag{21}
\end{equation*}
$$
where $C_{1} \in S(\# C)$. Then (b) is true and (a) is false.
Case $\tau=\tau_{1} \rightarrow \tau_{2}$. Then (b), (d) are false. Since $\tau \cong \perp \vee\left(\tau_{1} \rightarrow \tau_{2}\right)$, by Lemma B. 89 on (1), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)}{\triangleright \Xi \vdash \tau_{1} \rightarrow \tau_{2} \leq V_{j}^{D_{j}}}{ }^{j} \tag{22}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime}}}_{j}^{j}$ and ${\overline{D_{j}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$, where $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. By Lemma B.59, (23) implies:
$$
\begin{equation*}
{\overline{D_{j} \in\{\rightarrow, \top, \nmid\}}}^{j} \tag{24}
\end{equation*}
$$

By Lemma B.22> on S-AndOr12•, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{25}
\end{equation*}
$$

By S-Trans on (25) and (10), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \pi \tag{26}
\end{equation*}
$$

Since $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \varsubsetneqq \perp \tag{27}
\end{equation*}
$$

Then (26) and (27) imply:
$$
\begin{equation*}
\pi \subseteq \perp \tag{28}
\end{equation*}
$$

By Lemma B.82, (26) implies:
$$
\begin{equation*}
V_{k}^{D_{k}} \subseteq \pi \tag{29}
\end{equation*}
$$
for some $k$. By Lemma B.60, (29) implies either $\pi=\top$ or $V_{k}^{D_{k}}=\bigvee_{l} \pi$.
Case $\pi=\mathrm{T}$. Then (a) is true and (c) is false.
Case $\pi \neq \top$. Then we have:
$$
\begin{equation*}
\pi \cong \bigvee_{l} \pi=V_{k}^{D_{k}} \tag{30}
\end{equation*}
$$

By the syntax of $U^{\top}$ and $U^{\chi}$, we have:
$$
\begin{equation*}
D_{k} \notin\{T, \not \subset\} \tag{31}
\end{equation*}
$$

Then (24) and (31) imply:
$$
\begin{equation*}
D_{k}=\rightarrow \tag{32}
\end{equation*}
$$

By case analysis on the assumption on the form of $\pi$, we have:
$$
\begin{equation*}
\pi=\pi_{1} \rightarrow \pi_{2} \tag{33}
\end{equation*}
$$

Then (23) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash \tau_{1} \rightarrow \tau_{2} \leq \bigvee_{l} \pi_{1} \rightarrow \pi_{2} \tag{34}
\end{equation*}
$$

By case analysis on the $\leq$ rules, (34) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash \tau_{1} \rightarrow \tau_{2} \leq\left(\bigwedge_{l} \pi_{1}\right) \rightarrow\left(\bigvee_{l} \pi_{2}\right) \tag{35}
\end{equation*}
$$

Again by case analysis on the $\leq$ rules, (35) implies:
$$
\begin{align*}
& \Xi \vdash \bigwedge_{l} \pi_{1} \leqslant \tau_{1}  \tag{36}\\
& \Xi \vdash \tau_{2} \leqslant \bigvee_{l} \pi_{2} \tag{37}
\end{align*}
$$

By S-Trans with S-AndOr2 o on S-Refl, (36) and (37) imply:
$$
\begin{align*}
& \Xi \vdash \pi_{1} \leqslant \tau_{1}  \tag{38}\\
& \Xi \vdash \tau_{2} \leqslant \pi_{2} \tag{39}
\end{align*}
$$

Then (c) is true and (a) is false.
Case $\tau=\left\{\bar{x}_{i}: \tau_{i}{ }^{i}\right\}$. Then (b), (c) are false. Since $\tau \cong \bigwedge_{i}\left(\perp \vee\left\{x_{i}: \tau_{i}\right\}\right)$, by Lemma B. 89 on (1), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)}{\triangleright \Xi \vdash\left\{x_{k_{j}}: \tau_{k_{j}}\right\} \leq V_{j}^{D_{j}}}{ }^{j} \tag{40}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime}}}^{j}$ and ${\overline{D_{j}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$ and ${\overline{k_{j}}}^{j}$, where $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. By Lemma B.59, (41) implies:
$$
\begin{equation*}
{\overline{D_{j} \in\left\{x_{k_{j}}, \top, \nmid\right\}}}^{j} \tag{42}
\end{equation*}
$$

By Lemma B.22> on S-AndOr12•, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{43}
\end{equation*}
$$

By S-Trans on (43) and (10), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \pi \tag{44}
\end{equation*}
$$

Since $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free, we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subsetneq \perp \tag{45}
\end{equation*}
$$

Then (44) and (45) imply:
$$
\begin{equation*}
\pi \Phi \perp \tag{46}
\end{equation*}
$$

By Lemma B.82, (44) implies:
$$
\begin{equation*}
V_{k}^{D_{k}} \subseteq \pi \tag{47}
\end{equation*}
$$
for some $k$. By Lemma B.60, (47) implies either $\pi=\top$ or $V_{k}^{D_{k}}=\bigvee_{l} \pi$.
Case $\pi=\mathrm{T}$. Then (a) is true and (d) is false.
Case $\pi \neq \top$. Then we have:
$$
\begin{equation*}
\pi \cong \bigvee_{l} \pi=V_{k}^{D_{k}} \tag{48}
\end{equation*}
$$

By the syntax of $U^{\top}$ and $U^{\chi}$, we have:
$$
\begin{equation*}
D_{k} \notin\{T, \not \subset\} \tag{49}
\end{equation*}
$$

Then (42) and (49) imply:
$$
\begin{equation*}
D_{k}=x_{k_{k}} \tag{50}
\end{equation*}
$$

By case analysis on the assumption on the form of $\pi$, we have:
$$
\begin{equation*}
\pi=\left\{x_{k_{k}}: \pi_{1}\right\} \tag{51}
\end{equation*}
$$

Then (41) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash\left\{x_{k_{k}}: \tau_{k_{k}}\right\} \leq \bigvee_{l}\left\{x_{k_{k}}: \pi_{1}\right\} \tag{52}
\end{equation*}
$$

By case analysis on the $\leq$ rules, (52) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash\left\{x_{k_{k}}: \tau_{k_{k}}\right\} \leq\left\{x_{k_{k}}: \bigvee_{l} \pi_{1}\right\} \tag{53}
\end{equation*}
$$

Again by case analysis on the $\leq$ rules, (53) implies:
$$
\begin{equation*}
\Xi \vdash \tau_{k_{k}} \leqslant \bigvee_{l} \pi_{1} \tag{54}
\end{equation*}
$$

By S-Trans with S-AndOr2•on S-Refl, (54) implies:
$$
\begin{equation*}
\Xi \vdash \tau_{k_{k}} \leqslant \pi_{1} \tag{55}
\end{equation*}
$$

Then (d) is true and (a) is false.

\section*{Lemma B. 89 (Subtyping consistency).}
(A) If $\triangleright \Sigma \vdash \tau \leqslant \pi$ and $\tau \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right)$, where the following are true:
- $\bigwedge_{i} U_{i}^{C_{i}}$ is a complement-free CDN-normalized form
- → $\notin\left\{{\overline{C_{i}}}^{i}\right\}$ or $\longrightarrow \notin\left\{{\overline{C_{i}}}^{i}\right\}$
- $\forall x \in\left\{{\overline{C_{i}}}^{i}\right\} . x \notin\left\{{\overline{C_{i}}}^{i}\right\}$
- $\forall \# C \in\left\{{\overline{C_{i}}}^{i}\right\}$. $\# \ell \notin\left\{{\overline{C_{i}}}^{i}\right\}$
- $\forall \# C_{1} \in\left\{{\overline{C_{i}}}^{i}\right\}, \# C_{2} \in\left\{{\overline{C_{i}}}^{i}\right\} . C_{1} \in \mathcal{S}\left(\# C_{2}\right)$ or $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$
- $\left|\left\{\chi \mid \chi \in\left\{{\overline{C_{i}}}^{i}\right\}\right\}\right| \leqslant 1$
- $\left|\left\{\chi \mid \chi \in\left\{{\overline{C_{i}}}^{i}\right\}\right\}\right|=0$ or $\longrightarrow \notin\left\{{\overline{C_{i}}}^{i}\right\}$
then there exists some ${\overline{\pi_{j}^{\prime}}}^{j}$ and $\overline{D_{j} \in\left\{{\overline{C_{i}}}^{i}\right\} \cup\{\mathrm{T}, \nmid\} \cup\left\{\bar{x}^{\chi \notin\left\{{\overline{C_{i}}}^{i}\right\}}\right\} \cup\left\{\overline{\# \ell^{\# C \notin\left\{{\overline{C_{i}}}^{j}\right\}}}{ }^{j} \text { and }\right.} {\overline{V_{j}^{D_{j}}}}^{j}$ such that $\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)$ and $\bigwedge_{j} V_{j}^{D_{j}}$ is a complement-free CDN-normalized form and $\overline{\triangleright \Sigma \vdash \bigwedge_{i \in S_{j}} U_{i}^{C_{i}} \leq V_{j}^{D_{j}}}{ }^{j}$ for some $\bar{S}_{j}{ }^{j}$.
(B) If $\triangleright \Sigma \vdash \tau \leqslant \pi$ and $\pi \cong \bigvee_{j}\left(\pi_{j}^{\prime} \wedge Y_{j}^{D_{j}}\right)$, where the following are true:
- $\bigvee_{j} Y_{j}^{D_{j}}$ is a complement-free DCN-normalized form
- → $\notin\left\{{\overline{D_{j}}}^{j}\right\}$ or $\longrightarrow \notin\left\{{\overline{D_{j}}}^{j}\right\}$
- $\forall x \in\left\{{\overline{D_{j}}}^{j}\right\} . \chi \notin\left\{{\overline{D_{j}}}^{j}\right\}$
- $\forall \# C \in\left\{{\overline{D_{j}}}^{j}\right\} . \# C \notin\left\{{\overline{D_{j}}}^{j}\right\}$
- $\forall \# \ell_{1} \in\left\{{\overline{C_{i}}}^{i}\right\}, \# \ell_{2} \in\left\{{\overline{C_{i}}}^{i}\right\} . C_{1} \in \mathcal{S}\left(\# C_{2}\right)$ or $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$
- $\left|\left\{x \mid x \in\left\{{\overline{D_{j}}}^{j}\right\}\right\}\right| \leqslant 1$
- $\left|\left\{x \mid x \in\left\{{\overline{D_{j}}}^{j}\right\}\right\}\right|=0$ or $\rightarrow \notin\left\{{\overline{D_{j}}}^{j}\right\}$
then there exists some ${\overline{\tau_{i}^{\prime}}}^{i}$ and $\overline{C_{i} \in\left\{{\overline{D_{j}}}^{j}\right\} \cup\{\perp, \not X\} \cup\left\{\bar{\chi}^{x \notin\left\{{\overline{D_{j}}}^{j}\right\}}\right\} \cup\left\{\overline{\# C}^{\# \notin \notin\left\{{\overline{D_{j}}}^{j}\right\}}\right\}}{ }^{i}$ and ${\overline{X_{i}^{C_{i}}}}^{i}$ such that $\tau \cong \bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right)$ and $\bigvee_{i} X_{i}^{C_{i}}$ is a complement-free DCN-normalized form and $\overline{\triangleright \Sigma \vdash X_{i}^{C_{i}} \leq \bigvee_{j \in S_{i}} Y_{j}^{D_{j}}}{ }^{i}$ for some ${\overline{S_{i}}}^{i}$.
Only the proof for (A) is shown below. The proof for (B) is mostly symmetric.

Proof. By Lemma B.67, there exists some $\tau^{\mathrm{cdn}}$ and $\pi^{\mathrm{cdn}}$ such that $\tau \cong \tau^{\mathrm{cdn}}$ and $\pi \cong \pi^{\mathrm{cdn}}$. Then by Lemma B.71, we only need to consider CDN-normalized derivations for $\tau^{\mathrm{cdn}} \leqslant^{\mathrm{cdn}} \pi^{\mathrm{cdn}}$, and the result would also apply to the original derivation for $\tau \leqslant \pi$. By induction on unassuming CDN-normalized subtyping derivations.

Notice that the property to prove has a conclusion that can itself be used as a hypothesis for another application of the property. When proving (A), we consider the leftmost rule application in a transitivity chain, show the property for it, and this allows us to apply the induction hypothesis on the rest of the chain; this works even if the chain is of length 1 (with no uses of S-Trans). When proving (B), we proceed in the same way but from the right. So we do not have to consider uses of S-Trans explicitly, and only consider uses of the other rules here:
Case S-Refl. Immediate since $\tau \cong \pi$. Pick ${\overline{\pi_{i}^{\prime}=\tau_{i}^{\prime}}}^{i}$ and ${\overline{V_{i}^{D_{i}}=U_{i}^{C_{i}}}}^{i}$. Then $\pi \cong \bigwedge_{i}\left(\pi_{i}^{\prime} \vee V_{i}^{D_{i}}\right)$ and ${\overline{U_{i}^{C_{i}} \leq V_{i}^{D_{i}}}}^{i}$.
Case S-ToB. Then $\pi=$ T. Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=V_{1}^{\top}=$ T. Then $\pi \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and ${\overline{U_{i}^{C_{i}}} \leq V_{1}^{D_{1}}}^{i}$.
Case S-ToB ᄀ . Then $\tau=\neg \top$. So $\pi \cong \pi \vee \tau \cong \pi \vee \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right)$. By distributivity, we have $\pi \cong \bigwedge_{i}\left(\pi \vee \tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right)$. Pick $\overline{\pi_{i}^{\prime}=\pi \vee \tau_{i}^{\prime}}$ and ${\overline{V_{i}^{D_{i}}=U_{i}^{C_{i}}}}^{i}$. Then $\pi \cong \bigwedge_{i}\left(\pi_{i}^{\prime} \vee V_{i}^{D_{i}}\right)$ and ${\overline{U_{i}^{C_{i}} \leq V_{i}^{D_{i}}}}^{i}$.
Cases S-Complo. Immediate since $\tau \cong \pi$. Proceed with the same reasoning as case S-Refl.
Case S-NegInv. Then $\tau=\neg \tau^{\prime}$ and $\pi=\neg \pi^{\prime}$ for some $\tau^{\prime}$ and $\pi^{\prime}$. The premise of the rule is:
$$
\begin{equation*}
\pi^{\prime} \leqslant \tau^{\prime} \tag{1}
\end{equation*}
$$

From the assumptions, we have:
$$
\begin{array}{ll} 
& \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \cong \neg \tau^{\prime} \\
\text { i.e., } \quad \tau^{\prime} \cong \bigvee_{i}\left(\neg \tau_{i}^{\prime} \wedge \neg U_{i}^{C_{i}}\right) \tag{2}
\end{array}
$$

Let $\overline{X_{i}^{\ell i}=\neg U_{i}^{C_{i}}}{ }^{i}$ (Lemma B.57). By IH on (1) and (2), we have:
$$
\begin{equation*}
{\frac{\pi^{\prime} \cong \bigvee_{j}\left(\pi_{j}^{\prime \prime} \wedge Y_{j}^{D_{j}^{\prime \prime}}\right)}{\triangleright \Sigma \vdash Y_{j}^{D_{j}^{\prime \prime}} \leq \bigvee_{i \in S_{j}} X_{i}^{\ell_{i}}}}_{j}^{j} \tag{3}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime \prime}}}^{j}$ and ${\overline{Y_{j}^{D_{j}^{\prime \prime}}}}^{j}$ and $\bar{S}_{j}{ }^{j}$. (3) implies:
$$
\begin{equation*}
\pi \cong \neg \bigvee_{j}\left(\pi_{j}^{\prime \prime} \wedge Y_{j}^{D_{j}^{\prime \prime}}\right) \cong \bigwedge_{j}\left(\neg \pi_{j}^{\prime \prime} \vee \neg Y_{j}^{D_{j}^{\prime \prime}}\right) \tag{5}
\end{equation*}
$$

Pick ${\overline{\pi_{j}^{\prime}=\neg \pi_{j}^{\prime \prime}}}_{j}^{j},{\overline{D_{j}=D_{j}^{\prime \prime}}}_{j}^{j}$, and ${\overline{V_{j}^{D_{j}}=\neg Y_{j}^{D_{j}^{\prime \prime}} \text { or } \neg V_{j}^{D_{j}}=Y_{j}^{D_{j}^{\prime \prime}}}}^{j}$ (Lemma B.57). Then we have:
$$
\begin{equation*}
\pi \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{6}
\end{equation*}
$$

Then (4) implies:
$$
\begin{equation*}
\overline{\triangleright \Sigma \vdash \bigwedge_{i \in S_{j}} U_{i}^{C_{i}} \leq V_{j}^{D_{j}}}{ }^{j} \tag{7}
\end{equation*}
$$

Case S-AndOr1. $\pi=\tau \vee \pi^{\prime}$ for some $\pi^{\prime}$. Then $\pi \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \vee \pi^{\prime} \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee \pi^{\prime} \vee U_{i}^{C_{i}}\right)$.
Pick ${\overline{\pi_{i}^{\prime}=\tau_{i}^{\prime} \vee \pi^{\prime}}}^{i}$ and ${\overline{V_{i}^{D_{i}}=U_{i}^{C_{i}}}}^{i}$. Then $\pi \cong \bigwedge_{i}\left(\pi_{i}^{\prime} \vee V_{i}^{D_{i}}\right)$ and ${\overline{U_{i}^{C_{i}}} \leq V_{i}^{D_{i}}}^{i}$.
Case S-AndOr1 D. $\tau=\pi \wedge \tau^{\prime}$ for some $\tau^{\prime}$. Then from the assumption, we have:
$$
\begin{equation*}
\tau=\pi \wedge \tau^{\prime} \cong \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \tag{8}
\end{equation*}
$$

By Lemma B.22• on S-Refl and (8), we have:
$$
\begin{gather*}
\left(\pi \wedge \neg \tau^{\prime}\right) \vee\left(\pi \wedge \tau^{\prime}\right) \cong\left(\pi \wedge \neg \tau^{\prime}\right) \vee \bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \\
\text { i.e., } \quad \pi \cong \bigwedge_{i}\left(\left(\pi \wedge \neg \tau^{\prime}\right) \vee \tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \tag{9}
\end{gather*}
$$

Pick $\overline{\pi_{i}^{\prime}=\left(\pi \wedge \neg \tau^{\prime}\right) \vee \tau_{i}^{\prime}}$ and $\overline{V_{i}^{D_{i}}=U_{i}^{C_{i}}}{ }^{i}$. Then $\pi \cong \bigwedge_{i}\left(\pi_{i}^{\prime} \vee V_{i}^{D_{i}}\right)$ and $\overline{U_{i}^{C_{i}} \leq V_{i}^{D_{i}}}{ }^{i}$.
Case S-AndOr2- By induction on the number of premises. Denote the inner induction hypothesis as $\mathrm{IH}^{\prime}$. We have $\tau=\bigvee_{h \in 1 . . n} \tau_{h}^{\mathrm{n}}$ for some ${\overline{\tau_{h}^{\mathrm{n}}}}^{h \in 1 . . n}$. Let $\tau_{2}^{\mathrm{dn}}=\bigvee_{h \in 2 . . n} \tau_{h}^{\mathrm{n}}$, then $\tau=\tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}$. The premises of the rule are:
$$
\begin{equation*}
{\overline{\tau_{h}^{\mathrm{n}} \leqslant \pi}}^{h \in 1 . . n} \tag{10}
\end{equation*}
$$

By S-AndOr2•on (10) for $h \in 2 . . n$, we have:
$$
\begin{equation*}
\tau_{2}^{\mathrm{dn}} \leqslant \pi \tag{11}
\end{equation*}
$$
with the same size as the current derivation and one fewer premise. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}} \tag{12}
\end{equation*}
$$

By S-Trans with Lemma B.22> on S-AndOr12•, (12) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}} \tag{13}
\end{equation*}
$$

By Corollary B.86, (13) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}} \tag{14}
\end{equation*}
$$
for some $k$.
Case $C_{k}=B$. If $C_{k}=B$ for some $B$, then by Lemma B.54, (14) implies:
$$
\begin{equation*}
{\overline{U_{l}^{\prime C^{\prime}} l \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}}}^{l} \tag{15}
\end{equation*}
$$
where $U_{k}^{C_{k}}=\bigvee_{l} U_{l}^{\prime^{\prime}}{ }_{l}$ and $\overline{{U^{\prime}}_{l}^{\prime^{\prime}}}{ }_{l}^{l}$ are not unions. By Lemma B.83, (15) implies either ${\overline{U^{\prime} C^{\prime}}{ }_{l} \subseteq \tau_{1}^{\mathrm{n}} \text { or } U^{\prime^{\prime}{ }_{l}}{ }_{l} \subseteq \tau_{2}^{\mathrm{dn}}}^{l}$ or $\mathrm{T} \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}$.
Case ${\overline{U^{\prime} C^{\prime} l} \subseteq \tau_{1}^{\mathrm{n}} \text { or } U^{\prime^{\prime} C^{\prime} l}{ }_{l} \tau_{2}^{\mathrm{dn}}}^{l}$. By S-AndOR2., we have:
$$
\begin{align*}
U^{1 C^{1}} & :=\bigvee_{l \mid U_{l}^{\prime^{\prime}} l \subseteq \tau_{1}^{\mathrm{n}}} U_{l}^{\prime C^{\prime} l} \subseteq \tau_{1}^{\mathrm{n}}  \tag{16}\\
U^{2 C^{2}} & :=\bigvee_{l \mid U_{l}^{\prime} C_{l}^{\prime}{ }_{l} \tau_{2}^{\mathrm{dn}}} U_{l}^{\prime C^{\prime} l} \subseteq \tau_{2}^{\mathrm{dn}} \tag{17}
\end{align*}
$$

By S-AndOr2 ⋅ with S-Refl, (16) and (17) imply:
$$
\begin{gather*}
\tau_{1}^{\mathrm{n}} \vee U^{1 C^{1}} \subseteq \tau_{1}^{\mathrm{n}}  \tag{18}\\
\tau_{2}^{\mathrm{dn}} \vee U^{2} C^{2} \subseteq \tau_{2}^{\mathrm{dn}} \tag{19}
\end{gather*}
$$

Since we have the other direction by S-AndOr11', (18) and (19) imply:
$$
\begin{align*}
\tau_{1}^{\mathrm{n}} & \cong \tau_{1}^{\mathrm{n}} \vee U^{1} C^{1}  \tag{20}\\
\tau_{2}^{\mathrm{dn}} & \cong \tau_{2}^{\mathrm{dn}} \vee U^{2} C^{2} \tag{21}
\end{align*}
$$

Then by IH on the (10) for $h=1$ and (20), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{p}\left(\pi_{p}^{1} \vee V_{p}^{1^{D_{p}^{1} p}}\right)}{\triangleright \Sigma \vdash U^{1^{1}} \leq V_{p}^{1^{D_{p}^{1}} p}} \tag{22}
\end{equation*}
$$

By $\mathrm{IH}^{\prime}$ on (11) and (21), we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{q}\left(\pi_{q}^{2} \vee V_{q}^{2^{D^{2}} q}\right)}{\triangleright \Sigma \vdash U^{2 C^{2}} \leq V^{2^{D^{2}{ }^{q}}}{ }_{q}^{q}} \tag{24}
\end{equation*}
$$

By distributivity, (22) and (24) imply:
$$
\begin{equation*}
\pi \cong \bigwedge_{p, q}\left(\pi_{p}^{1} \vee \pi_{q}^{2} \vee V_{p}^{1 D_{p}^{1}}{ }_{p} \vee V_{q}^{2 D^{2}}{ }_{q}\right) \tag{26}
\end{equation*}
$$

For each pair $(p, q)$, we pick $\pi_{p q}^{\prime}$ and $V_{p q}^{D_{p q}}$ as follows:
- If $D^{1}{ }_{p} \in\{\mathrm{~T}, \nmid\}$, pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2} \vee V^{2}{ }_{q}^{D^{2}}{ }_{q}$ and $V_{p q}^{D_{p q}}=V^{1}{ }_{p}{ }^{1}{ }_{p}$. Then $\triangleright \Sigma \vdash U_{k}^{C_{k}} \leq V_{p q}^{D_{p q}}$.
- If $D^{2}{ }_{q} \in\{\top, \nmid\}$, pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2} \vee V_{p}^{1 D^{1}{ }_{p}}$ and $V_{p q}^{D_{p q}}=V^{2}{ }_{q}^{D^{2}}{ }_{q}$. Then $\triangleright \Sigma \vdash U_{k}^{C_{k}} \leq V_{p q}^{D_{p q}}$.
- If $D^{1}{ }_{p} \notin\{\top, \notin\}$ and $D^{2}{ }_{q} \notin\{\top, \notin\}$ and $D^{1}{ }_{p} \neq D^{2}{ }_{q}$, then we have at least one of the following by Lemma B. 59 (note that since $C_{k}=B$, we have $C^{1}=B^{1}$ and $C^{2}=B^{2}$ for some $B^{1}$ and $B^{2}$ ):
- $D^{1}{ }_{p}=C^{1}$ and $D^{2}{ }_{q}=C^{2}$, which implies $C^{1} \neq C^{2}$. Since $U_{k}^{C_{k}} \cong U^{1 C^{1}} \vee U^{2} C^{2}$, we have $C_{k}=\top$ and $\left(C^{1}, C^{2}\right) \in\left\{\left(x, y^{\neq x}\right),(x, \rightarrow),(\rightarrow, x)\right\}$ for some $x$ and $y$. Then $V^{1^{D}}{ }_{p}{ }^{1}{ }_{p} \vee V^{2}{ }_{q}^{D^{2}}{ }_{q} \cong \pi_{p q}^{3} \vee V_{p q}^{\top}$ for some $\pi_{p q}^{3}$ and $V_{p q}^{\top}$. Then we can pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2} \vee \pi_{p q}^{3}$ and $V_{p q}^{D_{p q}}=V_{p q}^{\top}$, where we have $\triangleright \Sigma \vdash U_{k}^{C_{k}} \leq V_{p q}^{D_{p q}}$.
- $C^{1}=\# C_{1}$ and $D^{1}{ }_{p}=\# C_{2}$, where $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$. Since $U_{k}^{C_{k}} \cong U^{1} C^{1} \vee U^{2} C^{2}$, we have $C_{k}=C^{1}=C^{2}=\# C_{1}$ Then we can pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2} \vee V^{2}{ }_{q}^{D^{2} q}$ and $V_{p q}^{D_{p q}}=V^{1}{ }_{p}^{D^{1}{ }_{p}}$, where we have $\triangleright \Sigma \vdash U_{k}^{C_{k}} \leq V_{p q}^{D_{p q}}$.
- $C^{1}=\# C_{1}$ and $D^{1}{ }_{p}=\# \ell_{2}$, where $C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$ and $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$. Proceed similarly as above.
- $C^{2}=\# C_{1}$ and $D^{2}{ }_{q}=\# C_{2}$, where $C_{2} \in \mathcal{S}\left(\# C_{1}\right)$. Since $U_{k}^{C_{k}} \cong U^{1} C^{1} \vee U^{2} C^{2}$, we have $C_{k}=C^{1}=C^{2}=\# C_{1}$ Then we can pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2} \vee V^{1^{D^{1}}{ }_{p}}$ and $V_{p q}^{D_{p q}}=V^{2 D^{2}{ }_{q}}$, where we have $\triangleright \Sigma \vdash U_{k}^{C_{k}} \leq V_{p q}^{D_{p q}}$.
- $C^{2}=\# C_{1}$ and $D^{2}{ }_{q}=\# \ell_{2}$, where $C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$ and $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$. Proceed similarly as above.
- If $D^{1}{ }_{p}=D^{2}{ }_{q} \notin\{\top, \nmid\}$, then we have $C^{1}=C^{2}=D^{1}{ }_{p}=D^{2}{ }_{q}$. Then $U_{k}^{C_{k}} \cong U^{1 C^{1}} \vee U^{2} C^{2}$ and $U^{1} C^{1} \leq V^{1}{ }_{p}^{D^{1}}{ }_{p}$ and $U^{2} C^{2} \leq V^{2}{ }_{q}^{D^{2}}{ }_{q}$ imply $U_{k} C_{k} \leq V^{1}{ }_{p}^{D^{1}}{ }_{p} \vee V^{2}{ }_{q}^{D^{2}}{ }_{q}$, so we can pick $\pi_{p q}^{\prime}=\pi_{p}^{1} \vee \pi_{q}^{2}$ and $V_{p q}^{D_{p q}}=V^{1}{ }_{p}^{D^{1}}{ }_{p} \vee V^{2}{ }_{q}^{D^{2}}{ }_{q}$.
Then we have:
$$
\begin{equation*}
\frac{\pi \cong \bigwedge_{p, q}\left(\pi_{p q}^{\prime} \vee V_{p q}^{D_{p q}}\right)}{D_{p q} \in\left\{{\overline{C_{i}}}^{i}\right\} \cup\left\{\top, \not\lfloor \} \cup\left\{\bar{x}^{\chi \notin\left\{{\overline{C_{i}}}^{i}\right\}}\right\} \cup\left\{\overline{\# \ell}^{\# C \notin\left\{{\overline{C_{i}}}^{i}\right\}}\right\}\right.} \mathrm{p,q} \tag{27}
\end{equation*}
$$

The conditions on $D_{p q}$ in (28) ensures that we can rewrite $\bigwedge_{p, q}\left(\pi_{p q}^{\prime} \vee V_{p q}^{D_{p q}}\right)$ to an equivalent complement-free form, where the $\leq$ relation is still satisfyable.
Case $\mathrm{T} \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}$. By Lemma B.62, we have $V^{D} \subseteq \pi$ for some $V^{D}$ and $D \in\{\top, \nvdash\}$. Then we can pick $\pi_{1}^{\prime}=\pi$ and $V_{1}^{D_{1}}=V^{D}$, which indeed satisfies $\pi \cong \pi_{1} \vee V_{1}^{D_{1}}$ and $U_{k}^{C_{k}} \leq V_{1}^{D_{1}}$.
Case $C_{k}=B$. If $C_{k}=B$ for some $B$, then we proceed symmetrically to the case above on the negation-inversion of $U_{k}^{C_{k}} \subseteq \tau_{1}^{\mathrm{n}} \vee \tau_{2}^{\mathrm{dn}}$, i.e., $\tau_{1}^{\prime \mathrm{n}} \vee \tau_{2}^{\prime \mathrm{cn}} \subseteq X_{k}^{C / k}$ for some $\tau_{1}^{\prime \mathrm{n}}$ and $\tau_{2}^{\prime \mathrm{cn}}$ and $X_{k}^{G k}$, and finally apply negation-inversion again to obtain the desired result.
Case S-AndOr2 . Then $\pi=\bigwedge_{h} \pi_{h}^{\mathrm{dn}}$ for some ${\overline{\pi_{h}^{\mathrm{dn}}}}^{h}$. The premises are ${\overline{\tau \leqslant \pi_{h}^{\mathrm{dn}}}}^{h}$. By IH on each premise, we have $\pi_{h}^{\mathrm{dn}} \cong \bigwedge_{p_{h}}\left(\pi_{p_{h}}^{h} \vee V^{h}{ }_{p_{h}}^{D_{p_{h}}^{h}}\right)$ and ▷ $\overline{\Sigma \vdash U_{k_{p_{h}}^{h}}^{C_{k_{h}^{h}}}} \leq V^{h}{ }_{p_{h}}^{D_{p_{h}}^{h}} p_{h}$ for some $\overline{\pi_{p_{h}}^{h}} p_{h}$ and $\overline{V^{h}{ }_{p_{h}}^{D_{p_{h}}^{h}}} p_{h}$ and $\overline{k_{p_{h}}^{h}} p_{h}$. Then we have $\pi \cong \bigwedge_{h} \bigwedge_{p_{h}}\left(\pi_{p_{h} \vee}^{h} \vee V_{p_{h}}^{h}{ }_{p_{h}}^{h}\right)$.
Cases S-DistribCdn∘。Similar to case S-AndOr2⋅.
Case S-RcoDepth. Then $\tau=\left\{x: \tau_{1}\right\}$ and $\pi=\left\{x: \pi_{1}\right\}$ for some $\tau_{1}$ and $\pi_{1}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\left\{x: \tau_{1}\right\} \tag{30}
\end{equation*}
$$

By S-Trans with Lemma B.22↓ on S-AndOr12•, (30) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq\left\{x: \tau_{1}\right\} \tag{31}
\end{equation*}
$$

By Lemma B.82, (31) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq\left\{x: \tau_{1}\right\} \tag{32}
\end{equation*}
$$
for some $k$. By Lemma B.60, (32) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l}\left\{x: \tau_{1}\right\} \tag{33}
\end{equation*}
$$

The premise of the rule is:
$$
\begin{equation*}
\triangleright \Sigma \vdash \tau_{1} \leqslant \pi_{1} \tag{34}
\end{equation*}
$$

By the definition of $\leq$, (34) implies:
$$
\begin{align*}
& \triangleright \Sigma \vdash\left\{x: \tau_{1}\right\} \leq\left\{x: \pi_{1}\right\} \\
\text { i.e., } & \triangleright \Sigma \vdash U_{k}^{C_{k}} \leq\left\{x: \pi_{1}\right\} \tag{35}
\end{align*}
$$

So we can pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\left\{x: \pi_{1}\right\}$, which indeed yields $\pi=\left\{x: \pi_{1}\right\} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$. Case S-RcoMrg. Then $\tau=\left\{x: \tau_{1} \vee \tau_{2}\right\}$ and $\pi=\left\{x: \tau_{1}\right\} \vee\left\{x: \tau_{2}\right\}$ for some $\tau_{1}$ and $\tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\left\{x: \tau_{1} \vee \tau_{2}\right\} \tag{36}
\end{equation*}
$$

By S-Trans with Lemma B.22d on S-AndOr12•, (36) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq\left\{x: \tau_{1} \vee \tau_{2}\right\} \tag{37}
\end{equation*}
$$

By Lemma B.82, (37) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq\left\{x: \tau_{1} \vee \tau_{2}\right\} \tag{38}
\end{equation*}
$$
for some $k$. By Lemma B.60, (38) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l}\left\{x: \tau_{1} \vee \tau_{2}\right\} \tag{39}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\left\{x: \tau_{1}\right\} \vee\left\{x: \tau_{2}\right\}$, which indeed satisfies $\pi=\left\{x: \tau_{1}\right\} \vee\{x: \left.\tau_{2}\right\} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $U_{k}^{C_{k}} \leq V_{1}^{D_{1}}$.
Case S-RcoMrg. Then $\tau=\left\{x: \tau_{1}\right\} \wedge\left\{x: \tau_{2}\right\}$ and $\pi=\left\{x: \tau_{1} \wedge \tau_{2}\right\}$ for some $\tau_{1}$ and $\tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\left\{x: \tau_{1}\right\} \wedge\left\{x: \tau_{2}\right\} \tag{40}
\end{equation*}
$$

By S-Trans with Lemma B.22d on S-AndOr12., (40) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq\left\{x: \tau_{1}\right\} \wedge\left\{x: \tau_{2}\right\} \tag{41}
\end{equation*}
$$

Let $l$ range from 1 to 2. By Lemma B.54, (41) implies:
$$
\begin{equation*}
{\overline{\bigwedge_{i} U_{i}^{C_{i}} \subseteq\left\{x: \tau_{l}\right\}}}^{l} \tag{42}
\end{equation*}
$$

By Lemma B.82, (42) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}} \subseteq\left\{x: \tau_{l}\right\}}}^{l} \tag{43}
\end{equation*}
$$
for some $\bar{k}_{l}^{l}$. By Lemma B.60, (43) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}}=\bigvee_{l_{l}}\left\{x: \tau_{l}\right\}}^{l} \tag{44}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\left\{x: \tau_{1} \wedge \tau_{2}\right\}$, which indeed satisfies $\pi=\left\{x: \tau_{1} \wedge \tau_{2}\right\} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $\bigwedge_{l} U_{k_{l}}^{C_{k_{l}}} \leq V_{1}^{D_{1}}$.
Case S-RcoMrgNegInv. Then $\tau=\neg\left\{x: \tau_{1}\right\} \wedge \neg\left\{x: \tau_{2}\right\}$ and $\pi=\neg\left\{x: \tau_{1} \vee \tau_{2}\right\}$ for some $\tau_{1}$ and $\tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\neg\left\{x: \tau_{1}\right\} \wedge \neg\left\{x: \tau_{2}\right\} \tag{45}
\end{equation*}
$$

By S-Trans with Lemma B.22. on S-AndOr12., (45) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left\{x: \tau_{1}\right\} \wedge \neg\left\{x: \tau_{2}\right\} \tag{46}
\end{equation*}
$$

Let $l$ range from 1 to 2. By Lemma B.54, (46) implies:
$$
\begin{equation*}
{\overline{\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left\{x: \tau_{l}\right\}}}^{l} \tag{47}
\end{equation*}
$$

By Corollary B.86, (47) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}} \subseteq \neg\left\{x: \tau_{l}\right\}}_{l}^{l} \tag{48}
\end{equation*}
$$
for some $\bar{k}_{l}^{l}$. By Corollary B.61, (48) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}}}^{\bigvee_{l_{l}} \neg\left\{x: \tau_{l}\right\}}{ }^{l} \tag{49}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\neg\left\{x: \tau_{1} \vee \tau_{2}\right\}$, which indeed satisfies $\pi=\pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $\bigwedge_{l} U_{k_{l}}^{C_{k_{l}}} \preceq V_{1}^{D_{1}}$.
Case S-RcoMrgNegInv . Then $\tau=\neg\left\{x: \tau_{1} \wedge \tau_{2}\right\}$ and $\pi=\neg\left\{x: \tau_{1}\right\} \vee \neg\left\{x: \tau_{2}\right\}$ for some $\tau_{1}$ and $\tau_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\neg\left\{x: \tau_{1} \wedge \tau_{2}\right\} \tag{50}
\end{equation*}
$$

By S-Trans with Lemma B.22. on S-AndOr12., (50) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left\{x: \tau_{1} \wedge \tau_{2}\right\} \tag{51}
\end{equation*}
$$

By Corollary B.86, (51) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq \neg\left\{x: \tau_{1} \wedge \tau_{2}\right\} \tag{52}
\end{equation*}
$$
for some $k$. By Corollary B.61, (52) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l} \neg\left\{x: \tau_{1} \wedge \tau_{2}\right\} \tag{53}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\neg\left\{x: \tau_{1}\right\} \vee \neg\left\{x: \tau_{2}\right\}$, which indeed satisfies $\pi \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $U_{k}^{C_{k}} \leq V_{1}^{D_{1}}$.
Case S-RcoTop. Then $\tau=\top$ and $\pi=\left\{x: \pi_{1}\right\} \vee \pi_{0}$, where $\pi_{0} \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}$. Pick $\pi_{1}^{\prime}=\perp$ and $D_{1}=\top$ and $V_{1}^{D_{1}}=\left\{x: \pi_{1}\right\} \vee \pi_{0}$, which indeed satisfies $\pi=\left\{x: \pi_{1}\right\} \vee \pi_{0} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and ${\overline{U_{i}^{C_{i}}} \leq V_{1}^{D_{1}}}^{i}$.
Case S-RcdTopNegInv. Then $\tau=\neg\left\{x: \tau_{1}\right\} \wedge \neg \tau_{0}$ and $\pi=\neg \top$, where $\tau_{0} \in\left\{\left\{y^{\neq x}: \tau_{2}\right\}, \tau_{2} \rightarrow \tau_{3}\right\}$.
From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\neg\left\{x: \tau_{1}\right\} \wedge \neg \tau_{0} \tag{54}
\end{equation*}
$$

By S-Trans with Lemma B.22. on S-AndOr12., (54) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left\{x: \tau_{1}\right\} \wedge \neg \tau_{0} \tag{55}
\end{equation*}
$$

By Lemma B.54, (55) implies:
$$
\begin{gather*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left\{x: \tau_{1}\right\}  \tag{56}\\
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg \tau_{0} \tag{57}
\end{gather*}
$$

By Corollary B.86, (56) and (57) imply:
$$
\begin{gather*}
U_{k_{1}}^{C_{k_{1}}} \subseteq \neg\left\{x: \tau_{1}\right\}  \tag{58}\\
U_{k_{2}}^{C_{k_{2}}} \subseteq \neg \tau_{0} \tag{59}
\end{gather*}
$$
for some $k_{1}$ and $k_{2}$. By Corollary B.61, (58) and (59) imply:
$$
\begin{gather*}
U_{k_{1}}^{C_{k_{1}}}=\bigvee_{i_{1}} \neg\left\{x: \tau_{1}\right\}  \tag{60}\\
U_{k_{2}}^{C_{k_{2}}}=\bigvee_{i_{2}} \neg \tau_{0} \tag{61}
\end{gather*}
$$

Case $\tau_{0}=\left\{y: \tau_{2}\right\}$. Then $C_{k_{1}}=\chi$ and $C_{k_{2}}=y$, which is impossible since $\left|\left\{\chi \mid \chi \in\left\{{\overline{C_{i}}}^{i}\right\}\right\}\right| \leqslant$ 1.

Case $\tau_{0}=\tau_{2} \rightarrow \tau_{3}$. Then $C_{k_{1}}=\chi$ and $C_{k_{2}}=\longrightarrow$, which is impossible since $\left|\left\{\chi \mid \chi \in\left\{{\overline{C_{i}}}^{i}\right\}\right\}\right|=$ 0 or $\longrightarrow \notin\left\{{\overline{C_{i}}}^{i}\right\}$.
Case S-FunDepth. Then $\tau=\tau_{1} \rightarrow \tau_{2}$ and $\pi=\tau_{0} \rightarrow \tau_{3}$ for some $\bar{\tau}_{l}^{l \in 0 . .3}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\tau_{1} \rightarrow \tau_{2} \tag{62}
\end{equation*}
$$

By S-Trans with Lemma B.22. on S-AndOr12., (62) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \tau_{1} \rightarrow \tau_{2} \tag{63}
\end{equation*}
$$

By Lemma B.82, (63) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq \tau_{1} \rightarrow \tau_{2} \tag{64}
\end{equation*}
$$
for some $k$. By Lemma B.60, (64) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l} \tau_{1} \rightarrow \tau_{2} \tag{65}
\end{equation*}
$$

The premises of the rule are:
$$
\begin{align*}
& \triangleright \Sigma \vdash \tau_{0} \leqslant \tau_{1}  \tag{66}\\
& \triangleright \Sigma \vdash \tau_{2} \leqslant \tau_{3} \tag{67}
\end{align*}
$$

By the definition of $\leq$, (66) and (67) imply:
$$
\begin{gather*}
\quad \triangleright \Sigma \vdash \tau_{1} \rightarrow \tau_{2} \leq \tau_{0} \rightarrow \tau_{3} \\
\text { i.e., } \quad \triangleright \Sigma \vdash U_{k}^{C_{k}} \leq \tau_{0} \rightarrow \tau_{3} \tag{68}
\end{gather*}
$$

So we can pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\tau_{0} \rightarrow \tau_{3}$, which indeed yields $\pi=\tau_{0} \rightarrow \tau_{3} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$.
Case S-FunMrg. Then $\tau=\tau_{11} \rightarrow \tau_{12} \wedge \tau_{21} \rightarrow \tau_{22}$ and $\pi=\left(\tau_{11} \vee \tau_{21}\right) \rightarrow\left(\tau_{12} \wedge \tau_{22}\right)$ for some $\tau_{11}, \tau_{12}, \tau_{21}$, and $\tau_{22}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\tau_{11} \rightarrow \tau_{12} \wedge \tau_{21} \rightarrow \tau_{22} \tag{69}
\end{equation*}
$$

By S-Trans with Lemma B.22. on S-AndOr12., (69) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \tau_{11} \rightarrow \tau_{12} \wedge \tau_{21} \rightarrow \tau_{22} \tag{70}
\end{equation*}
$$

Let $l$ range from 1 to 2 . By Lemma B.54, (70) implies:
$$
\begin{equation*}
{\overline{\bigwedge_{i} U_{i}^{C_{i}} \subseteq \tau_{l 1} \rightarrow \tau_{l 2}}}^{l} \tag{71}
\end{equation*}
$$

By Lemma B.82, (71) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}} \subseteq \tau_{l 1} \rightarrow \tau_{l 2}}}^{l} \tag{72}
\end{equation*}
$$
for some $\bar{k}_{l}^{l}$. By Lemma B.60, (72) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}}=\bigvee_{l_{l}} \tau_{l 1} \rightarrow \tau_{l 2}}_{l}^{l} \tag{73}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\left(\tau_{11} \vee \tau_{21}\right) \rightarrow\left(\tau_{12} \wedge \tau_{22}\right)$, which indeed satisfies $\pi=\left(\tau_{11} \vee \tau_{21}\right) \rightarrow \left(\tau_{12} \wedge \tau_{22}\right) \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $\bigwedge_{l} U_{k_{l}}^{C_{k_{l}}} \leq V_{1}^{D_{1}}$.
Case S-FunMrg . Then $\tau=\left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right)$ and $\pi=\tau_{1} \rightarrow \tau_{2} \vee \tau_{3} \rightarrow \tau_{4}$ for some $\bar{\tau}_{l}^{l \in 1 . .4}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right) \tag{74}
\end{equation*}
$$

By S-Trans with Lemma B.22> on S-AndOr12., (74) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq\left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right) \tag{75}
\end{equation*}
$$

By Lemma B.82, (75) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq\left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right) \tag{76}
\end{equation*}
$$
for some $k$. By Lemma B.60, (76) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l}\left(\tau_{1} \wedge \tau_{3}\right) \rightarrow\left(\tau_{2} \vee \tau_{4}\right) \tag{77}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\tau_{1} \rightarrow \tau_{2} \vee \tau_{3} \rightarrow \tau_{4}$, which indeed satisfies $\pi=\tau_{1} \rightarrow \tau_{2} \vee \tau_{3} \rightarrow \tau_{4} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $U_{k}^{C_{k}} \leq V_{1}^{D_{1}}$.
Case S-FunMrgNegInv. Then $\tau=\neg\left(\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)\right)$ and $\pi=\neg\left(\tau_{1} \rightarrow \tau_{3}\right) \vee \neg\left(\tau_{2} \rightarrow \tau_{4}\right)$ for some $\bar{\tau}_{l}^{l \in 1 . .4}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\neg\left(\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)\right) \tag{78}
\end{equation*}
$$

By S-Trans with Lemma B.22d on S-AndOr12•, (78) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left(\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)\right) \tag{79}
\end{equation*}
$$

By Corollary B.86, (79) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq \neg\left(\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)\right) \tag{80}
\end{equation*}
$$
for some $k$. By Corollary B.61, (80) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l} \neg\left(\left(\tau_{1} \vee \tau_{3}\right) \rightarrow\left(\tau_{2} \wedge \tau_{4}\right)\right) \tag{81}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\neg\left(\tau_{1} \rightarrow \tau_{3}\right) \vee \neg\left(\tau_{2} \rightarrow \tau_{4}\right)$, which indeed satisfies $\pi \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $U_{k}^{C_{k}} \leq V_{1}^{D_{1}}$.
Case S-FunMrgNegInv D. Then $\tau=\neg\left(\tau_{11} \rightarrow \tau_{12}\right) \wedge \neg\left(\tau_{21} \rightarrow \tau_{22}\right)$ and $\pi=\neg\left(\left(\tau_{11} \wedge \tau_{21}\right) \rightarrow\right. \left.\left(\tau_{12} \vee \tau_{22}\right)\right)$ for some $\tau_{11}, \tau_{12}, \tau_{21}$, and $\tau_{22}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\neg\left(\tau_{11} \rightarrow \tau_{12}\right) \wedge \neg\left(\tau_{21} \rightarrow \tau_{22}\right) \tag{82}
\end{equation*}
$$

By S-Trans with Lemma B.22d on S-AndOr12•, (82) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left(\tau_{11} \rightarrow \tau_{12}\right) \wedge \neg\left(\tau_{21} \rightarrow \tau_{22}\right) \tag{83}
\end{equation*}
$$

Let $l$ range from 1 to 2 . By Lemma B.54, (83) implies:
$$
\begin{equation*}
{\overline{\bigwedge_{i} U_{i}^{C_{i}} \subseteq \neg\left(\tau_{l 1} \rightarrow \tau_{l 2}\right)}}^{l} \tag{84}
\end{equation*}
$$

By Corollary B.86, (84) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}} \subseteq \neg\left(\tau_{l 1} \rightarrow \tau_{l 2}\right)}_{l}^{l} \tag{85}
\end{equation*}
$$
for some $\bar{k}_{l}^{l}$. By Corollary B.61, (85) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}}=\bigvee_{l_{l}} \neg\left(\tau_{l 1} \rightarrow \tau_{l 2}\right)}_{l}^{l} \tag{86}
\end{equation*}
$$

Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\neg\left(\left(\tau_{11} \wedge \tau_{21}\right) \rightarrow\left(\tau_{12} \vee \tau_{22}\right)\right)$. Then $\pi=\pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and $\bigwedge_{l} U_{k_{l}}^{C_{k_{l}}} \leq V_{1}^{D_{1}}$. Case S-ClsSub. Then $\tau=\# C_{1}$ and $\pi=\# C_{2}$ for some $\# C_{1}$ and $\# C_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\# C_{1} \tag{87}
\end{equation*}
$$

By S-Trans with Lemma B.22> on S-AndOr12., (87) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \# C_{1} \tag{88}
\end{equation*}
$$

By Lemma B.82, (88) implies:
$$
\begin{equation*}
U_{k}^{C_{k}} \subseteq \# C_{1} \tag{89}
\end{equation*}
$$

By Lemma B.60, (89) implies:
$$
\begin{equation*}
U_{k}^{C_{k}}=\bigvee_{l} \# C_{1} \tag{90}
\end{equation*}
$$

The premise of the rule is:
$$
\begin{equation*}
C_{2} \in \mathcal{S}\left(\# C_{1}\right) \tag{91}
\end{equation*}
$$

By the definition of $\leq$, (91) implies:
$$
\begin{align*}
& \quad \# C_{1} \leq \# C_{2} \\
& \text { i.e., } \quad U_{k}^{C_{k}} \leq \# C_{2} \tag{92}
\end{align*}
$$

So we can pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\# C_{2}$, which indeed yields $\pi=\# C_{2} \cong \pi_{1}^{\prime} \vee V_{1}^{D_{1}}$.
Case S-ClsBot. Then $\tau=\# C_{1} \wedge \# C_{2}$ and $\pi=\perp$ for some $\# C_{1}$ and $\# C_{2}$. From the assumption, we have:
$$
\begin{equation*}
\bigwedge_{i}\left(\tau_{i}^{\prime} \vee U_{i}^{C_{i}}\right) \subseteq \tau=\# C_{1} \wedge \# C_{2} \tag{93}
\end{equation*}
$$

By S-Trans with Lemma B.22d on S-AndOr12•, (93) implies:
$$
\begin{equation*}
\bigwedge_{i} U_{i}^{C_{i}} \subseteq \# C_{1} \wedge \# C_{2} \tag{94}
\end{equation*}
$$

Let $l$ range from 1 to 2. By Lemma B.54, (94) implies:
$$
\begin{equation*}
{\overline{\bigwedge_{i} U_{i}^{C_{i}} \subseteq \# C_{l}}}^{l} \tag{95}
\end{equation*}
$$

By Lemma B.82, (95) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}} \subseteq \# C_{l}}}_{l}^{l} \tag{96}
\end{equation*}
$$
for some $\bar{k}_{l}^{l}$. By Lemma B.60, (96) implies:
$$
\begin{equation*}
{\overline{U_{k_{l}}^{C_{k_{l}}}}=\bigvee_{k_{l}} \# C_{l}}_{l}^{l} \tag{97}
\end{equation*}
$$

Then (97) implies:
$$
\begin{equation*}
{\overline{C_{k_{l}}=\# C_{l}}}^{l} \tag{98}
\end{equation*}
$$

The premises of the rule are:
$$
\begin{align*}
& C_{1} \notin \mathcal{S}\left(\# C_{2}\right)  \tag{99}\\
& C_{2} \notin \mathcal{S}\left(\# C_{1}\right) \tag{100}
\end{align*}
$$
which is impossible by the condition on $\bar{C}_{i}^{i}$.
Case S-ClsBotNegInv. Then $\tau=\top$ and $\pi=\neg \# C_{1} \vee \neg \# C_{1}$ for some $C_{1}$ and $C_{2}$. The premises are $C_{1} \notin \mathcal{S}\left(\# C_{2}\right)$ and $C_{2} \notin \mathcal{S}\left(\# C_{1}\right)$. Pick $\pi_{1}^{\prime}=\perp$ and $V_{1}^{D_{1}}=\neg \# C_{1} \vee \neg \# C_{1}$, which indeed satisfies $\pi=\pi_{1}^{\prime} \vee V_{1}^{D_{1}}$ and ${\overline{U_{i}^{C_{i}} \leq V_{1}^{D_{1}}}}^{i}$.

\section*{B. 12 Progress Proofs}

Lemma B. 90 (Progress - general). If $\epsilon, \epsilon \vdash P: \tau$ and body $(P)$ is not a value then $P \leadsto P^{\prime}$ for some $P^{\prime}$.

Proof. By induction on program typing derivations.
Case T-Body. By progress for terms (Lemma B.91).
Case T-Def. By E-Def.

Lemma B. 91 (Term progress). If $\epsilon, \epsilon \vdash t: \tau$ and $t$ is not a value then $t \leadsto t^{\prime}$ for some $t^{\prime}$.
Proof. By induction on typing derivations.
Case T-Subs. Immediate from the induction hypothesis.
Case T-Obj. $\quad t=C\left\{\overline{x=t^{\prime}}\right\} \quad$ If all $t^{\prime}$ are values, then $t$ is a value; otherwise $t$ reduces by E-CTX and IH .
Case T-Proj. $\quad t=t^{\prime} . x$
If $t^{\prime}$ is not a value, by IH we have $t^{\prime} \leadsto t^{\prime \prime}$, and thus $t \leadsto t^{\prime \prime}$. $x$ by E-CTX. Otherwise, by canonical form for record types (Lemma B.92), we have $t^{\prime}=C R$ and $\left\{x=v^{\prime}\right\} \in R$, and therefore $t \leadsto v^{\prime}$ by E-Proj.
Cases T-Var1, T-Var2. $\quad t=x$
Impossible since there is no rule that would type $x$ in an empty typing context.
Case T-Abs. $\quad t=\lambda x . t^{\prime} \quad$ Immediate since $t$ is a value.
Case T-App. $\quad t=t_{0} t_{1}$
We can apply the induction hypothesis on $t_{0}$ and $t_{1}$, which are given types in the premises of this typing rule. If either $t_{0}$ or $t_{1}$ is not a value, then $t$ can progress by E -Ctx, so we only have to consider the case where $t_{0}=v_{0}$ and $t_{1}=v_{1}$. By canonical form for function types (Lemma B.93), we have $v_{0}=\lambda x . t^{\prime}$. Then $t \leadsto\left[x \mapsto v_{2}\right] t^{\prime}$ by E-App.
Case T-Asc. $\quad t=t_{1}: \tau \quad$ Immediate since $t_{1}: \tau \leadsto t_{1}$ by E-Asc.
Case T-Case 1. $t=$ case $x=t_{1}$ of $\epsilon$
By IH, if $t_{1}$ is not a value, then $t$ progresses by E-Ctx. Moreover, by canonical form for bottom types (Lemma B.95), $t_{1}$ cannot be a value.
Case T-Case2. $\quad t=$ case $x=t_{1}$ of $\rightarrow t_{2}$
By IH, if $t_{1}$ is not a value, then $t$ progresses by E-CTX. On the other hand, if $t_{1}=v_{1}$, then $t \leadsto t_{2}$ by E-CASEWlD.

Case T-Case3. $\quad t=$ case $x=t_{1}$ of $C \rightarrow t_{2}, M$
By IH, if $t_{1}$ is not a value, then $t$ progresses by E-Ctx. On the other hand, if $t_{1}=v_{1}$, either $v_{1}=C_{1} R$ with $C_{2} \in \mathcal{S}\left(C_{1}\right)$, in which case E-CaseCls1 applies, or E-CaseCls2 applies since scrutinees can only be classes by Lemma B. 96 and canonical form for class types (Lemma B.94); in either case, $t$ progresses.

Lemma B. 92 (Canonical form for record types). If $\epsilon, \Gamma \vdash v:\{x: \tau\}$ then we have $v=C R$ for some $C$ and $R$, and $\left\{x=v^{\prime}\right\} \in R$.

Proof. By induction on typing derivations for the statement: if $\epsilon, \Gamma \vdash v: \tau$ and $\epsilon \vdash \tau \leqslant\left\{x: \tau^{\prime}\right\}$ then $\left\{x=v^{\prime}\right\} \in v$. The only cases to consider are those rules that can type values:
Case T-Subs. Then the premises of the rule are $v: \tau^{\prime \prime}$ and $\tau^{\prime \prime} \leqslant \tau$ for some $\tau^{\prime \prime}$. By S-Trans on $\tau^{\prime \prime} \leqslant \tau$ and $\tau \leqslant\left\{x: \tau^{\prime}\right\}$, we have $\tau^{\prime \prime} \leqslant\left\{x: \tau^{\prime}\right\}$. This allows us to apply the IH on the premise $v: \tau^{\prime \prime}$, by which we have $\left\{x=v^{\prime}\right\} \in v$.
Case T-Abs. Then $\tau=\tau_{1} \rightarrow \tau_{2}$. By consistency of subtyping (Theorem B.88), $\tau_{1} \rightarrow \tau_{2} \leqslant\left\{x: \tau^{\prime}\right\}$ cannot be true, therefore this case is impossible.
Case T-Obj. Then $\tau=\# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}$ and $v=C\left\{{\overline{x_{i}=v_{i}}}^{i}\right\}$. Then by consistency of subtyping (Theorem B.88) we know that there is an $i$ such that $x_{i}=x$. Given the conclusion of T-Obj and the definition of field projection (Section 4.2), this implies that there is a $v^{\prime}=v_{i}$ such that $\left\{x=v^{\prime}\right\} \in v$.

Lemma B. 93 (Canonical form for function types). If $\epsilon, \Gamma \vdash v: \tau_{1} \rightarrow \tau_{2}$ then we have $v=\lambda x . t$ for some $x$ and $t$.

Proof. By induction on typing derivations for the statement: if $\epsilon, \Gamma \vdash v: \tau$, and $\epsilon \vdash \tau \leqslant \tau_{1} \rightarrow \tau_{2}$ then $v=\lambda x$. $t$ for some $x$ and $t$. The only cases to consider are those rules that can type values:
Case T-Subs. Then the premises of the rule are $v: \tau^{\prime}$ and $\tau^{\prime} \leqslant \tau$ for some $\tau^{\prime}$. By S-Trans on $\tau^{\prime} \leqslant \tau$ and $\tau \leqslant \tau_{1} \rightarrow \tau_{2}$, we have $\tau^{\prime} \leqslant \tau_{1} \rightarrow \tau_{2}$. Then the result follows from IH on $v: \tau^{\prime}$.
Case T-Abs. Immediate.
Case T-Obj. Then $\tau=\left\{\overline{x_{i}: \tau_{i}}{ }^{i}\right\}$ for some $\bar{x}_{i}^{i}$ and ${\overline{v_{i}}}^{i}$. By consistency of subtyping (Theorem B.88), $\tau \leqslant \tau_{1} \rightarrow \tau_{2}$ cannot be true, therefore this case is impossible.

Lemma B. 94 (Canonical form for class types). If $\epsilon, \Gamma \vdash v: \# C$ then we have $v=C R$ for some $R$.

Proof. By induction on typing derivations for the statement: if $\epsilon, \Gamma \vdash v: \tau$, and $\epsilon \vdash \tau \leqslant \# C$ then $v=C R$ for some $R$. The only cases to consider are those rules that can type values:
Case T-Subs. Then the premises of the rule are $v: \tau^{\prime}$ and $\tau^{\prime} \leqslant \tau$ for some $\tau^{\prime}$. By S-Trans on $\tau^{\prime} \leqslant \tau$ and $\tau \leqslant \# C$, we have $\tau^{\prime} \leqslant \# C$. Then the result follows from IH on $v: \tau^{\prime}$.
Case T-Abs. Then $\tau=\tau_{1} \rightarrow \tau_{2}$ for some $\tau_{1}$ and $\tau_{2}$. By consistency of subtyping (Theorem B.88), $\tau \leqslant \# C$ cannot be true, therefore this case is impossible.
Case T-Obj. Immediate.

Lemma B. 95 (Canonical form for bottom type). For all $v, \epsilon, \Gamma \vdash v: \perp$ cannot be derived.

Proof. By case analysis on the last typing rule used in the typing derivation, assuming without loss of generality that this typing derivation is in subsumption-normalized form (Lemma B.6). The only cases to consider are those rules that can type values:
Cases T-Abs, T-Obj. Immediate.
Case T-Subs. The premises are $\epsilon, \Gamma \vdash v: \tau$ and $\epsilon \vdash \tau \leqslant \tau^{\prime}$ and the goal is to show that we cannot have $\tau^{\prime}=\perp$, i.e., that $\tau \leqslant \perp$ cannot be derived. The typing derivation being subsumptionnormalized, the first premise is not an application of T-Subs, so it must be an application of either T-Abs or T-Obj, meaning that $\tau \in\left\{\tau_{1} \rightarrow \tau_{2}, \# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}\right\}$. We conclude that $\tau \leqslant \perp$ cannot be derived by consistency of subtyping (Theorem B.88).

Lemma B. 96 (Scrutinee types). If $\epsilon, \Gamma \vdash$ case $x=t$ of $M: \tau$ then we have $\epsilon, \Gamma \vdash t: \# C$ for some $C$.

Proof. By induction of typing derivations.
Case T-Subs. Then the former premise of the rule is $\epsilon, \Gamma \vdash$ case $x=v$ of $M: \tau^{\prime}$ for some $\tau^{\prime}$. The result follows from IH.
Case T-Cass1. Then the premise of the rule is $\epsilon, \Gamma \vdash v: \perp$, which is impossible by canonical form for bottom type (Lemma B.95).
Case T-Case2. Then the former premise of the rule is $\epsilon, \Gamma \vdash v: \tau_{1} \wedge \# C$ for some $\tau_{1}$ and $C$. Then by T-Subs with $\tau_{1} \wedge \# C \leqslant \# C$ (S-AndOr12d), we have $\epsilon, \Gamma \vdash v: \# C$.
Case T-Case3. Then the first premise of the rule is $\epsilon, \Gamma \vdash t: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}$ for some $\tau_{1}$ and $\tau_{2}$ We have either $\epsilon, \Gamma \vdash t: \# C^{\prime}$ or $\epsilon, \Gamma \vdash t: \neg \# C^{\prime}$ for some $C^{\prime}$. For the former, the result is immediate. For the latter, we have $\epsilon, \Gamma \vdash t:\left(\# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}\right) \wedge \neg \# C$, which implies $\epsilon, \Gamma \vdash t: \tau_{2}$ by T-Subs since $\left(\# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}\right) \wedge \neg \# C \equiv \neg \# C \wedge \tau_{2} \leqslant \tau_{2}$. By IH on the last premise $\epsilon, \Gamma \cdot\left(x: \tau_{2}\right) \vdash$ case $x=x$ of $M: \tau$, we have $\epsilon, \Gamma \cdot\left(x: \tau_{2}\right) \vdash x: \# C^{\prime \prime}$ for some $C^{\prime \prime}$, i.e., $\tau_{2} \leqslant \# C^{\prime \prime}$. Then we have $\epsilon, \Gamma \vdash t$ : \# $C^{\prime \prime}$ by T-Subs.

\section*{B. 13 Preservation Proofs}

Lemma B. 97 (Preservation - general). If $\epsilon, \Gamma \vdash^{\star} P: \tau$ and $P \leadsto P^{\prime}$, then we have $\epsilon, \Gamma \vdash^{\star} P^{\prime}: \tau$.

Proof. By induction on program typing derivations.
Case T-Body. By preservation for terms (Lemma B.101).
Case T-Def. $\quad P=\operatorname{def} x=t ; P^{\prime}$
The only applicable reduction rule is E-Def. The premises of the rule are $\Xi, \Gamma \vdash t: \tau$ and $\epsilon, \Gamma \cdot(x: \forall \Xi . \tau) \vdash^{\star} P^{\prime}: \tau_{P}$ for some $\Xi$ and $\tau$. By substitution (Lemma B.98), we have $\epsilon, \Gamma \vdash[x \mapsto t] P^{\prime}: \tau_{P}$.

Lemma B. 98 (Substitution). For all $\mathcal{D}$ wf, $\Gamma$ and $\Xi$ such that $T V(\Gamma) \cap T V(\forall \Xi . \tau)=\varnothing$ :
(1) If $\epsilon, \Gamma \cdot(x: \forall \Xi . \tau) \vdash^{\star} P: \tau_{P}$ and $\Xi, \Gamma \vdash t: \tau$, then $\epsilon, \Gamma \vdash^{\star}[x \mapsto t] P: \tau_{P}$.
(2) If $\Xi_{0}, \Gamma \cdot(x: \forall \Xi, \tau) \vdash t_{P}: \tau_{P}$ and $\Xi_{0} \cdot \Xi, \Gamma \vdash t: \tau$, then $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{P}: \tau_{P}$.

Proof. By induction on program typing derivations of $\epsilon, \Gamma \cdot(x: \forall \Xi, \tau) \vdash^{\star} P: \tau_{P}$ and typing derivations of $\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t_{P}: \tau_{P}$. Note that the $T V(\Gamma) \cap T V(\forall \Xi . \tau)=\varnothing$ condition can always be obtained by renaming variables quantified in definitions, when necessary. The only difficult cases are for T-Body and T-Var2:

Case T-Body. $\quad P=t_{P}$
The premises of the rule are $\epsilon$ cons. and $\epsilon, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t_{P}: \tau_{P}$. By assumption, we have $\Xi, \Gamma \vdash t: \tau$ By IH, we have $\epsilon, \Gamma \vdash[x \mapsto t] t_{P}: \tau_{P}$. The result $\epsilon, \Gamma \vdash^{\star}[x \mapsto t] t_{P}: \tau_{P}$ then follows by T-Body, as $P=t_{P}$.
Case T-Def. $\quad P=\boldsymbol{\operatorname { d e f }} x^{\prime}=t^{\prime} ; P^{\prime}$
If $x^{\prime}=x$, then $[x \mapsto t] P=P$ and the result is immediate.
Otherwise, $[x \mapsto t] P=\boldsymbol{\operatorname { d e f }} x^{\prime}=[x \mapsto t] t^{\prime} ;[x \mapsto t] P$. We can apply the IH on the second premise of T-Def, $\Xi^{\prime}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t^{\prime}: \tau^{\prime}$, to get $\Xi^{\prime}, \Gamma \vdash[x \mapsto t] t^{\prime}: \tau^{\prime}$. Then, the third premise of T-Def, $\epsilon, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right) \vdash^{\star} P: \tau_{P}$, can be commuted (Lemma B.100) to $\epsilon, \Gamma \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right) \cdot(x: \forall \Xi \cdot \tau) \vdash^{\star} P: \tau_{P}$, on which we can apply the IH to get $\epsilon, \Gamma \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right) \vdash^{\star}[x \mapsto t] P: \tau_{P}$. We then conclude by T-Def, for which we have just derived the last two premises (the first premise is unchanged).
Case T-Subs. The premises of the rule are $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{P}: \tau_{1}$ and $\Xi_{0} \vdash \tau_{1} \leqslant \tau_{P}$. By IH on the first premise, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{P}: \tau_{1}$. Then $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{P}: \tau_{P}$ by T-Subs with the second premise.
Case T-Obj. $\quad t_{P}=C\left\{\overline{x^{\prime}=t^{\prime}}\right\} \quad \tau_{P}=\# C \wedge\left\{\overline{x^{\prime}: \tau^{\prime}}\right\}$
The premises of the rule are $\overline{\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t^{\prime}: \tau^{\prime}}$. By IH, we have $\overline{\Xi_{0}, \Gamma \vdash[x \mapsto t] t^{\prime}: \tau^{\prime}}$. Then $\Xi_{0}, \Gamma \vdash C\left\{\overline{x^{\prime}=[x \mapsto t] t^{\prime}}\right\}: \# C \wedge\left\{\overline{x^{\prime}: \tau^{\prime}}\right\}$ by T-ObJ, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]\left(C\left\{\overline{x^{\prime}=t^{\prime}}\right\}\right)$ : $\# C \wedge\left\{\overline{x^{\prime}: \tau^{\prime}}\right\}$ by the definition of substitution.
Case T-Proj. $\quad t_{P}=t^{\prime} . x^{\prime}$
The premise of the rule is $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t^{\prime}:\left\{x^{\prime}: \tau_{P}\right\}$. By IH, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t^{\prime}:\left\{x^{\prime}: \tau_{P}\right\}$. Then $\Xi_{0}, \Gamma \vdash\left([x \mapsto t] t^{\prime}\right) . x^{\prime}: \tau_{P}$ by T-ProJ, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t] t^{\prime} . x^{\prime}: \tau_{P}$ by the definition of substitution.
Case T-Var1. $\quad t_{P}=x^{\prime}(\Gamma \cdot(x: \forall \Xi \cdot \tau))\left(x^{\prime}\right)=\tau_{P}$
Since $x^{\prime}$ is mapped to a simple type in the context $\Gamma \cdot(x: \forall \Xi \cdot \tau), x \neq x^{\prime}$, then $\Gamma\left(x^{\prime}\right)=\tau_{P}$.
Then $\Xi_{0}, \Gamma \vdash x^{\prime}: \tau_{P}$, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t] x^{\prime}: \tau_{P}$ by the definition of substitution.
Case T-Var2. $\quad t_{P}=x^{\prime} \quad \rho\left(\tau_{P}^{\prime}\right) \leqslant \tau_{P} \quad(\Gamma \cdot(x: \forall \Xi \cdot \tau))\left(x^{\prime}\right)=\forall \Xi^{\prime} . \tau_{P}^{\prime} \quad \Xi_{0} \models \rho\left(\Xi^{\prime}\right)$
There are two cases to consider:
Case $x^{\prime} \neq x$. Then $[x \mapsto t] t_{P}=t_{P}$ and the result is immediate.
Case $x^{\prime}=x$. Then $[x \mapsto t] t_{P}=t$ and moreover $(\Gamma \cdot(x: \forall \Xi \cdot \tau))(x)=\forall \Xi^{\prime} . \tau_{P}^{\prime}$, thus $\forall \Xi . \tau= \forall \Xi^{\prime} . \tau_{p}^{\prime}$, and thus $\Xi=\Xi^{\prime}$ and $\tau=\tau_{p}^{\prime}$.
By assumption, $\Xi, \Gamma \vdash t: \tau$ so $\Xi^{\prime}, \Gamma \vdash t: \tau_{p}^{\prime}$. By preservation of typing under substitution (Lemma B.35), $\rho\left(\Xi^{\prime}\right), \rho(\Gamma) \vdash t: \rho\left(\tau_{P}^{\prime}\right)$, i.e., $\rho\left(\Xi^{\prime}\right), \Gamma \vdash t: \tau_{P}$ by T-Subs and since $T V(\Gamma) \cap \operatorname{dom}(\rho)=\varnothing$ by assumption.
Moreover, since we have $\Xi_{0} \vDash \rho\left(\Xi^{\prime}\right)$, this implies that $\Xi_{0}, \Gamma \vdash t: \tau_{P}$ (Lemma B.34), which is what we wanted to prove (remember $t=[x \mapsto t] t_{P}$ ).
Case T-Abs. $\quad t_{P}=\lambda x^{\prime} . t^{\prime} \quad \tau_{P}=\tau_{1} \rightarrow \tau_{2}$
There are two cases to consider:
Case $x^{\prime}=x$. The premise of the rule is $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$. Since the binding $(x: \forall \Xi . \tau)$ is shadowed, we can remove it from the typing context (Lemma B.99), i.e., $\Xi_{0}, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$. Then $\Xi_{0}, \Gamma \vdash \lambda x . t^{\prime}: \tau_{1} \rightarrow \tau_{2}$ by T-Abs, which is the desired result since $[x \mapsto t] t_{P}=t_{P}$ and $x^{\prime}=x$.
Case $x^{\prime} \neq x$. The premise of the rule is $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$, which can be commuted (Lemma B.100) to $\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \cdot(x: \forall \Xi . \tau) \vdash t^{\prime}: \tau_{2}$. By IH, we have $\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \vdash[x \mapsto t] t^{\prime}: \tau_{2}$. Then $\Xi_{0}, \Gamma \vdash \lambda x^{\prime}$. $[x \mapsto t] t^{\prime}: \tau_{1} \rightarrow \tau_{2}$, i.e., $\Xi_{0}, \Gamma \vdash [x \mapsto t] \lambda x^{\prime} . t^{\prime}: \tau_{1} \rightarrow \tau_{2}$ by the definition of substitution.

Case T-App. $\quad t_{P}=t_{0} t_{1}$
The premises of the rule are $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{0}: \tau_{1} \rightarrow \tau_{P}$ and $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{1}: \tau_{1}$ for some $\tau_{1}$. By IH, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{0}: \tau_{1} \rightarrow \tau_{P}$ and $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \tau_{1}$. Then $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{0}[x \mapsto t] t_{1}: \tau_{P}$ by T-App, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]\left(t_{0} t_{1}\right): \tau_{P}$ by the definition of substitution.
Case T-Asc. $\quad t_{P}=t^{\prime}: \tau_{P}$
The premise of the rule is $\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t^{\prime}: \tau_{P}$. By IH, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t^{\prime}: \tau_{P}$. Then $\Xi_{0}, \Gamma \vdash\left([x \mapsto t] t^{\prime}: \tau_{P}\right): \tau_{P}$ by T-Asc, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]\left(t^{\prime}: \tau_{P}\right): \tau_{P}$ by the definition of substitution.
Case T-Case 1. $\quad t_{P}=$ case $x^{\prime}=t_{1}$ of $\epsilon \quad \tau_{P}=\perp$
The premise of the rule is $\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t_{1}: \perp$. By IH, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \perp$. Then $\Xi_{0}, \Gamma \vdash$ case $x^{\prime}=[x \mapsto t] t_{1}$ of $\epsilon: \perp$ by T-Case1, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]$ case $x^{\prime}=t_{1}$ of $\epsilon: \perp$ by the definition of substitution.
Case T-Case2. $\quad t_{P}=$ case $x^{\prime}=t_{1}$ of ${ }_{-} \rightarrow t_{2}$
There are two cases to consider:
Case $x^{\prime}=x$. The premises of the rule are $\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \vdash t_{1}: \tau_{1}$ and $\Xi_{0}, \Gamma \cdot(x: \forall \Xi . \tau) \cdot\left(x: \tau_{1}\right) \vdash t_{2}: \tau_{P}$. Since the binding ( $x: \forall \Xi . \tau$ ) in the second premise is shadowed, we can remove it from the typing context (Lemma B.99), i.e., $\Xi_{0}, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t_{2}: \tau_{p}$. By IH on the first premise, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \tau_{1}$. Then $\Xi_{0}, \Gamma \vdash$ case $x=[x \mapsto t] t_{1}$ of $\rightarrow t_{2}: \tau_{P}$, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]$ case $x=t_{1}$ of $\xrightarrow{ } \rightarrow t_{2}: \tau_{P}$ by the definition of substitution.
Case $x^{\prime} \neq x$. The premises of the rule are $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{1}: \tau_{1}$ and $\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t_{2}: \tau_{P}$. The latter can be commuted (Lemma B.100) to $\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \cdot(x: \forall \Xi . \tau) \vdash t_{2}$ : $\tau_{P}$. By IH, we have $\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \tau_{1}$ and $\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \vdash[x \mapsto t] t_{2}: \tau_{P}$. Then $\Xi_{0}, \Gamma \vdash$ case $x^{\prime}=[x \mapsto t] t_{1}$ of $\_[x \mapsto t] t_{2}$ by T-Case2, i.e., $\Xi_{0}, \Gamma \vdash[x \mapsto t]$ case $x^{\prime}=t_{1}$ of $\rightarrow t_{2}$ by the definition of substitution.
Case T-Case3. $\quad t_{P}=$ case $x^{\prime}=t_{1}$ of $C \rightarrow t_{2}, M$
There are two cases to consider:
Case $x^{\prime}=x$. The premises of the rule are:
$$
\begin{gather*}
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{1}: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}  \tag{1}\\
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x: \tau_{1}\right) \vdash t_{2}: \tau_{P}  \tag{2}\\
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x: \tau_{2}\right) \vdash \text { case } x^{\prime}=x^{\prime} \text { of } M: \tau_{P} \tag{3}
\end{gather*}
$$

By IH on (1), we have:
$$
\begin{equation*}
\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2} \tag{4}
\end{equation*}
$$

Since the binding ( $x: \forall \Xi \tau$ ) in (2) and (3) are shadowed, we can remove them from the typing contexts (Lemma B.99):
$$
\begin{gather*}
\Xi_{0}, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t_{2}: \tau_{P}  \tag{5}\\
\Xi_{0}, \Gamma \cdot\left(x: \tau_{2}\right) \vdash \text { case } x=x \text { of } M: \tau_{P} \tag{6}
\end{gather*}
$$

Then by T-Case3 on (4) and (5) and (6), we have:
$$
\begin{array}{ll} 
& \Xi_{0}, \Gamma \vdash \text { case } x=[x \mapsto t] t_{1} \text { of } C \rightarrow t_{2}, M: \tau_{P} \\
\text { i.e., } & \Xi_{0}, \Gamma \vdash[x \mapsto t] \text { case } x=t_{1} \text { of } C \rightarrow t_{2}, M: \tau_{P} \tag{7}
\end{array}
$$

Case $x^{\prime} \neq x$. The premises of the rule are:
$$
\begin{gather*}
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \vdash t_{1}: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}  \tag{8}\\
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t_{2}: \tau_{P}  \tag{9}\\
\Xi_{0}, \Gamma \cdot(x: \forall \Xi \cdot \tau) \cdot\left(x^{\prime}: \tau_{2}\right) \vdash \text { case } x^{\prime}=x^{\prime} \text { of } M: \tau_{P} \tag{10}
\end{gather*}
$$

The typing contexts in (9) and (10) can be commuted (Lemma B.100) to:
$$
\begin{gather*}
\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \cdot(x: \forall \Xi \cdot \tau) \vdash t_{2}: \tau_{P}  \tag{11}\\
\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{2}\right) \cdot(x: \forall \Xi \cdot \tau) \vdash \text { case } x^{\prime}=x^{\prime} \text { of } M: \tau_{P} \tag{12}
\end{gather*}
$$

By IH on (8) and (11) and (12) respectively, we have:
$$
\begin{gather*}
\Xi_{0}, \Gamma \vdash[x \mapsto t] t_{1}: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}  \tag{13}\\
\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{1}\right) \vdash[x \mapsto t] t_{2}: \tau_{P}  \tag{14}\\
\Xi_{0}, \Gamma \cdot\left(x^{\prime}: \tau_{2}\right) \vdash \text { case } x^{\prime}=x^{\prime} \text { of }[x \mapsto t] M: \tau_{P} \tag{15}
\end{gather*}
$$

Then by T-Case3 on (13) and (14) and (15), we have:
$$
\begin{align*}
& \Xi_{0}, \Gamma \vdash \text { case } x^{\prime}=[x \mapsto t] t_{1} \text { of } C \rightarrow[x \mapsto t] t_{2},[x \mapsto t] M: \tau_{P} \\
& \text { i.e., } \quad \Xi_{0}, \Gamma \vdash[x \mapsto t] \text { case } x^{\prime}=t_{1} \text { of } C \rightarrow t_{2}, M: \tau_{P} \tag{16}
\end{align*}
$$

Lemma B. 99 (Shadowing of typing contexts). For all $\gamma=\tau$ or $\sigma$, and $\gamma^{\prime}=\tau^{\prime}$ or $\sigma^{\prime}$ :
(1) If $\Xi, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash^{\star} P: \tau_{P}$, then $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash^{\star} P: \tau_{P}$ and $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash^{\star} P: \tau_{p}$.
(2) If $\Xi, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash t_{P}: \tau_{P}$, then $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash t_{P}: \tau_{P}$ and $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot\left(x: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime} \vdash t_{P}: \tau_{P}$.

Proof. By straightforward induction on typing derivations. The only non-trivial cases are T-Var1 and T-Var2.
Case T-Var1. By the definition of $\Gamma(\cdot)$, if $\left(\Gamma \cdot(y: \gamma) \cdot \Gamma^{\prime} \cdot\left(y: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime}\right)(x)=\tau^{\prime \prime}$ for some $\tau^{\prime \prime}$, then $\left(\Gamma \cdot \Gamma^{\prime} \cdot(y: \gamma) \cdot\left(y: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime}\right)(x)=\tau^{\prime \prime}$ and $\left(\Gamma \cdot \Gamma^{\prime} \cdot\left(y: \gamma^{\prime}\right) \cdot \Gamma^{\prime \prime}\right)(x)=\tau^{\prime \prime}$. The result then follows from T-Var1.
Case T-Var2. Similarly.

Lemma B. 100 (Commutativity of typing contexts). For all $\Gamma^{\prime}$ such that $x \notin \operatorname{dom}\left(\Gamma^{\prime}\right)$, and $\gamma=\tau$ or $\sigma$ :
(1) If $\epsilon, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \vdash^{\star} P: \tau_{P}$, then $\epsilon, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \vdash^{\star} P: \tau_{P}$.
(2) If $\Xi, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \vdash t_{P}: \tau_{P}$, then $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \vdash t_{P}: \tau_{P}$.

Proof. By induction on typing derivations.
Case T-Body. By IH, followed by T-Body.
Case T-Def. $\quad P=\boldsymbol{\operatorname { d e f }} x^{\prime}=t^{\prime} ; P^{\prime}$
The premises are $\epsilon$ cons., $\Xi^{\prime}, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \vdash t^{\prime}: \tau^{\prime}$, and $\epsilon, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \cdot\left(x^{\prime}: \forall \Xi^{\prime} . \tau^{\prime}\right) \vdash^{\star} P^{\prime}: \tau_{P}$. By IH on the second premise, we have $\Xi^{\prime}, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \vdash t^{\prime}: \tau^{\prime}$. If $x^{\prime}=x$, we can rearrange the third premise (Lemma B.99) to $\epsilon, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right) \vdash^{\star} P^{\prime}: \tau_{p}$. If $x^{\prime} \neq x$, then $x \notin \operatorname{dom}\left(\Gamma^{\prime} \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right)\right)$ and $x^{\prime} \notin \operatorname{dom}((x: \gamma))$, so we have $\epsilon, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x^{\prime}: \forall \Xi^{\prime} \cdot \tau^{\prime}\right) \vdash^{\star} P^{\prime}: \tau_{P}$ by IH. The result $\epsilon, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \vdash^{\star} \mathbf{d e f} x^{\prime}=t^{\prime} ; P^{\prime}: \tau_{P}$ then follows from T-Def.

Cases T-Subs, T-Rcd, T-Proj, T-App, T-Asc, T-Case1. By IH on the premises, followed by the respective rules.
Case T-Var1. By the definition of $\Gamma(\cdot)$, since $x \notin \operatorname{dom}\left(\Gamma^{\prime}\right)$ by assumption, if $\left(\Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime}\right)\left(x^{\prime}\right)=\tau^{\prime}$ for some $\tau^{\prime}$, then $\left(\Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma)\right)\left(x^{\prime}\right)=\tau^{\prime}$. The result then follows from T-Var1.
Case T-Var2. Similar to the case above.
Case T-Abs. $\quad t_{P}=\lambda x^{\prime} . t^{\prime} \quad \tau_{P}=\tau_{1} \rightarrow \tau_{2}$
The premise is $\Xi, \Gamma \cdot(x: \gamma) \cdot \Gamma^{\prime} \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$. If $x^{\prime}=x$, we can rearrange it (Lemma B.99) to $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$. If $x^{\prime} \neq x$, then $x \notin \operatorname{dom}\left(\Gamma^{\prime} \cdot\left(x^{\prime}: \tau_{1}\right)\right)$ and $x^{\prime} \notin \operatorname{dom}((x: \gamma))$, so we have $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \cdot\left(x^{\prime}: \tau_{1}\right) \vdash t^{\prime}: \tau_{2}$ by IH. The result $\Xi, \Gamma \cdot \Gamma^{\prime} \cdot(x: \gamma) \vdash \lambda x^{\prime} \cdot t^{\prime}: \tau_{1} \rightarrow \tau_{2}$ then follows from T-Abs.
Cases T-Case2, T-Case3. Similar to the case above.

Lemma B. 101 (Term preservation). If $\epsilon, \Gamma \vdash t: \tau$ and $t \leadsto t^{\prime}$, then $\epsilon, \Gamma \vdash t^{\prime}: \tau$.
Proof. By induction on typing derivations. In the following, we sometimes abbreviate $\epsilon, \Gamma \vdash t: \tau$ to $t: \tau$.

Case T-Subs. Immediate from the induction hypothesis.
Case T-Obj. $\quad t=C\{\overline{x=t}\} \quad \tau=\# C \wedge\{\overline{x: \tau}\}$
There is only one rule that reduces objects, E-Ctx. By straightforward application of the induction hypothesis with the respective premises of T-Obj and E-Obj and by reapplication of T-Obj on $t^{\prime}$.
Case T-Proj. $\quad t=t_{0} . x \quad t_{0}:\{x: \tau\}$
If $t \leadsto t_{0}^{\prime} . x$ by E-Ctx, we conclude by IH.
Otherwise, $t \leadsto v_{2}$ reduces by E-Proj, meaning that $t_{0}=v_{1}$ and $\left\{x=v_{2}\right\} \in v_{1}$. We conclude by inversion of object types (Lemma B.105), which gives us $v_{2}: \tau$.
Cases T-Var1,T-Var2. Immediate since $t$ cannot reduce.
Case T-Abs. $\quad t=\lambda x . t_{0} \quad$ Immediate since $t$ cannot reduce.
Case T-App. $\quad t=t_{0} t_{1} \quad t_{0}: \tau_{1} \rightarrow \tau \quad t_{1}: \tau_{1}$
There are two rules by which $t \leadsto t^{\prime}$ can hold:
Case E-Ctx. The result holds by IH and T-App.
Case E-App $\quad t_{0}=\lambda x . t_{0}^{\prime} \quad t_{1}=v_{1} \quad t \leadsto\left[x \mapsto v_{1}\right] t_{0}^{\prime}$
By inversion (Lemma B.102), $\epsilon, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t_{0}^{\prime}: \tau$. Together with substitution (Lemma B.98, applicable since $\epsilon, \Gamma \vdash v_{1}: \tau_{1}$ ), this gives us $\epsilon, \Gamma \vdash\left[x \mapsto v_{1}\right] t_{0}^{\prime}: \tau$, i.e., $\epsilon, \Gamma \vdash t^{\prime}: \tau$.
Case T-Asc. $\quad t=t_{0}: \tau \quad t^{\prime}=t_{0}$
Immediate by the premise of the rule.
Case T-Case 1. $t=$ case $x=t_{1}$ of $\epsilon$
Immediate since the only rule that can apply is E-Ctx, and it yields a term $t^{\prime}$ that can still be typed at ⟂ by T-Case1.
Case T-Case2. $\quad t=$ case $x=t_{1}$ of $\rightarrow t_{2}$
If the rule that applies is E-Ctx, by IH. Otherwise, the rule that applies is E-CaseWld, and we conclude by substitution.
Case T-Case3. $\quad t=$ case $x=t_{1}$ of $C \rightarrow t_{2}, M \quad t_{1}: \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}$
If the rule that applies is E-Ctx, by IH.
Otherwise, if E-CaseCls1 is the rule that applies, it means $t_{1}$ is an instance of a subclass of $C_{2}$, so by Lemma B. 107 we know that $\epsilon, \Gamma \vdash t_{1}: \tau_{1}$, and we can conclude by substitution (Lemma B.98).

Otherwise, E-CaseCls2 must be the rule that applies, so by Lemma B. 107 we know that $\epsilon, \Gamma \vdash t_{1}: \tau_{2}$, and we can conclude by substitution (Lemma B.98) and IH.

Lemma B. 102 (Inversion of function types). If $\epsilon, \Gamma \vdash \lambda x . t: \tau_{0}$ and $\epsilon \vdash \tau_{0} \leqslant \tau_{1} \rightarrow \tau_{2}$, then $\epsilon, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t: \tau_{2}$.

Proof. Straightforward induction on typing derivations. The only rules that can be used to type such a lambda expression are:
Case T-Subs. Then the premises of the rule are $\epsilon, \Gamma \vdash \lambda x . t: \tau_{0}^{\prime}$ and $\epsilon \vdash \tau_{0}^{\prime} \leqslant \tau_{0}$ for some $\tau_{0}^{\prime}$, on which we can apply the IH by S-Trans $\left(\tau_{0}^{\prime} \leqslant \tau_{0} \leqslant \tau_{1} \rightarrow \tau_{2}\right)$.
Case T-Abs. Then $\tau_{0}=\tau_{1}^{\prime} \rightarrow \tau_{2}^{\prime}$ for some $\tau_{1}^{\prime}$ and $\tau_{2}^{\prime}$. The premise is $\epsilon, \Gamma \cdot\left(x: \tau_{1}^{\prime}\right) \vdash t: \tau_{2}^{\prime}$. By Lemma B. 103 we have $\epsilon \vdash \tau_{1} \leqslant \tau_{1}^{\prime}$ and $\epsilon \vdash \tau_{2}^{\prime} \leqslant \tau_{2}$. Combined with strengthening (Lemma B.104) and T-Subs, this gives us the desired result.

Lemma B. 103 (Inversion of function subtyping). If $\epsilon \vdash \tau_{0} \rightarrow \tau_{1} \leqslant \tau_{2} \rightarrow \tau_{3}$, then $\epsilon \vdash \tau_{2} \leqslant \tau_{0}$ and $\epsilon \vdash \tau_{1} \leqslant \tau_{3}$.

Proof. By consistency of subtyping (Theorem B.88).
Lemma B. 104 (Strengthening). If $\epsilon, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t: \tau$ and $\epsilon \vdash \tau_{2} \leqslant \tau_{1}$, then we have $\epsilon, \Gamma \cdot\left(x: \tau_{2}\right) \vdash t: \tau$.

Proof. By straightforward induction on typing derivations, using T-Subs for the T-Var1 case.
Lemma B. 105 (Inversion of object types). If $\epsilon, \Gamma \vdash C R: \tau_{0}$ and $\{x=v\} \in C R$ and $\epsilon \vdash \tau_{0} \leqslant\{x: \tau\}$, then $\epsilon \vdash v: \tau$.

Proof. Straightforward induction on typing derivations. The only rules that can be used to type such a lambda expression are:
Case T-Subs. Then the premises of the rule are $\epsilon, \Gamma \vdash \lambda x . t: \tau_{0}^{\prime}$ and $\epsilon \vdash \tau_{0}^{\prime} \leqslant \tau_{0}$ for some $\tau_{0}^{\prime}$, on which we can apply the IH by S-Trans $\left(\tau_{0}^{\prime} \leqslant \tau_{0} \leqslant\{x: \tau\}\right)$.
Case T-Obj. Then $\tau_{0}=\# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}$ for some $C$ and $\bar{\tau}_{i}^{i}$. One of the premises is $\epsilon, \Gamma \vdash v: \tau_{k}$, where $x_{k}=x$. By Lemma B. 106 we have $\epsilon \vdash \tau_{k} \leqslant \tau$. Combined with T-Subs, this gives us the desired result.

Lemma B. 106 (Inversion of object subtyping). If $\epsilon \vdash \# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\} \leqslant\left\{x_{k}: \tau\right\}$, then $\epsilon \vdash \tau_{k} \leqslant \tau$.

Proof. Let $U_{0}^{C_{0}}=\# C$ and ${\overline{U_{i}^{C_{i}}=\left\{x_{i}: \tau_{i}\right\}}}^{i}$. Since $\# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\} \cong \bigwedge_{i^{\prime} \in\{0, \bar{i}\}}\left(\perp \vee U_{i}^{C_{i}}\right)$, by Lemma B.89, we have:
$$
\begin{gather*}
\left\{x_{k}: \tau\right\} \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right)  \tag{1}\\
{\frac{U_{k_{j}}^{C_{k_{j}}}}{} \leq V_{j}^{D_{j}}}^{j} \tag{2}
\end{gather*}
$$
for some ${\overline{\pi_{j}^{\prime}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$ and ${\overline{k_{j}}}^{j}$. By S-Trans with Lemma B.22d on S-AndOr12., (1) implies:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq\left\{x_{k}: \tau\right\} \tag{3}
\end{equation*}
$$

By Lemma B.82, (3) implies:
$$
\begin{equation*}
V_{l}^{D_{l}} \subseteq\left\{x_{k}: \tau\right\} \tag{4}
\end{equation*}
$$
for some $l$. By Lemma B.60, (4) implies:
$$
\begin{equation*}
V_{l}^{D_{l}}=\bigvee_{p}\left\{x_{k}: \tau\right\} \tag{5}
\end{equation*}
$$

Then $D_{l}=x_{k}$. By Lemma B.59, (2) for $j=l$ implies:
$$
\begin{equation*}
C_{k_{l}}=x_{k} \tag{6}
\end{equation*}
$$
i.e., $k_{l}=k$. Then (2) for $j=l$ becomes:
$$
\begin{equation*}
\left\{x_{k}: \tau_{k}\right\} \leq \bigvee_{p}\left\{x_{k}: \tau\right\} \tag{7}
\end{equation*}
$$

By case analysis on the $\leq$ rules, (7) implies:
$$
\begin{align*}
\tau_{k} & \leqslant \bigvee_{p} \tau \\
\text { i.e., } \quad \tau_{k} & \leqslant \tau \tag{8}
\end{align*}
$$

Lemma B. 107 (Inversion of discriminated class types). Assume $\epsilon, \Gamma \vdash v: \tau$ where $v$ is the scrutinee of a case expression and $\epsilon \vdash \tau \leqslant \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}$. Then we have:
- If $v=C_{0} R$ and $C_{0}$ is a subclass of $C$ (i.e., $C \in \mathcal{S}\left(C_{0}\right)$ ), then $\epsilon, \Gamma \vdash v: \tau_{1}$.
- Otherwise, $\epsilon, \Gamma \vdash v: \tau_{2}$.

Proof. By induction on typing derivations.The only rules that can be used to type a value are:
Case T-Subs. Then the premises of the rule are $\epsilon, \Gamma \vdash v: \tau^{\prime}$ and $\epsilon \vdash \tau^{\prime} \leqslant \tau$ for some $\tau^{\prime}$, on which we can apply the IH by S-Trans $\left(\tau^{\prime} \leqslant \tau \leqslant \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}\right)$.
Case T-Abs. $\quad v=\lambda x . t$
Impossible since scrutinees can only be classes (Lemma B.96).
Case T-Obj. $v=C_{0} R$
We have $R=\{\overline{x=t}\}$ and $\tau=\# C_{0} \wedge\{\overline{x: \tau}\}$ and $\overline{t: \tau}$ and $C_{0}$ is final.
So we have $\# C_{0} \wedge\{\overline{x: \tau}\} \leqslant \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2}$
i.e., $\# C_{0} \wedge\{\overline{x: \tau}\} \wedge\left(\# C \vee \neg \tau_{2}\right) \leqslant \# C \wedge \tau_{1}$
i.e., (1) $\# C_{0} \wedge \# C \wedge\{\overline{x: \tau}\} \vee \# C_{0} \wedge\{\overline{x: \tau}\} \wedge \neg \tau_{2} \leqslant \# C \wedge \tau_{1}$ Then from the assumption, we have:
$$
\begin{align*}
\# C_{0} & \wedge\{\overline{x: \tau}\} \leqslant \# C \wedge \tau_{1} \vee \neg \# C \wedge \tau_{2} \\
\text { i.e., } \# C_{0} & \wedge\{\overline{x: \tau}\} \wedge\left(\# C \vee \neg \tau_{2}\right) \leqslant \# C \wedge \tau_{1} \\
\text { i.e., } \quad \# C_{0} \wedge \# C & \wedge\{\overline{x: \tau}\} \vee \# C_{0}  \tag{1}\\
& \wedge\{\overline{x: \tau}\} \wedge \neg \tau_{2} \leqslant \# C \wedge \tau_{1}
\end{align*}
$$

Case $C \in \mathcal{S}\left(C_{0}\right)$. Then by S-ClsSub, we have:
$$
\begin{gather*}
\# C_{0} \leqslant \# C \\
\text { i.e., } \quad \# C_{0} \wedge \# C \equiv \# C_{0} \tag{2}
\end{gather*}
$$

Then (1) and (2) imply:
$$
\begin{gather*}
\# C_{0} \wedge\{\overline{x: \tau}\} \vee \# C_{0} \wedge\{\overline{x: \tau}\} \wedge \neg \tau_{2} \leqslant \# C \wedge \tau_{1} \\
\text { i.e., } \# C_{0} \wedge\{\overline{x: \tau}\} \leqslant \# C \wedge \tau_{1} \\
\text { i.e., } \tau \leqslant \# C \wedge \tau_{1} \tag{3}
\end{gather*}
$$

By S-Trans on (3) and S-AndOr12>, we have:
$$
\begin{equation*}
\tau \leqslant \tau_{1} \tag{4}
\end{equation*}
$$

Then by T-Subs, the assumption $\epsilon, \Gamma \vdash v: \tau$ and (4) imply:
$$
\begin{equation*}
\epsilon, \Gamma \vdash v: \tau_{1} \tag{5}
\end{equation*}
$$

Case $C \notin \mathcal{S}\left(C_{0}\right)$. By S-Trans on S-AndOr12. and (1), we have:
$$
\begin{align*}
\# C_{0} & \wedge\{\overline{x: \tau}\} \wedge \neg \tau_{2} \leqslant \# C \\
\text { i.e., } \quad \# C_{0} & \wedge\{\overline{x: \tau}\} \leqslant \# C \vee \tau_{2} \tag{6}
\end{align*}
$$

Case $C_{0} \in \mathcal{S}(C)$. This case is impossible because $C_{0}$ is final and $C_{0} \neq C$ (since $C \notin \mathcal{S}\left(C_{0}\right)$ ).
Case $C_{0} \notin \mathcal{S}(C)$. Then by S-ClsBot and Theorem B.20, we have:
$$
\begin{equation*}
\# C_{0} \leqslant \neg \# C \tag{7}
\end{equation*}
$$

Then (6) and (7) imply:
$$
\begin{align*}
& \# C_{0} \wedge\{\overline{x: \tau}\} \wedge \neg \# C \leqslant \tau_{2} \\
& \text { i.e., } \quad \# C_{0} \wedge\{\overline{x: \tau}\} \leqslant \tau_{2} \\
& \text { i.e., } \quad \tau \leqslant \tau_{2} \tag{8}
\end{align*}
$$

Then by T-Subs, the assumption $\epsilon, \Gamma \vdash v: \tau$ and (8) imply:
$$
\begin{equation*}
\epsilon, \Gamma \vdash v: \tau_{2} \tag{9}
\end{equation*}
$$

\section*{B. 14 Type Inference Soundness Proofs}

We first define a few judgements to be used in the remainder of this chapter.
The consistency of subtyping contexts is lifted to typing contexts through the bounds in the polymorphic bindings.

Definition B. 108 (Consistency of typing contexts). The consistency of typing contexts is defined as follows:

$\Gamma$ cons. $\quad \frac{\Gamma \text { cons. }}{\epsilon \text { cons. }} \quad \frac{\Gamma \text { cons } . \quad \Xi \text { cons } .}{\Gamma \cdot(x: \tau) \text { cons } .}$

A constraining context is said to be guarded if none of the type variables appear on the top level of its bounds. Guardedness is also similarly raised to typing contexts.

Definition B. 109 (Guardedness of constraining contexts). The guardedness of constraining contexts is defined as follows:
$\Xi$ guard.

$\overline{\epsilon \text { guard. }} \quad \frac{\alpha \notin T T V(\tau) \quad \Xi \text { guard } .}{\Xi \cdot\left(\alpha \leqslant^{\diamond} \tau\right) \text { guard } .}$

Definition B. 110 (Guardedness of typing contexts). The guardedness of typing contexts is defined as follows:
$\Gamma$ guard.

$\overline{\epsilon \text { guard. }} \quad \frac{\Gamma \text { guard. }}{\Gamma \cdot(x: \tau) \text { guard. }} \quad \frac{\Gamma \text { guard. Eguard. }}{\Gamma \cdot(x: \forall \Xi \cdot \tau) \text { guard. }}$

Lemma B. 111 (Soundness of type inference - general). If $\Gamma \Vdash^{\star} P: \pi \Rightarrow \Xi$ and $\Gamma$ cons. and err $\notin \Xi$, then $\Xi, \Gamma \vdash^{\star} P: \pi$.

Proof. By induction on type inference derivations.
Case I-Body. By soundness of term inference (Lemma B.112).
Case I-Def. By soundness of term inference (Lemma B.112), we get the subtyping relationship necessary to apply the IH on $P$.

Lemma B. 112 (Soundness of term type inference). If $\Xi^{0}, \Gamma \Vdash s: \pi \Rightarrow \Xi^{1}$ and $\Xi^{0}$, $\Gamma$ cons. and $\Xi^{0}, \Gamma$ guard. and err $\notin \Xi^{1}$, then $\Xi^{0} \cdot \Xi^{1}, \Gamma \vdash s: \pi$ and $\Xi^{0} \cdot \Xi^{1}$ cons. and $\Xi^{0} \cdot \Xi^{1}$ guard..

Proof. By induction on term type inference derivations.
Case I-Proj. $s=t . x$
By IH, we have $\Xi_{0} \cdot \Xi_{1} \vdash t: \tau$ and $\Xi_{0} \cdot \Xi_{1}$ cons. and $\Xi_{0} \cdot \Xi_{1}$ guard. And by sound constraining (Lemma 5.6), we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \vdash \tau \leqslant\{x: \alpha\}$ and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ guard.. Therefore, by weakening (Lemma B.34) and T-Subs we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \vdash t:\{x: \alpha\}$ and by T-Proj we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \vdash t . x: \alpha$.
Case I-Obj. By straightforward applications of the IH and weakening.
Case I-Var1. By T-Var1.
Case I-Var2. $\quad t=x \quad \Gamma(x)=\forall \Xi_{1} \cdot \tau_{1}$
Let $\rho=\left[\overline{\alpha \mapsto \gamma \alpha}^{\alpha \in S}\right]$. We have $\Xi^{0} \cdot \rho \Xi_{1} \models \rho \Xi_{1}$ by S-Cons and S-Hyp. We also have $\Xi^{0} \cdot \rho \Xi_{1} \vdash \rho \tau_{1} \leqslant \rho \tau_{1}$ by S-Refl. Then we have $\Xi^{0} \cdot \rho \Xi_{1} \vdash \forall \Xi_{1} \cdot \tau_{1} \leqslant^{\forall} \rho \tau_{1}$ by S-All, and by S-Var2, we have $\Xi^{0} \cdot \rho \Xi_{1}, \Gamma \vdash x: \rho \tau_{1}$ Since $\Gamma$ cons., we have $\left[\overline{\alpha \mapsto \tau}_{\alpha}{ }^{\alpha \in S}\right] \Xi_{1}$ cons. for some ${\overline{\tau_{\alpha}}}^{\alpha \in S}$. Since ${\overline{\gamma_{\alpha} \text { fresh }}}^{\alpha \in S}$, we have $\left[{\overline{\alpha \longmapsto \tau_{\alpha}}}^{\alpha \in S}\right] \Xi_{1}=\left[{\overline{\gamma_{\alpha} \mapsto \tau_{\alpha}}}^{\alpha \in S}\right] \rho \Xi_{1}$. Then $\left[{\overline{\alpha \longmapsto \tau_{\alpha}}}^{\alpha \in S}\right] \Xi_{1}$ cons. implies $\rho \Xi_{1}$ cons.. Similarly, we also have $\rho \Xi_{1}$ guard..
Case I-Abs. By straightforward applications of the IH.
Cases I-App I-Asc, I-Case1. By analogous reasoning to the I-Proj case, applying the IH and sound constraining (Lemma 5.6) successively on the premises, threading the inferred constraints through and weakening accordingly.
Case I-Case2. $\quad t=$ case $x=t_{1}$ of $\rightarrow t_{2}$
By IH, we have $\Xi_{0} \cdot \Xi_{1}$ cons. and $\Xi_{0} \cdot \Xi_{1}$ guard. and $\Xi_{0} \cdot \Xi_{1}, \Gamma \vdash t_{1}: \tau_{1}$, which implies $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}, \Gamma \vdash t_{1}: \tau_{1}$ by weakening. By sound constraining (Lemma 5.6), we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ guard. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \vdash \tau_{1} \leqslant \# C$, which implies $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \vdash \tau_{1} \leqslant \tau_{1} \wedge \# C$ by weakening S-AndOr2 with S-Refl. Then by T-Subs, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}, \Gamma \vdash t_{1}: \tau_{1} \wedge \# C$. By IH, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ guard. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t_{2}: \tau$. Therefore, by T-CASE2, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}, \Gamma \vdash$ case $x=t_{1}$ of $\rightarrow t_{2}: \tau$.
Case I-Case3. $\quad t=$ case $x=t_{1}$ of $C \rightarrow t_{2}, M$
By IH, we have $\Xi_{0} \cdot \Xi_{1}$ cons. and $\Xi_{0} \cdot \Xi_{1}$ guard. and $\Xi_{0} \cdot \Xi_{1}, \Gamma \vdash t_{1}: \tau_{1}$, which implies $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}, \Gamma \vdash t_{1}: \tau_{1}$ by weakening. Then by IH, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}$ guard. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}, \Gamma \cdot(x: \alpha) \vdash t_{2}: \tau_{2}$, which implies $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}, \Gamma \cdot(x: \alpha) \vdash t_{2}: \tau_{2} \vee \tau_{3}$ by weakening and S-Trans with S-AndOr11 . Then by IH again, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ guard. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}, \Gamma \cdot(x: \beta) \vdash$ case $x=x$ of $M$ : $\tau_{3}$, which implies $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}, \Gamma \cdot(x: \beta) \vdash$ case $x=x$ of $M: \tau_{2} \vee \tau_{3}$ by weakening and STrans with S-AndOr12. By sound constraining (Lemma 5.6), we have $\Xi_{4} \cdot \Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ cons. and $\Xi_{4} \cdot \Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3}$ guard. and $\Xi_{4} \cdot \Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \vdash \tau_{1} \leqslant \# C \wedge \alpha \vee \neg \# C \wedge \beta$, which imply $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}$ cons. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}$ guard. and $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4} \vdash \tau_{1} \leqslant \# C \wedge \alpha \vee \neg \# C \wedge \beta$ by commutation. Then by T-Subs, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}, \Gamma \vdash t_{1}: \# C \wedge \alpha \vee \neg \# C \wedge \beta$. Therefore, by T-CASE3, we have $\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2} \cdot \Xi_{3} \cdot \Xi_{4}, \Gamma \vdash$ case $x=t_{1}$ of $C \rightarrow t_{2}, M: \tau_{2} \vee \tau_{3}$.

Proof 5.6 (Soundness of Constraining). By Lemma B. 113 and Theorem B.114.
Lemma B. 113 (Sufficiency of Constraining).
(1) If $\Sigma \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi$ and $\tau_{1}, \tau_{2}$ wf and err $\notin \Xi$, then $\Xi \cdot \Sigma \vdash \tau_{1} \leqslant \tau_{2}$.
(2) If $\Sigma \vdash \mathrm{D}^{0} \Rightarrow \Xi$ and $\mathrm{D}^{0}$ wf and err $\notin \Xi$, then $\Xi \cdot \Sigma \vdash \mathrm{D}^{0} \leqslant \perp$.

Proof. By induction on constraining derivations.
Case C-Hyp. Immediate by S-Hyp.
Case C-Assum. By IH on the latter premise, we have $\Xi \cdot \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \leqslant \perp$. By Lemma 5.3, we have $\operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \equiv \tau_{1} \wedge \neg \tau_{2}$. Then we have $\Xi \cdot \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \tau_{1} \wedge \neg \tau_{2} \leqslant \perp$, which implies $\Xi \cdot \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \tau_{1} \leqslant \tau_{2}$ by Theorem B.20, which implies $\Xi \cdot \Sigma \vdash \tau_{1} \leqslant \tau_{2}$ by S-Assum.
Case C-Or. Then $\mathrm{D}^{0}=\mathrm{D}_{1}^{0} \vee \mathrm{C}_{1}^{0}$ for some $\mathrm{D}_{1}^{0}$ and $\mathrm{C}_{1}^{0}$, and $\Xi=\Xi_{1} \cdot \Xi_{2}$ for some $\Xi_{1}$ and $\Xi_{2}$. By IH on the former premise, we have $\Xi_{1} \cdot \Sigma \vdash \mathrm{D}_{1}^{0} \leqslant \perp$. By IH on the latter premise, we have $\Xi_{2} \cdot \Xi_{1} \cdot \Sigma \vdash \mathrm{C}_{1}^{0} \leqslant \perp$, which implies $\Xi \cdot \Sigma \vdash \mathrm{C}_{1}^{0} \leqslant \perp$ by commutation. $\Xi_{1} \cdot \Sigma \vdash \mathrm{D}_{1}^{0} \leqslant \perp$ implies $\Xi \cdot \Sigma \vdash \mathrm{D}_{1}^{0} \leqslant \perp$ by Lemma B.30. Then by S-AndOr2., we have $\Xi \cdot \Sigma \vdash \mathrm{D}_{1}^{0} \vee \mathrm{C}_{1}^{0} \leqslant \perp$.
Case C-Bot. Immediate by S-Refl.
Case C-Cls1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right)$ for some $C_{1}$ and $C_{2}$ and U . From the premise, we have $\Sigma \vdash \# C_{1} \leqslant \# C_{2}$ by S-ClsSub, which implies $\Sigma \vdash \# C_{1} \wedge \mathcal{F} \wedge \mathcal{R} \leqslant U \vee \# C_{2}$ by S-Trans with S-AndOr11> and S-AndOr12•, which implies $\Sigma \vdash \# C_{1} \wedge \mathcal{F} \wedge \mathcal{R} \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \perp$ by Theorem B.20, i.e., $\Sigma \vdash \mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \perp$.
Case C-Cls2. Then $\mathrm{D}^{0}=\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right)$ for some $C_{1}$ and $C_{2}$ and U . By IH on the latter premise, we have $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\# C_{1}\right] \wedge \neg \mathrm{U} \leqslant \perp$. Since $\neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \neg \mathrm{U}$ by S-AndOr11• and S-NegInv, we have $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \mathcal{I}\left[\# C_{1}\right] \wedge \neg \mathrm{U}$ by Lemma B.22d with S-Refl. Then we have $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \perp$ by S-Trans.
Case C-Cls3. Then $\mathrm{D}^{0}=\mathcal{I}^{\mathcal{N}}[\mathrm{T}] \wedge \neg(\mathrm{U} \vee \# C)$ for some $C$ and U . By IH on the premise, we have $\Xi \cdot \Sigma \vdash \mathcal{I}^{\mathcal{N}}[\mathrm{T}] \wedge \neg \mathrm{U} \leqslant \perp$. Since $\neg(\mathrm{U} \vee \# C) \leqslant \neg \mathrm{U}$ by S-AndOr11• and S-NegInv, we have $\Xi \cdot \Sigma \vdash \mathcal{I}^{\mathcal{N}}[\mathrm{T}] \wedge \neg(\mathrm{U} \vee \# C) \leqslant \mathcal{I}^{\mathcal{N}}[\mathrm{T}] \wedge \neg \mathrm{U}$ by Lemma B.22d with S-Refl. Then we have $\Xi \cdot \Sigma \vdash I^{\mathcal{N}}[\mathrm{T}] \wedge \neg(\mathrm{U} \vee \# C) \leqslant \perp$ by S-Trans.
Case C-Fun1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right)$ for some $\mathrm{D}_{1}$ and $\mathrm{D}_{2}$ and $\mathrm{D}_{3}$ and $\mathrm{D}_{4}$, and $\Xi=\Xi_{1} \cdot \Xi_{2}$ for some $\Xi_{1}$ and $\Xi_{2}$. By IH on the former premise, we have $\Xi_{1} \cdot \triangleleft \Sigma \vdash \mathrm{D}_{3} \leqslant \mathrm{D}_{1}$, which implies $\triangleleft(\Xi \cdot \Sigma) \vdash \mathrm{D}_{3} \leqslant \mathrm{D}_{1}$ by Lemma B.30. By IH on the latter premise, we have $\Xi_{2} \cdot \Xi_{1} \cdot \triangleleft \Sigma \vdash \mathrm{D}_{2} \leqslant \mathrm{D}_{4}$, which implies $\triangleleft(\Xi \cdot \Sigma) \vdash \mathrm{D}_{2} \leqslant \mathrm{D}_{4}$ by Lemma B.30. Then by SFunDepth, we have $\Xi \cdot \Sigma \vdash \mathrm{D}_{1} \rightarrow \mathrm{D}_{2} \leqslant \mathrm{D}_{3} \rightarrow \mathrm{D}_{4}$, which implies $\Xi \cdot \Sigma \vdash \mathcal{N} \wedge \mathrm{D}_{1} \rightarrow \mathrm{D}_{2} \wedge \mathcal{R} \leqslant \mathrm{D}_{3} \rightarrow \mathrm{D}_{4}$ by S-Trans with S-AndOr11> and S-AndOr12>, i.e., $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\mathrm{D}_{1} \rightarrow\right. \left.\mathrm{D}_{2}\right] \leqslant \mathrm{D}_{3} \rightarrow \mathrm{D}_{4}$, which implies $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \leqslant \perp$ by Theorem B.20.
Case C-Rco1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\left\{{\overline{x: \mathrm{D}_{x}}}^{x \in S}\right\}\right] \wedge \neg\{y: \mathrm{D}\}$ for some ${\overline{\mathrm{D}_{x}}}^{x \in S}$ and D . By IH on the premise, we have $\Xi \cdot \triangleleft \Sigma \vdash \mathrm{D}_{y} \leqslant \mathrm{D}$, which implies $\triangleleft(\Xi \cdot \Sigma) \vdash \mathrm{D}_{y} \leqslant \mathrm{D}$ by Lemma B.30. Then by S-RcoDepth, we have $\Xi \cdot \Sigma \vdash\left\{y: \mathrm{D}_{y}\right\} \leqslant\{y: \mathrm{D}\}$, which implies $\Xi \cdot \Sigma \vdash \mathcal{N} \wedge \mathcal{F} \wedge\left\{{\overline{x: \mathrm{D}_{x}}}^{x \in S}\right\} \leqslant\{y: \mathrm{D}\}$ by S-Trans with S-AndOr11> and S-AndOr122, i.e., $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\left\{{\overline{x: \mathrm{D}_{x}}}^{x \in S}\right\}\right] \leqslant\{y: \mathrm{D}\}$, which implies $\Xi \cdot \Sigma \vdash \mathcal{I}\left[\left\{{\overline{x: \mathrm{D}_{x}}}^{x \in S}\right\}\right] \wedge \neg\{y: \mathrm{D}\} \leqslant \perp$ by Theorem B.20.
Cases C-NotBot, C-Fun2, C-Rco2, C-Rco3. Then $\boldsymbol{e r r} \in \boldsymbol{\Xi}$.
Case C-Var1. By S-Hyp, we have $\Xi \cdot(\alpha \leqslant \neg C) \cdot \Sigma \vdash \alpha \leqslant \neg C$, which implies $\Xi \cdot(\alpha \leqslant \neg C) \cdot \Sigma \vdash \mathrm{C} \wedge \alpha \leqslant \perp$ by Theorem B.20.
Case C-Var2. By S-Hyp, we have $\Xi \cdot(\alpha \leqslant \mathrm{C}) \cdot \Sigma \vdash \alpha \leqslant \mathrm{C}$, which implies $\Xi \cdot(\alpha \leqslant \mathrm{C}) \cdot \Sigma \vdash \alpha \wedge \neg \mathrm{C} \leqslant$ ⟂ by Theorem B.20.

\begin{figure}
\includegraphics[alt={},max width=\textwidth]{https://cdn.mathpix.com/cropped/47ae8222-9bd7-4d6b-9bb1-eaf0d638437c-123.jpg?height=1081&width=1390&top_left_y=298&top_left_x=148}
\captionsetup{labelformat=empty}
\caption{Fig. 22. Reformulated normal form constraining rules. The only difference with the rules of Figure 8 is that we now explicitly split the subtyping context into a constraining part $\Xi$ and a plain subtyping part $\Sigma$.}
\end{figure} \(\square\)

Theorem B. 114 (Consistency of constraining). If $\Xi$ cons. and $\Xi$ guard. and $\Xi \vdash \tau \ll \pi \Rightarrow \Xi^{\prime}$ and $\boldsymbol{\operatorname { e r r }} \notin \Xi^{\prime}$, then $\Xi \cdot \Xi^{\prime}$ cons. and $\Xi \cdot \Xi^{\prime}$ guard..

Proof. By Lemma B.115. \(\square\)

In the remainder of this section, we consider the reformulated type constraining rules in Figure 22. In these rules, we assume that we always start derivations with an empty $\Sigma$, so that we start only with bounds, and all these bounds are in $\Xi$. It is easy to see that they are equivalent to the ones presented in Figure 8.

\section*{Lemma B. 115 (Consistency of constraining).}
(1) If $\triangleleft \Sigma \cdot \Delta \vdash \Xi$; $\rho$ cons. and $\Xi$ guard. and $\Xi, \Sigma \vdash \tau \ll \pi \Rightarrow \Xi^{\prime}$ and err $\notin \Xi^{\prime}$, then $\triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi^{\prime}$; $\rho^{\prime}$ cons. and $\Xi \cdot \Xi^{\prime}$ guard. for some $\rho^{\prime}$.
(2) If $\triangleleft \Sigma \cdot \Delta \vdash \Xi ; \rho$ cons. and $\Xi$ guard. and $\Xi, \Sigma \vdash \bigvee_{i \in 1 . . n} \mathrm{C}_{i}^{0} \Rightarrow \Xi^{\prime}$ and $\overline{T T V^{\prime}\left(\mathrm{C}_{i}^{0}\right) \text { are distinct }}$ and err $\notin \Xi^{\prime}$, then $\triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime}$ cons. and $\Xi \cdot \Xi^{\prime}$ guard. for some $\rho^{\prime}$.

Proof. By induction on constraining derivations.
Cases C-Hyp, C-Bot, C-Cls1. Immediate since $\Xi^{\prime}=\epsilon$.

Case C-Assum. Then the premise of the rule is:
$$
\begin{equation*}
\Xi, \Sigma \cdot \triangleright(\tau \leqslant \pi) \vdash \mathrm{dnf}^{0}(\tau \wedge \neg \pi) \Rightarrow \Xi^{\prime} \tag{1}
\end{equation*}
$$

From the assumptions, we have:
$$
\begin{equation*}
\triangleleft \Sigma \cdot \Delta \vdash \Xi ; \rho^{\prime} \text { cons. } \tag{2}
\end{equation*}
$$

By Lemma B. 33 with Lemma B.25, (2) implies:
$$
\begin{equation*}
\triangleleft(\Sigma \cdot \triangleright(\tau \leqslant \pi)) \cdot \Delta \vdash \Xi ; \rho^{\prime} \text { cons. } \tag{3}
\end{equation*}
$$
for some $\rho^{\prime}$. Then by IH on (3) and (1), we have:
$$
\begin{array}{ll} 
& \triangleleft(\Sigma \cdot \triangleright(\tau \leqslant \pi)) \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime} \text { cons. } \\
\text { i.e., } & \triangleleft \Sigma \cdot(\tau \leqslant \pi) \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime} \text { cons. } \tag{4}
\end{array}
$$

By Lemma B.113, $\Xi, \Sigma \vdash \tau \ll \pi \Rightarrow \Xi^{\prime}$ implies:
$$
\begin{equation*}
\Xi \cdot \Xi^{\prime} \cdot \Sigma \vdash \tau \leqslant \pi \tag{5}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (5) implies:
$$
\begin{equation*}
\Xi \cdot \Xi^{\prime} \cdot \triangleleft \Sigma \cdot \Delta \vdash \tau \leqslant \pi \tag{6}
\end{equation*}
$$

Then by Lemma B. 33 with (6), (4) implies:
$$
\begin{equation*}
\triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime} \text { cons. } \tag{7}
\end{equation*}
$$

Case C-Or. Then the premises of the rule are:
$$
\begin{gather*}
\Xi, \Sigma \vdash \bigvee_{i \in 1 . . n-1} C_{i}^{0} \Rightarrow \Xi_{1}^{\prime}  \tag{8}\\
\Xi \cdot \Xi_{1}^{\prime}, \Sigma \vdash C_{n}^{0} \Rightarrow \Xi_{2}^{\prime} \tag{9}
\end{gather*}
$$
where $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}$. Then by IH on (8), we have:
$$
\begin{equation*}
\triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi_{1}^{\prime} ; \rho^{\prime \prime} \text { cons. } \tag{10}
\end{equation*}
$$
for some $\rho^{\prime \prime}$. Then by IH on (10) and (9), we have:
$$
\begin{array}{ll} 
& \triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime} ; \rho^{\prime} \text { cons. } \\
\text { i.e., } & \triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime} \text { cons. } \tag{11}
\end{array}
$$
for some $\rho^{\prime}$.
Cases C-Cls2, C-Cls3, C-Rco1. Immediate by IH on the premise.
Case C-Fun1. Similar to case C-Or.
Case C-Var1. Then the premise of the rule is:
$$
\begin{equation*}
\Xi \cdot(\alpha \leqslant \neg \mathrm{C}), \Sigma \vdash l b_{\Xi}(\alpha) \ll \neg \mathrm{C} \Rightarrow \Xi_{1}^{\prime} \tag{12}
\end{equation*}
$$
where $\bigvee_{i \in 1 . . n} \mathrm{C}_{\mathrm{i}}^{0}=\mathrm{C} \wedge \alpha$ and $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot(\alpha \leqslant \neg \mathrm{C})$ for some $\alpha$ and C and $\Xi_{1}^{\prime}$. From the assumption, we have:
$$
\begin{equation*}
\triangleleft \Sigma \cdot \Delta \vdash \Xi ; \rho \text { cons. } \tag{13}
\end{equation*}
$$

By Lemma B. 33 with Lemma B.25, (13) implies:
$$
\begin{equation*}
(\alpha \leqslant \neg \mathrm{C}) \cdot \Xi_{1}^{\prime} \cdot \triangleleft \Sigma \cdot \Delta \vdash \Xi ; \rho \text { cons. } \tag{14}
\end{equation*}
$$

Since $T T V^{\prime}(\mathrm{C} \wedge \alpha)$ are distinct, by the syntax of RDNF, we have $\alpha \notin T T V(\mathrm{C})$. Then we have:
$$
\begin{equation*}
\Xi \cdot(\alpha \leqslant \neg C) \text { guard. } \tag{15}
\end{equation*}
$$

Since (15) implies $\alpha \notin \operatorname{TTV}\left(l b_{\Xi}(\alpha)\right) \cup \operatorname{TTV}(\neg \mathrm{C})$, by Lemma B. 117 on (12) followed by Lemma B.30, we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \neg \mathrm{C}) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Xi_{1}^{\prime} \cdot \Sigma\right) \vdash l b_{\Xi}(\alpha) \leqslant \neg \mathrm{C} \tag{16}
\end{equation*}
$$
where $\operatorname{split}_{\alpha}(\Xi, d o m(\rho) \backslash\{\alpha\})=\left(\Xi_{\alpha}, \Xi_{\not \alpha}\right)$ and $\rho_{\alpha}^{\prime}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) \vee l b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha)\right]$. By Lemma B. 30 with Lemma B.25, (16) implies:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \neg \mathrm{C}) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Xi_{1}^{\prime} \cdot \triangleleft \Sigma\right) \vdash l b_{\Xi}(\alpha) \leqslant \neg \mathrm{C} \tag{17}
\end{equation*}
$$

Then by Lemma B. 116 on (14), (15), and (17), we have:
$$
\begin{array}{cc} 
& \Xi_{1}^{\prime} \cdot \triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot(\alpha \leqslant \neg C) ; \rho^{\prime} \text { cons. } \\
\text { i.e., } \quad \triangleleft \Sigma \cdot\left(\Xi_{1}^{\prime} \cdot \Delta\right) \vdash \Xi \cdot(\alpha \leqslant \neg C) ; \rho^{\prime} \text { cons. } \tag{18}
\end{array}
$$
for some $\rho^{\prime}$. Then by IH on (18) and (12), we have:
$$
\begin{equation*}
\triangleleft \Sigma \cdot\left(\Xi_{1}^{\prime} \cdot \Delta\right) \vdash \Xi \cdot(\alpha \leqslant \neg C) \cdot \Xi_{1}^{\prime} ; \rho^{\prime} \text { cons. } \tag{19}
\end{equation*}
$$

By Lemma B.25, we have:
$$
\begin{equation*}
\Xi \cdot(\alpha \leqslant \neg \mathrm{C}) \cdot \Xi_{1}^{\prime} \cdot \triangleleft \Sigma \cdot \Delta \vDash \neg \Sigma \cdot\left(\Xi_{1}^{\prime} \cdot \Delta\right) \tag{20}
\end{equation*}
$$

Then by Lemma B. 33 with (20), (19) implies:
$$
\begin{align*}
& \triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot(\alpha \leqslant \neg \mathrm{C}) \cdot \Xi_{1}^{\prime} ; \rho^{\prime} \text { cons. } \\
& \text { i.e., } \quad \triangleleft \Sigma \cdot \Delta \vdash \Xi \cdot \Xi^{\prime} ; \rho^{\prime} \text { cons. } \tag{21}
\end{align*}
$$

Case C-Var2. Similar to case C-Var1.

Lemma B.116. If $\left(\alpha \leqslant^{\diamond} \tau\right) \cdot \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons. and $\Xi \cdot\left(\alpha \leqslant^{\diamond} \tau\right)$ guard. and $\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \vdash l b_{\Xi}^{\diamond}(\alpha) \leqslant \tau$, where split ${ }_{\alpha}(\Xi, \operatorname{dom}(\rho) \backslash\{\alpha\})=\left(\Xi_{\alpha}, \Xi_{\alpha}\right)$, then $\Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi \cdot\left(\alpha \leqslant^{\diamond} \tau\right)$; $\rho^{\prime}$ cons. for some $\rho^{\prime}$, where $\rho_{\alpha}^{\prime}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi \cdot\left(\alpha \leqslant^{\diamond} \tau\right)}(\alpha) \vee l b_{\Xi \cdot\left(\alpha \leqslant^{\diamond} \tau\right)}(\alpha)\right]$.

The proof for the ⋅ direction is shown below. The $\downarrow$ direction is symmetric.
Proof. By Lemma B.44, $(\alpha \leqslant \tau) \cdot \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \Xi ; \rho$ cons. implies:
$$
\begin{gather*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} \cdot \rho_{\alpha}((\alpha \leqslant \tau) \cdot \Sigma) \models \rho_{\alpha} \Xi_{\alpha}  \tag{1}\\
\rho_{\alpha}((\alpha \leqslant \tau) \cdot \Sigma) \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\not \alpha} ; \rho_{1}^{\prime} \text { cons. } \tag{2}
\end{gather*}
$$
for some $\rho_{1}^{\prime}$, where $\rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right]$ and $\operatorname{dom}\left(\rho_{1}^{\prime}\right)=\operatorname{dom}(\rho) \backslash\{\alpha\}$.
Let $\rho_{\tau}=[\alpha \mapsto \alpha \wedge \tau]$. By Lemma B. 36 on (1), we have:
$$
\begin{equation*}
\rho_{\tau}\left(\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\not \alpha} \cdot \rho_{\alpha}((\alpha \leqslant \tau) \cdot \Sigma)\right) \models \rho_{\tau} \rho_{\alpha} \Xi_{\alpha} \tag{3}
\end{equation*}
$$

By Corollary B. 40 and Corollary B.41, we have:
$$
\begin{gather*}
(\alpha \leqslant \tau) \vdash \pi \equiv \rho_{\tau} \pi \quad \text { for all } \pi  \tag{4}\\
\triangleright(\alpha \leqslant \tau) \vdash \pi \equiv \rho_{\tau} \pi \quad \text { for all } \pi \text { where } \alpha \notin \operatorname{TTV}(\pi) \tag{5}
\end{gather*}
$$

By S-Trans on Lemma B. 25 and (4), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \models \rho_{\tau}\left(\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha}\right) \tag{6}
\end{equation*}
$$

Then by Lemma B. 30 on (3) with (6), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\tau} \rho_{\alpha}\left(\Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \Sigma\right) \models \rho_{\tau} \rho_{\alpha} \Xi_{\alpha} \tag{7}
\end{equation*}
$$

Expanding the composition, we have:
$$
\begin{equation*}
\rho_{\tau} \circ \rho_{\alpha}=\left[\alpha \mapsto \alpha \wedge \tau \wedge \rho_{\tau} u b_{\Xi}(\alpha) \vee \rho_{\tau} l b_{\Xi}(\alpha)\right] \tag{8}
\end{equation*}
$$

By Lemma B. 22 on S-Refl and (5), we have:
$$
\begin{gather*}
\triangleright(\alpha \leqslant \tau) \vdash \alpha \wedge \tau \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge \tau \wedge \rho_{\tau} u b_{\Xi}(\alpha) \vee \rho_{\tau} l b_{\Xi}(\alpha) \\
\text { i.e., } \quad \triangleright(\alpha \leqslant \tau) \vdash \alpha \wedge u b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) \vee l b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) \equiv \alpha \wedge \tau \wedge \rho_{\tau} u b_{\Xi}(\alpha) \vee \rho_{\tau} l b_{\Xi}(\alpha) \tag{9}
\end{gather*}
$$

Then by Lemma B. 38 on (9), we have:
$$
\begin{equation*}
\triangleright(\alpha \leqslant \tau) \vdash \rho_{\alpha}^{\prime} \pi \equiv \rho_{\tau} \rho_{\alpha} \pi \quad \text { for all } \pi \tag{10}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (10), we have:
$$
\begin{gather*}
\triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\mathscr{X}} \cdot(\alpha \leqslant \tau) \cdot \Sigma\right) \models \rho_{\tau} \rho_{\alpha}\left(\Xi_{\mathscr{X}} \cdot(\alpha \leqslant \tau) \cdot \Sigma\right)  \tag{11}\\
\rho_{\tau} \rho_{\alpha} \Xi_{\alpha} \models \rho_{\alpha}^{\prime} \Xi_{\alpha} \tag{12}
\end{gather*}
$$

Then by Lemma B. 30 on (7) with (11), followed by Lemma B. 26 with (12), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \Sigma\right) \models \rho_{\alpha}^{\prime} \Xi_{\alpha} \tag{13}
\end{equation*}
$$

From the assumption, we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\not \alpha} \cdot \Sigma\right) \vdash l b_{\Xi}(\alpha) \leqslant \tau \tag{14}
\end{equation*}
$$

By S-AndOr2- on S-AndOr11>/S-AndOr12> and (14), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\mathscr{X}} \cdot \Sigma\right) \vdash \alpha \wedge \tau \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \leqslant \tau \tag{15}
\end{equation*}
$$

By Corollary B.41, we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \vdash \tau \equiv \rho_{\alpha}^{\prime} \tau \tag{16}
\end{equation*}
$$

Then by S-Trans on (15) and (16), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\not,} \cdot \Sigma\right) \vdash \rho_{\alpha}^{\prime} \alpha \leqslant \rho_{\alpha}^{\prime} \tau \tag{17}
\end{equation*}
$$

Then by Lemma B. 30 on (13) with (17), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \models \rho_{\alpha}^{\prime} \Xi_{\alpha} \tag{18}
\end{equation*}
$$

By S-Cons on (18) with (17), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \models \rho_{\alpha}^{\prime} \Xi_{\alpha} \cdot \rho_{\alpha}^{\prime}(\alpha \leqslant \tau) \tag{19}
\end{equation*}
$$

By S-Trans on S-AndOr11>, we have:
$$
\begin{equation*}
(\alpha \leqslant \tau) \vdash \alpha \wedge u b_{\Xi}(\alpha) \leqslant \tau \tag{20}
\end{equation*}
$$

Then by S-AndOr2 ⋅ on (20) and (14), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \leqslant \tau \tag{21}
\end{equation*}
$$

By Corollary B.41, we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \vdash \tau \equiv \rho_{\alpha} \tau \tag{22}
\end{equation*}
$$

Then by S-Trans on (21) and (22), we have:
$$
\begin{align*}
& \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \leqslant \rho_{\alpha} \tau \\
& \text { i.e., } \quad \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime}\left(\Xi_{\alpha} \cdot \Sigma\right) \vdash \rho_{\alpha} \alpha \leqslant \rho_{\alpha} \tau \tag{23}
\end{align*}
$$

By S-AndOr22 and Lemma B. 22 on S-Hyp and S-Refl, we have:
$$
\begin{gather*}
(\alpha \leqslant \tau) \vdash \alpha \wedge \tau \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \\
\text { i.e., } \quad(\alpha \leqslant \tau) \vdash \alpha \wedge u b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) \vee l b_{\Xi \cdot(\alpha \leqslant \tau)}(\alpha) \equiv \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \tag{24}
\end{gather*}
$$

By Lemma B. 38 on (24), we have:
$$
\begin{equation*}
(\alpha \leqslant \tau) \vdash \rho_{\alpha}^{\prime} \pi \equiv \rho_{\alpha} \pi \quad \text { for all } \pi \tag{25}
\end{equation*}
$$

By S-Trans on Lemma B. 25 and (25), we have:
$$
\begin{equation*}
\rho_{\alpha}\left(\Xi_{\not \mathscr{X}} \cdot \Sigma\right) \models \rho_{\alpha}^{\prime}\left(\Xi_{\not \mathscr{X}} \cdot \Sigma\right) \tag{26}
\end{equation*}
$$

Then by Lemma B. 30 on (23) with (26), we have:
$$
\begin{equation*}
\triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot(\alpha \leqslant \tau) \cdot \rho_{\alpha}\left(\Xi_{\alpha} \cdot \Sigma\right) \vdash \rho_{\alpha} \alpha \leqslant \rho_{\alpha} \tau \tag{27}
\end{equation*}
$$

Then by Lemma B. 36 and Lemma B. 30 with (27), (2) implies:
$$
\begin{equation*}
(\alpha \leqslant \tau) \cdot \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \rho_{\alpha} \Xi_{\alpha} ; \rho_{1}^{\prime} \text { cons. } \tag{28}
\end{equation*}
$$

Then by Lemma B. 50 on (28), we have:
$$
\begin{equation*}
\rho_{\tau} \rho_{\alpha} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\tau} \rho_{\alpha} \Xi_{\not \alpha} ; \rho_{2}^{\prime} \text { cons. } \tag{29}
\end{equation*}
$$
for some $\rho_{2}^{\prime}$. By Lemma B. 43 on (29) with (9), we have:
$$
\begin{equation*}
\rho_{\alpha}^{\prime} \Sigma \vdash \triangleright \Xi_{\triangleright} \cdot \triangleright \Xi_{\alpha} \cdot \triangleright(\alpha \leqslant \tau) \cdot \rho_{\alpha}^{\prime} \Xi_{\not \alpha} ; \rho_{3}^{\prime} \text { cons. } \tag{30}
\end{equation*}
$$
for some $\rho_{3}^{\prime}$. Then by the definition of consistency on (19) and (30), we have:
$$
\begin{equation*}
\Sigma \vdash \Xi \cdot(\alpha \leqslant \tau) ; \rho_{3}^{\prime} \circ \rho_{\alpha}^{\prime} \text { cons. } \tag{31}
\end{equation*}
$$

Lemma B.117.
(1) If $\Xi, \Sigma \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi^{\prime}$ and $\alpha \notin T T V\left(\tau_{1}\right) \cup T T V\left(\tau_{2}\right)$ and $\operatorname{err} \notin \Xi^{\prime}$, then $\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \partial} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \tau_{1} \leqslant \tau_{2}$, where split ${ }_{\alpha}(\Xi, \varnothing)=\left(\Xi_{\alpha}, \Xi_{\not \alpha}\right)$ and $\rho=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right]$.
(2) If $\Xi, \Sigma \vdash \mathrm{D}^{0} \Rightarrow \Xi^{\prime}$ and $\alpha \notin T T V\left(\mathrm{D}^{0}\right)$ and err $\notin \Xi^{\prime}$, where $\mathrm{D}^{0}=\bigvee_{i \in 1 . . n} \mathrm{C}_{i}^{0}$, then $\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \chi} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathrm{D}^{0} \leqslant \perp$, where split ${ }_{\alpha}(\Xi, \varnothing)=\left(\Xi_{\alpha}, \Xi_{\not \alpha}\right)$ and $\rho=\left[\alpha \mapsto \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right]$.

Proof. By induction on constraining derivations.
Case C-Hyp. Since $\alpha \notin T T V\left(\tau_{1}\right) \cup T T V\left(\tau_{2}\right)$, we have from the premise:
$$
\begin{align*}
\left(\tau_{1} \leqslant \tau_{2}\right) & \in \Xi_{\mathscr{X}} \cdot \Sigma \\
\text { i.e., } \quad\left(\rho \tau_{1} \leqslant \rho \tau_{2}\right) & \in \rho\left(\Xi_{\not \mathscr{X}} \cdot \Sigma\right) \tag{1}
\end{align*}
$$

Then by S-Hyp on (1), we have:
$$
\begin{equation*}
\rho\left(\Xi_{\not \mathscr{X}} \cdot \Sigma\right) \vdash \rho \tau_{1} \leqslant \rho \tau_{2} \tag{2}
\end{equation*}
$$

By Corollary B.41, we have:
$$
\begin{align*}
& \triangleright \Sigma_{\alpha} \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{3}\\
& \triangleright \Sigma_{\alpha} \vdash \tau_{2} \equiv \rho \tau_{2} \tag{4}
\end{align*}
$$

Then by S-Trans on (2), (3), and (4), we have:
$$
\begin{equation*}
\triangleright \Sigma_{\alpha} \cdot \rho\left(\Xi_{\not \mathscr{C}} \cdot \Sigma\right) \vdash \tau_{1} \leqslant \tau_{2} \tag{5}
\end{equation*}
$$

Case C-Assum. Then the premise of the rule is:
$$
\begin{equation*}
\Xi, \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \mathrm{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \Rightarrow \Xi^{\prime} \tag{6}
\end{equation*}
$$

By IH on (6), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\chi} \cdot \Xi^{\prime} \cdot \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right)\right) \vdash \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \leqslant \perp \tag{7}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{align*}
& \Xi_{\alpha} \vdash \tau_{1} \equiv \rho \tau_{1}  \tag{8}\\
& \Xi_{\alpha} \vdash \tau_{2} \equiv \rho \tau_{2} \tag{9}
\end{align*}
$$

Then by S-Trans on Lemma B.25, (8), and (9), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vDash \rho \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \tag{10}
\end{equation*}
$$

Then by Lemma B. 30 with (10), (7) implies:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\mathscr{X}} \cdot \Xi^{\prime} \cdot \Sigma\right) \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \leqslant \perp \tag{11}
\end{equation*}
$$

By S-Trans on Lemma 5.3 and (11), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\mathscr{X}} \cdot \Xi^{\prime} \cdot \Sigma\right) \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \tau_{1} \wedge \neg \tau_{2} \leqslant \perp \tag{12}
\end{equation*}
$$

By Theorem B. 20 on (12), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \partial} \cdot \Xi^{\prime} \cdot \Sigma\right) \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \tau_{1} \leqslant \tau_{2} \tag{13}
\end{equation*}
$$

By S-Assum on (13), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\alpha} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \tau_{1} \leqslant \tau_{2} \tag{14}
\end{equation*}
$$

Case C-Or. It is easy to see that if $T T V^{\prime}\left(\mathrm{C}_{k}^{0}\right)$ are not distinct for some $k$, we can deduplicate them before preceeding, and duplicate them again in the conclusion. Therefore we can assume that ${\overline{T T V^{\prime}\left(\mathrm{C}_{i}^{0}\right) \text { are distinct }}}^{i \in 1 . . n}$.

The premises of the rule are:
$$
\begin{gather*}
\Xi, \Sigma \vdash \bigvee_{i \in 1 . . n-1} \mathrm{C}_{i}^{0} \Rightarrow \Xi_{1}^{\prime}  \tag{15}\\
\Xi \cdot \Xi^{\prime}, \Sigma \vdash \mathrm{C}_{n}^{0} \Rightarrow \Xi_{2}^{\prime} \tag{16}
\end{gather*}
$$
where $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}$ for some $\Xi_{1}^{\prime}$ and $\Xi_{2}^{\prime}$. By IH on (15), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \mathscr{X}} \cdot \Xi_{1}^{\prime} \cdot \Sigma\right) \vdash \bigvee_{i \in 1 . . n-1} \mathrm{C}_{i}^{0} \leqslant \perp \tag{17}
\end{equation*}
$$

By IH on (16), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright \Xi_{1 \alpha}^{\prime} \cdot \rho^{\prime}\left(\Xi_{\not \alpha} \cdot \Xi_{1 \neq}^{\prime} \cdot \Xi_{2}^{\prime} \cdot \Sigma\right) \vdash \mathrm{C}_{n}^{0} \leqslant \perp \tag{18}
\end{equation*}
$$
where $\operatorname{split}_{\alpha}\left(\Xi_{1}^{\prime}, \varnothing\right)=\left(\Xi_{1 \alpha}^{\prime}, \Xi_{1 \alpha}^{\prime}\right)$ and $\rho^{\prime}=\left[\alpha \mapsto \alpha \wedge u b_{\Xi \cdot \Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi \cdot \Xi_{1}^{\prime}}(\alpha)\right]$.
By Lemma B. 118 on (15), we have:
$$
\begin{equation*}
\Xi_{1}^{\prime} \text { guard. } \tag{19}
\end{equation*}
$$

By Lemma B.25, we have:
$$
\begin{equation*}
\rho \Xi_{1}^{\prime} \models \rho \Xi_{1 \alpha}^{\prime} \tag{20}
\end{equation*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv \rho \pi \quad \text { for all } \pi \tag{21}
\end{gather*}
$$

Then by S-Trans on (20) and (21), we have:
$$
\begin{equation*}
\Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \models \Xi_{1 \alpha}^{\prime} \tag{22}
\end{equation*}
$$

By Lemma B. 28 on (22), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \triangleright \rho \Xi_{1}^{\prime} \models \triangleright \Xi_{1 \alpha}^{\prime} \tag{23}
\end{equation*}
$$

By Lemma B. 26 on (23) and Lemma B.25, we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \models \triangleright \Xi_{1 \alpha}^{\prime} \tag{24}
\end{equation*}
$$

By Corollary B.41, we have:
$$
\begin{gather*}
\triangleright \Xi_{\alpha} \vdash u b_{\Xi_{1}^{\prime}}(\alpha) \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right] u b_{\Xi_{1}^{\prime}}(\alpha) \\
\text { i.e., } \quad \triangleright \Xi_{\alpha} \vdash u b_{\Xi_{1}^{\prime}}(\alpha) \equiv \rho u b_{\Xi_{1}^{\prime}}(\alpha) \tag{25}
\end{gather*}
$$

By S-AndOr2> on S-Hyp, we have:
$$
\begin{gather*}
\rho \Xi_{1}^{\prime} \vdash \rho \alpha \leqslant \rho u b_{\Xi_{1}^{\prime}}(\alpha) \\
\text { i.e., } \quad \rho \Xi_{1}^{\prime} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \leqslant \rho u b_{\Xi_{a}^{\prime}}(\alpha) \tag{26}
\end{gather*}
$$

Then by S-Trans on S-AndOr12•, (26) and (25), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash l b_{\Xi}(\alpha) \leqslant u b_{\Xi_{1}^{\prime}}(\alpha) \tag{27}
\end{equation*}
$$

By S-AndOr2> on S-Refl and (27), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash l b_{\Xi}(\alpha) \leqslant l b_{\Xi}(\alpha) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \tag{28}
\end{equation*}
$$

Then by S-AndOr11> and (28), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash l b_{\Xi}(\alpha) \equiv l b_{\Xi}(\alpha) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \tag{29}
\end{equation*}
$$

Then by (29) and S-Distr, we have:
$$
\begin{align*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash & \alpha \wedge u b_{\Xi \Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi \cdot \Xi_{1}^{\prime}}(\alpha) \\
= & \alpha \wedge u b_{\Xi}(\alpha) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi}(\alpha) \vee l b_{\Xi_{1}^{\prime}}(\alpha)  \tag{30}\\
\equiv & \alpha \wedge u b_{\Xi}(\alpha) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi}(\alpha) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi_{1}^{\prime}}(\alpha) \\
\equiv & \left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi_{1}^{\prime}}(\alpha)
\end{align*}
$$

By S-AndOr2> on S-Refl and S-Hyp, followed by S-Trans with S-AndOr11•, we have:
$$
\begin{equation*}
\rho \Xi_{1}^{\prime} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \leqslant\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \wedge \rho u b_{\Xi_{1}^{\prime}}(\alpha) \vee \rho l b_{\Xi_{1}^{\prime}}(\alpha) \tag{31}
\end{equation*}
$$

Similarly, by S-AndOr2- on S-Refl and S-Hyp, followed by S-Trans with S-AndOr11>, we have:
$$
\begin{equation*}
\rho \Xi_{1}^{\prime} \vdash\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \wedge \rho u b_{\Xi_{1}^{\prime}}(\alpha) \vee \rho l b_{\Xi_{1}^{\prime}}(\alpha) \leqslant \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \tag{32}
\end{equation*}
$$

By Corollary B.41, we have:
$$
\begin{align*}
\triangleright \Xi_{\alpha} \vdash u b_{\Xi_{1}^{\prime}}(\alpha) & \equiv \rho u b_{\Xi_{1}^{\prime}}(\alpha)  \tag{33}\\
\triangleright \Xi_{\alpha} \vdash l b_{\Xi_{1}^{\prime}}(\alpha) & \equiv \rho l b_{\Xi_{1}^{\prime}}(\alpha) \tag{34}
\end{align*}
$$

Then by S-Trans on (31)/(32), (33), and (34), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv\left(\alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha)\right) \wedge u b_{\Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi_{1}^{\prime}}(\alpha) \tag{35}
\end{equation*}
$$

Then by S-Trans on (35) and (30), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash \alpha \wedge u b_{\Xi}(\alpha) \vee l b_{\Xi}(\alpha) \equiv \alpha \wedge u b_{\Xi \cdot \Xi_{1}^{\prime}}(\alpha) \vee l b_{\Xi \cdot \Xi_{1}^{\prime}}(\alpha) \tag{36}
\end{equation*}
$$

By Lemma B. 38 on (36), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho \Xi_{1}^{\prime} \vdash \rho \pi \equiv \rho^{\prime} \pi \quad \text { for all } \pi \tag{37}
\end{equation*}
$$

Then by S-Trans on Lemma B. 25 and (37), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \alpha} \cdot \Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime} \cdot \Sigma\right) \vDash \rho^{\prime}\left(\Xi_{\not \alpha} \cdot \Xi_{1 \neq}^{\prime} \cdot \Xi_{2}^{\prime} \cdot \Sigma\right) \tag{38}
\end{equation*}
$$

Then by Lemma B. 30 with (24) and (38), (18) implies:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\chi} \cdot \Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime} \cdot \Sigma\right) \models C_{n}^{0} \leqslant \perp \tag{39}
\end{equation*}
$$

Then by S-AndOr2- on (17) and (39), we have:
$$
\begin{align*}
& \triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \alpha} \cdot \Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime} \cdot \Sigma\right) \vdash \bigvee_{i \in 1 . . n} \mathrm{C}_{i}^{0} \leqslant \perp \\
& \text { i.e., } \quad \triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\not \alpha} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathrm{D}^{0} \leqslant \perp \tag{40}
\end{align*}
$$

Case C-Bot. Immediate by S-ToB?.
Case C-Cls1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right)$ for some $C_{1}, C_{2}, \mathcal{I}\left[\# C_{1}\right]$, and U. By S-ClsSub on the premise $C_{2} \in S\left(\# C_{1}\right)$, we have:
$$
\begin{equation*}
\# C_{1} \leqslant \# C_{2} \tag{41}
\end{equation*}
$$

By S-Trans on S-AndOr11>, (41), and S-AndOr12•, we have:
$$
\begin{equation*}
\mathcal{I}\left[\# C_{1}\right] \leqslant \cup \vee \# C_{2} \tag{42}
\end{equation*}
$$

Then by Theorem B.20, (42) implies:
$$
\begin{equation*}
\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right) \leqslant \perp \tag{43}
\end{equation*}
$$

Cases C-Cls2, C-Cls3. Then $\mathrm{D}^{0}=\mathcal{I}^{\mathcal{N}}[\mathcal{N}] \wedge \neg(\mathrm{U} \vee \# C)$ for some $\mathcal{N}, C, \mathcal{I}^{\mathcal{N}}[\mathcal{N}]$, and U . The premise of the rule is:
$$
\begin{equation*}
\Xi, \Sigma \vdash \mathcal{I}^{\mathcal{N}}[\mathcal{N}] \wedge \neg \mathrm{U} \Rightarrow \Xi^{\prime} \tag{44}
\end{equation*}
$$

By IH on (44), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\mathscr{X}} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathcal{I}^{\mathcal{N}}[\mathcal{N}] \wedge \neg U \leqslant \perp \tag{45}
\end{equation*}
$$

By S-AndOr11• followed by S-NegInv, we have:
$$
\begin{equation*}
\neg(\mathrm{U} \vee \# C) \leqslant \neg \mathrm{U} \tag{46}
\end{equation*}
$$

Then by S-Trans on (46) and (45), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\chi} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathcal{I}^{\mathcal{N}}[\mathcal{N}] \wedge \neg(\mathrm{U} \vee \# C) \leqslant \perp \tag{47}
\end{equation*}
$$

Case C-Fun1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right)$ for some ${\overline{\mathrm{D}_{j}}}^{j \in 1 . .4}$ and $\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]$. The premises of the rule are:
$$
\begin{gather*}
\Xi, \triangleleft \Sigma \vdash \mathrm{D}_{3}<\mathrm{D}_{1} \Rightarrow \Xi_{1}^{\prime}  \tag{48}\\
\Xi \cdot \Xi_{1}^{\prime}, \triangleleft \Sigma \vdash \mathrm{D}_{2}<\mathrm{D}_{4} \Rightarrow \Xi_{2}^{\prime} \tag{49}
\end{gather*}
$$
for some $\Xi_{1}^{\prime}$ and $\Xi_{2}^{\prime}$, where $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}$. By Lemma B. 113 on (48) and (49), we have:
$$
\begin{gather*}
\Xi \cdot \triangleleft \Sigma \cdot \Xi_{1}^{\prime} \vdash \mathrm{D}_{3} \leqslant \mathrm{D}_{1}  \tag{50}\\
\Xi \cdot \Xi_{1}^{\prime} \cdot \triangleleft \Sigma \cdot \Xi_{2}^{\prime} \vdash \mathrm{D}_{2} \leqslant \mathrm{D}_{4} \tag{51}
\end{gather*}
$$

By Lemma B. 30 with Lemma B.25, (50) and (51) imply:
$$
\begin{align*}
& \Xi \cdot \Xi^{\prime} \cdot \triangleleft \Sigma \vdash \mathrm{D}_{3} \leqslant \mathrm{D}_{1}  \tag{52}\\
& \Xi \cdot \Xi^{\prime} \cdot \triangleleft \Sigma \vdash \mathrm{D}_{2} \leqslant \mathrm{D}_{4} \tag{53}
\end{align*}
$$

By Corollary B.40, we have:
$$
\begin{gather*}
\Xi_{\alpha} \vdash \pi \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right] \pi \quad \text { for all } \pi \\
\text { i.e., } \quad \Xi_{\alpha} \vdash \pi \equiv \rho \pi \quad \text { for all } \pi \tag{54}
\end{gather*}
$$

By S-Trans on Lemma B. 25 and (54), we have:
$$
\begin{equation*}
\Xi_{\alpha} \cdot \rho\left(\Xi_{\not \alpha} \cdot \Xi^{\prime} \cdot \triangleleft \Sigma\right) \models \Xi_{\not \alpha} \cdot \Xi^{\prime} \cdot \triangleleft \Sigma \tag{55}
\end{equation*}
$$

Then by Lemma B. 30 with (55), (52) and (53) imply:
$$
\begin{align*}
& \Xi_{\alpha} \cdot \rho\left(\Xi_{\chi} \cdot \Xi^{\prime} \cdot \triangleleft \Sigma\right) \vdash \mathrm{D}_{3} \leqslant \mathrm{D}_{1}  \tag{56}\\
& \Xi_{\alpha} \cdot \rho\left(\Xi_{\chi} \cdot \Xi^{\prime} \cdot \triangleleft \Sigma\right) \vdash \mathrm{D}_{2} \leqslant \mathrm{D}_{4} \tag{57}
\end{align*}
$$

Then by S-FunDepth on (56) and (57), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\mathscr{X}} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathrm{D}_{1} \rightarrow \mathrm{D}_{2} \leqslant \mathrm{D}_{3} \rightarrow \mathrm{D}_{4} \tag{58}
\end{equation*}
$$

By S-Trans on S-AndOr11>, S-AndOr12>, and (58), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\alpha} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \leqslant \mathrm{D}_{3} \rightarrow \mathrm{D}_{4} \tag{59}
\end{equation*}
$$

By Theorem B.20, (59) implies:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\mathscr{X}} \cdot \Xi^{\prime} \cdot \Sigma\right) \vdash \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \leqslant \perp \tag{60}
\end{equation*}
$$

Case C-Rco1. Similar to case C-Fun1.
Case C-Var1. Then $\mathrm{D}^{0}=\mathrm{C} \wedge \beta$ and $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot(\beta \leqslant \neg \mathrm{C})$ for some $\beta, \mathrm{C}$, and $\Xi_{1}^{\prime}$. By S-Hyp, we have:
$$
\begin{array}{ll} 
& \rho(\beta \leqslant \neg \mathrm{C}) \vDash \rho \beta \leqslant \rho \neg \mathrm{C} \\
\text { i.e., } \quad & \rho(\beta \leqslant \neg \mathrm{C}) \vDash \rho \beta \leqslant \neg \rho \mathrm{C} \tag{61}
\end{array}
$$

By Theorem B.20, (61) implies:
$$
\begin{array}{ll} 
& \rho(\beta \leqslant \neg \mathrm{C}) \vDash \rho \mathrm{C} \wedge \rho \beta \leqslant \perp \\
\text { i.e., } & \rho(\beta \leqslant \neg \mathrm{C}) \vDash \rho(\mathrm{C} \wedge \beta) \leqslant \perp \tag{62}
\end{array}
$$

By Corollary B.41, we have:
$$
\begin{gather*}
\triangleright \Xi_{\alpha} \vdash \mathrm{C} \wedge \beta \equiv\left[\alpha \mapsto \alpha \wedge u b_{\Xi_{\alpha}}(\alpha) \vee l b_{\Xi_{\alpha}}(\alpha)\right](\mathrm{C} \wedge \beta) \\
\text { i.e., } \quad \triangleright \Xi_{\alpha} \vdash \mathrm{C} \wedge \beta \equiv \rho(\mathrm{C} \wedge \beta) \tag{63}
\end{gather*}
$$

Then by S-Trans on (63) and (62), we have:
$$
\begin{equation*}
\triangleright \Xi_{\alpha} \cdot \rho\left(\Xi_{\alpha} \cdot \Xi_{1}^{\prime} \cdot(\beta \leqslant \neg \mathrm{C}) \cdot \Sigma\right) \vdash \mathrm{C} \wedge \beta \leqslant \perp \tag{64}
\end{equation*}
$$

Case C-Var2. Similar to case C-Var1.

Lemma B. 118 (Guardedness of constraining).
(1) If $\Xi, \Sigma \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi^{\prime}$ and $\operatorname{err} \notin \Xi^{\prime}$, then $\Xi^{\prime}$ guard.
(2) If $\Xi, \Sigma \vdash \bigvee_{i} \mathrm{C}_{i}^{0} \Rightarrow \Xi^{\prime}$ and $\overline{T T V^{\prime}\left(\mathrm{C}_{i}^{0}\right) \text { are distinct }}$ and err $\notin \Xi^{\prime}$, then $\Xi^{\prime}$ guard..

Proof. By straightforward induction on constraining derivations.

\section*{B. 15 Type Inference Termination Proof}

The basic intuition is that by Theorem A.9, we know that in well-formed declarations contexts, there is only a finite number of types that can be reached by expanding all the user-defined type constructors in a given type. Therefore, the number of types that may be reached while applying constraining rules is finite, and since each traversed type is saved as part of the current subtyping hypotheses, all executions of constraining will eventually halt.

Proof 5.7 (Termination of Constraining). Let $T_{i}$ be the set of type pairs that are constrained at any recursive depth $i$ of the type constraining algorithm.

We can see from the constraining rules of Figure 8 that if we start from the constraint $\Xi \vdash \tau_{0} \leqslant \pi_{0}$, then $T_{0}=\left\{\tau_{0} \leqslant \pi_{0}\right\}$ and ${\overline{T_{i} \subseteq T_{i}^{\prime}}}^{i}$ where:
$$
\begin{aligned}
T_{0}^{\prime}= & \left\{\tau_{0} \leqslant \pi_{0}\right\} \cup \Xi \\
T_{i+1}^{\prime}= & \left.\left\{\mathrm{D}_{3} \leqslant \mathrm{D}_{1} \mid \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg S\right\urcorner\left[\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right] \in S_{i}\right\} \\
& \left.\cup\left\{\mathrm{D}_{2} \leqslant \mathrm{D}_{4} \mid \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg S\right\urcorner\left[\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right] \in S_{i}\right\} \\
& \left.\cup\left\{\mathrm{D}_{y} \leqslant \mathrm{D} \mid \mathcal{I}\left[\left\{{\overline{x: \mathrm{D}_{x}}}^{x}\right\}\right] \wedge \neg S\right\urcorner[\{y: \mathrm{D}\}] \in S_{i}, y \in\{\bar{x}\}\right\} \\
& \cup\left\{\bigvee_{\tau \in S} \tau \leqslant \neg \mathrm{C} \mid \mathrm{C} \wedge \alpha \in S_{i}, S \in \mathcal{P}\left(\left\{\pi \mid \pi \leqslant \alpha \in \bigcup_{j \leqslant i} T_{j}^{\prime}\right\}\right)\right\} \\
& \cup\left\{\alpha \leqslant \neg \mathrm{C} \mid \mathrm{C} \wedge \alpha \in S_{i}\right\} \\
& \cup\left\{\mathrm{C} \leqslant \wedge_{\tau \in S} \tau \mid \mathrm{C} \wedge \neg \alpha \in S_{i}, S \in \mathcal{P}\left(\left\{\pi \mid \alpha \leqslant \pi \in \bigcup_{j \leqslant i} T_{j}^{\prime}\right\}\right)\right\} \\
& \cup\left\{\mathrm{C} \leqslant \alpha \mid \mathrm{C} \wedge \neg \alpha \in S_{i}\right\} \\
S_{i}= & \left\{\mathrm{C} \mid(\tau \leqslant \pi) \in T_{i}^{\prime}, \operatorname{dnf}^{0}(\tau \wedge \neg \pi)=\bigvee_{i} \mathrm{C}_{i}, \mathrm{C} \in\left\{{\overline{\mathrm{C}_{i}}}_{i}^{i}\right\}\right\}
\end{aligned}
$$
$S_{i}$ puts each constraint in $T_{i}^{\prime}$ into RDNF, as is done by C-Assum. The first two components in the inductive definition of $T_{i}^{\prime}$ correspond to the premises of C-Fun1, and the third component to the premise of C-Rcd1. In addition to the pairs of types constrained (i.e., the hypotheses assumed), $T_{i}^{\prime}$ also contains the bounds assumed in the premises of C-Var1 and C-Var2, as seen in the fifth and seventh components. Therefore we can simply look up the bounds from the union of $T_{j}^{\prime}$ for $j \leqslant i$ in the fourth and sixth components, which correspond to the premise of C-Var1 and C-Var2 respectively. To exclude hypotheses assumed by C-Assum, which may not end up being assumed as a bound by C-Var1 and C-Var2, we overapproximate by considering all subsets of such pairs of types.

Next, we show that the size of $\bigcup_{i} T_{i}^{\prime}$ is bounded.
The functions ${\overline{\text { collect }_{c}}}^{c}$ traverse a type and collect the type variables, class and alias types, nominal tags, and record labels, which we abbreviate as $\bar{c}$, reachable from the type.
```
c ::= TV (type variables)
    | CA (class and alias types)
    | NT (nominal tags)
    | RL (record labels)
N[\overline{\tau}\mp@subsup{]}{}{*}::=\epsilon|N[\overline{\tau}\mp@subsup{]}{}{*}\cdotN[\overline{\tau}]
```

$$
\begin{aligned}
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{1} \rightarrow \tau_{2}\right)= & \operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{1}\right) \cup \operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{2}\right) \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\{x: \tau\})= & \begin{cases}\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\tau) \cup\{x\} & \text { ifc }=\mathrm{RL} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\tau) & \text { otherwise }\end{cases} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(N[\bar{\tau}])= & \begin{cases}\operatorname{collect}_{c}^{N[\bar{\tau}]^{*} \cdot N[\bar{\tau}]}\left(\tau^{\prime}\right) \cup\{N[\bar{\tau}]\} & \text { if } N[\bar{\tau}] \notin N[\bar{\tau}]^{*} \text { and } c=\mathrm{CA} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*} \cdot N[\bar{\tau}]}\left(\tau^{\prime}\right) & \text { if } N[\bar{\tau}] \notin N[\bar{\tau}]^{*} \text { and } c \neq \mathrm{CA} \\
\varnothing & \text { if } N[\bar{\tau}] \in N[\bar{\tau}]^{*}\end{cases} \\
& \text { where } N[\bar{\tau}] \exp . \tau^{\prime} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\# C)= & \begin{cases}\{\# C\} & \text { ifc }=\mathrm{NT} \\
\varnothing & \text { otherwise }\end{cases} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\alpha)= & \begin{cases}\{\alpha\} & \text { if } c=\mathrm{TV} \\
\varnothing & \text { otherwise }\end{cases} \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\mathrm{~T}^{\diamond}\right)= & \varnothing \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right)= & \operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{1}\right) \cup \operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}\left(\tau_{2}\right) \\
\operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\neg \tau)= & \operatorname{collect}_{c}^{N[\bar{\tau}]^{*}}(\tau)
\end{aligned}
$$

Similarly, the function depth traverses a type and measures the nesting depth of type constructors up to the first recursive occurrence of a class or alias type.
$$
\begin{aligned}
\operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{1} \rightarrow \tau_{2}\right) & =\max \left(\operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{1}\right), \operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{2}\right)\right)+1 \\
\operatorname{depth}^{N[\bar{\tau}]}(\{x: \tau\}) & =\operatorname{depth}^{N[\bar{\tau}]}(\tau)+1 \\
\operatorname{depth}^{N[\bar{\tau}]}(N[\bar{\tau}]) & = \begin{cases}\operatorname{depth}^{N[\bar{\tau}]^{*} \cdot N[\bar{\tau}]}\left(\tau^{\prime}\right) & \text { if } N[\bar{\tau}] \notin N[\bar{\tau}]^{*}, \text { where } N[\bar{\tau}] \exp . \tau^{\prime} \\
0 & \text { if } N[\bar{\tau}] \in N[\bar{\tau}]^{*}\end{cases} \\
\operatorname{depth}^{N[\bar{\tau}]}(\# C) & =\operatorname{depth}^{N[\bar{\tau}]}(\alpha)=\operatorname{depth}^{N[\bar{\tau}]}\left(\top^{\diamond}\right)=0 \\
\operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{1} \vee^{\diamond} \tau_{2}\right) & =\max \left(\operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{1}\right), \operatorname{depth}^{N[\bar{\tau}]}\left(\tau_{2}\right)\right) \\
\operatorname{depth}^{N[\bar{\tau}]}(\neg \tau) & =\operatorname{depth}^{N[\bar{\tau}]}(\tau)
\end{aligned}
$$

By the Theorem A.9, if $\mathcal{D} \boldsymbol{w} \boldsymbol{f}$, then for all $\tau$, the sets ${\overline{\operatorname{collect}_{c}(\tau)}}^{c}$ are finite, and depth $(\tau)$ is finite.
Given a set of types $S$, we can collect the $\bar{c}$ reachable from it as $\overline{\operatorname{collect}_{c}(S)=\bigcup_{\tau \in S} \operatorname{collect}_{c}(\tau)}{ }^{c}$ and the type constructor nesting depth as $\operatorname{depth}(S)=\max _{\tau \in S} \operatorname{depth}(\tau)$. Then we can inductively construct the universes $U_{i}$ of C's up to depth $i$ that only contain $\overline{\operatorname{collect}_{c}(S)}$ without duplicates, as do the results of $\mathrm{dnf}^{0}$. Notice that all of $U_{i}$ are finite.

For any $S^{\prime}$ where $\overline{\operatorname{collect}_{c}\left(S^{\prime}\right) \subseteq \operatorname{collect}_{c}(S)}{ }^{c}$, $\operatorname{depth}\left(S^{\prime}\right)$ is the type constructor nesting depth after expanding class and alias types up to the first recursive occurrences, while dnf ${ }^{0}$ expands class and alias types on the top level, which by the guardedness check does not include their first recursive occurrences. Since the RDNF subexpression unnesting in the first three components of the inductive definition of $T_{i}^{\prime}$, the Boolean algebraic connectives in the remaining four components, and $\mathrm{dnf}^{0}$ in
$S_{i}$ all preserve the depth and do not introduce new $\bar{c}$, we have:
$$
\begin{aligned}
S_{i} \subseteq U_{\text {depth }}\left(T_{0}^{\prime}\right) & \\
T_{i}^{\prime} \subseteq T_{0}^{\prime} \cup \quad & \left(\left\{\bigvee_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\} \cup \operatorname{collect}_{\mathrm{TV}}\left(T_{0}^{\prime}\right)\right) \\
& \times\left(\left\{\bigvee_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\} \cup\left\{\bigwedge_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\}\right. \\
& \left.\cup\left\{\tau \mid \tau \in U_{\text {depth }\left(T_{0}^{\prime}\right)}\right\} \cup \operatorname{collect}_{\mathrm{TV}}\left(T_{0}^{\prime}\right)\right)
\end{aligned}
$$

Therefore the set $T=\bigcup_{i} T_{i}$ of all pairs of types ever constrained by the algorithm is bounded by:
$$
\begin{aligned}
T \subseteq \bigcup_{i} T_{i}^{\prime} \subseteq T_{0}^{\prime} \cup & \left(\left\{\bigvee_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\} \cup \operatorname{collect}_{\mathrm{TV}}\left(T_{0}^{\prime}\right)\right) \\
\times & \left(\left\{\bigvee_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\} \cup\left\{\bigwedge_{\tau \in S} \tau \mid S \in \mathcal{P}\left(U_{\text {depth }\left(T_{0}^{\prime}\right)}\right)\right\}\right. \\
& \left.\cup\left\{\tau \mid \tau \in U_{\text {depth }\left(T_{0}^{\prime}\right)}\right\} \cup \operatorname{collect}_{\mathrm{TV}}\left(T_{0}^{\prime}\right)\right)
\end{aligned}
$$
and is thus finite.
Since C-Hyp ensures that the subtyping context $\Sigma$ reachable by the subtyping algorithm cannot contain duplicates, we have $\Sigma \subseteq T \cup\{\boldsymbol{e r r}\}$. Since $T$ is finite, $\Sigma$ is also finite. Since recursive calls to the constraining algorithm always increases the size of $\Sigma$, this implies that constraining always terminates.

\section*{B. 16 Type Inference Completeness Proofs}

Lemma B. 119 (Completeness of type inference - general). If $\Xi, \Gamma \vdash^{\star} P: \tau$, then $\Gamma \Vdash^{\star} P$ : $\tau^{\prime} \Rightarrow \Xi^{\prime}$ for some $\Xi^{\prime}$ and $\tau^{\prime}$ so that $\forall \Xi^{\prime} . \tau^{\prime} \leqslant^{\forall} \forall \Xi$. $\tau$.

Proof. By induction on program typing derivations.
Case T-Body. Then $P=t$ for some $t$. The premises of the rule are:
$$
\begin{gather*}
\Xi \text { cons. }  \tag{1}\\
\Xi, \Gamma \vdash t: \tau \tag{2}
\end{gather*}
$$

By Lemma B. 123 on (2) and (1), we have:
$$
\begin{gather*}
\Gamma \Vdash t: \tau^{\prime} \Rightarrow \Xi^{\prime}  \tag{3}\\
\Xi \vdash \rho \tau^{\prime} \leqslant \tau  \tag{4}\\
\Xi \models \rho \Xi^{\prime} \tag{5}
\end{gather*}
$$
for some $\tau^{\prime}$ and $\Xi^{\prime}$ and $\rho$, where $\operatorname{dom}(\rho)=\operatorname{fresh}((3))$. By I-Body on (3), we have:
$$
\begin{equation*}
\Gamma \Vdash^{\star} t: \tau^{\prime} \Rightarrow \Xi^{\prime} \tag{6}
\end{equation*}
$$

By S-All on (4) and (5), we have:
$$
\begin{equation*}
\forall \Xi^{\prime} . \tau^{\prime} \leqslant \leqslant^{\forall} \forall \Xi . \tau \tag{7}
\end{equation*}
$$

Case T-Def. Then $P=\boldsymbol{\operatorname { d e f }} x=t ; P^{\prime}$ for some $x$ and $t$ and $P^{\prime}$. The premises of the rule are:
$$
\begin{gather*}
\Xi_{1} \text { cons. }  \tag{8}\\
\Xi_{1}, \Gamma \vdash t: \tau_{1}  \tag{9}\\
\Xi, \Gamma \cdot\left(x: \forall \Xi_{1} \cdot \tau_{1}\right) \vdash^{\star} P^{\prime}: \tau \tag{10}
\end{gather*}
$$

By Lemma B. 123 on (9) and (8), we have:
$$
\begin{gather*}
\Gamma \Vdash t: \tau_{1}^{\prime} \Rightarrow \Xi_{1}^{\prime}  \tag{11}\\
\Xi_{1} \vdash \rho_{1} \tau_{1}^{\prime} \leqslant \tau_{1}  \tag{12}\\
\Xi_{1} \models \rho_{1} \Xi_{1}^{\prime} \tag{13}
\end{gather*}
$$
for some $\tau_{1}^{\prime}$ and $\Xi_{1}^{\prime}$ and $\rho_{1}$, where $\operatorname{dom}\left(\rho_{1}\right)=\operatorname{fresh}((11))$. By S-All on (12) and (13), we have:
$$
\begin{equation*}
\forall \Xi_{1}^{\prime} \cdot \tau_{1}^{\prime} \leqslant{ }^{\forall} \forall \Xi_{1} \cdot \tau_{1} \tag{14}
\end{equation*}
$$

By Lemma B. 120 on (10) and (14), we have:
$$
\begin{equation*}
\Xi, \Gamma \cdot\left(x: \forall \Xi_{1}^{\prime} \cdot \tau_{1}^{\prime}\right) \vdash^{\star} P^{\prime}: \tau \tag{15}
\end{equation*}
$$

By IH on (15), we have:
$$
\begin{gather*}
\Gamma \cdot\left(x: \forall \Xi_{1}^{\prime} \cdot \tau_{1}^{\prime}\right) \Vdash^{\star} P^{\prime}: \tau^{\prime} \Rightarrow \Xi^{\prime}  \tag{16}\\
\forall \Xi^{\prime} \cdot \tau^{\prime} \leqslant{ }^{\forall} \forall \Xi \cdot \tau \tag{17}
\end{gather*}
$$
for some $\tau^{\prime}$ and $\Xi^{\prime}$. By I-Body on (11) and (16), we have:
$$
\begin{equation*}
\Gamma \Vdash^{\star} \operatorname{def} x=t ; P^{\prime}: \tau^{\prime} \Rightarrow \Xi^{\prime} \tag{18}
\end{equation*}
$$

Lemma B. 120 (Strengthening). If $\Xi, \Gamma \cdot\left(x: \sigma_{1}\right) \vdash t: \tau$ and $\epsilon \vdash \sigma_{2} \leqslant^{\forall} \sigma_{1}$, then $\Xi, \Gamma \cdot\left(x: \sigma_{2}\right) \vdash t: \tau$.

Proof. By straightforward induction on typing derivations.
Definition B.121. We write $\operatorname{fresh}(A)$ to denote all the type variables that are taken as fresh in the given derivation $A$.

Definition B.122. We say $\rho$ extends $\rho^{\prime}$ if $\left[\overline{\alpha \longmapsto \tau}(\alpha \mapsto \tau) \in \rho, \alpha \in \operatorname{dom}\left(\rho^{\prime}\right)\right]=\rho^{\prime}$.
Lemma B. 123 (Completeness of polymorphic type inference). If $\Xi, \Gamma \vdash t: \tau$ and $\Xi$ cons. and $\Xi \vDash \rho_{0} \Xi_{0}$, then (A) $\Xi_{0}, \Gamma \Vdash t: \tau^{\prime} \Rightarrow \Xi^{\prime}$ and $\Xi \vdash \rho \tau^{\prime} \leqslant \tau$ and $\Xi \vDash \rho\left(\Xi_{0} \cdot \Xi^{\prime}\right)$ for some $\tau^{\prime}$ and $\Xi^{\prime}$ and $\rho$, where $\operatorname{err} \notin \Xi^{\prime}$ and $\rho$ extends $\rho_{0}$ and $\operatorname{dom}(\rho) \backslash \operatorname{dom}\left(\rho_{0}\right)=\operatorname{fresh}(A)$.

Proof. By induction on term typing derivations.
Case T-Subs. Then the premises of the rule are:
$$
\begin{align*}
& \Xi, \Gamma \vdash t: \tau_{1}  \tag{1}\\
& \Xi \vdash \tau_{1} \leqslant \tau \tag{2}
\end{align*}
$$
for some $\tau_{1}$. By IH on (1), we have:
$$
\begin{gather*}
\Xi_{0}, \Gamma \Vdash t: \tau^{\prime} \Rightarrow \Xi^{\prime}  \tag{3}\\
\Xi \vdash \rho \tau^{\prime} \leqslant \tau_{1}  \tag{4}\\
\Xi \models \rho\left(\Xi_{0} \cdot \Xi^{\prime}\right) \tag{5}
\end{gather*}
$$
for some $\tau^{\prime}$ and $\Xi^{\prime}$ and $\rho$, where $\operatorname{err} \notin \Xi^{\prime}$ and $\rho$ extends $\rho_{0}$ and $\operatorname{dom}(\rho) \backslash \operatorname{dom}\left(\rho_{0}\right)=\operatorname{fresh}((3))$. By S-Trans on (4) and (2), we have:
$$
\begin{equation*}
\Xi \vdash \rho \tau^{\prime} \leqslant \tau \tag{6}
\end{equation*}
$$

Case T-Obj. Then $t=C\left\{{\overline{x_{i}=t_{i}}}^{i}\right\}$ and $\tau=\# C \wedge\left\{{\overline{x_{i}: \tau_{i}}}^{i}\right\}$ for some $C$ and ${\overline{x_{i}}}^{i}$ and $\bar{t}_{i}^{i}$ and ${\overline{\tau_{i}}}^{i}$. The premises of the rule are:
$$
\begin{gather*}
\overline{\Xi, \Gamma \vdash t_{i}: \tau_{i}}  \tag{7}\\
C \text { final } \tag{8}
\end{gather*}
$$

Then for each $i$, repeat the following:
Assume the following:
$$
\begin{gather*}
\Xi \vDash \rho_{i-1}\left(\bar{\Xi}_{j}^{j \in 0 . . i-1}\right)  \tag{9}\\
{\frac{\Xi \vdash \rho_{i-1} \tau_{j}^{\prime} \leqslant \tau_{j}}{j \in 1 . . i-1}} \tag{10}
\end{gather*}
$$

By IH on (7), we have:
$$
\begin{gather*}
\bar{\Xi}_{j}^{j \in 0 . . i-1}, \Gamma \Vdash t_{i}: \tau_{i}^{\prime} \Rightarrow \Xi_{i}  \tag{11}\\
\Xi \vdash \rho_{i} \tau_{i}^{\prime} \leqslant \tau_{i}  \tag{12}\\
\Xi \models \rho_{i}\left(\bar{\Xi}_{j}^{j \in 0 . . i-1} \cdot \Xi_{i}\right) \tag{13}
\end{gather*}
$$
for some $\tau_{i}^{\prime}$ and $\Xi_{i}$ and $\rho_{i}$, where $\boldsymbol{\operatorname { e r r }} \notin \Xi_{i}$ and $\rho_{i}$ extends $\rho_{i-1}$ and $\operatorname{dom}\left(\rho_{i}\right) \backslash \operatorname{dom}\left(\rho_{i-1}\right)=$ fresh((11)). Since $\rho_{i}$ extends $\rho_{i-1}$ and $\operatorname{dom}\left(\rho_{i}\right) \backslash \operatorname{dom}\left(\rho_{i-1}\right)$ are picked to be fresh in (11), which means they could not have appeared in $\bar{\tau}_{j}^{\prime j \in 1 . . i-1}$, we have:
$$
\begin{equation*}
{\overline{\rho_{i} \tau_{j}^{\prime}=\rho_{i-1} \tau_{j}^{\prime}}}^{j \in 1 . . i-1} \tag{14}
\end{equation*}
$$

Then (10) implies:
$$
\begin{equation*}
{\overline{\Xi \vdash \rho_{i} \tau_{j}^{\prime} \leqslant \tau_{j}}}^{j \in 1 . . i-1} \tag{15}
\end{equation*}
$$

Then in the end we have:
$$
\begin{gather*}
{\overline{\bar{\Xi}_{j}^{j \in 0 . . i-1}}}^{j \vdash t_{i}: \tau_{i}^{\prime} \Rightarrow \Xi_{i}}  \tag{16}\\
{\overline{\Xi \vdash \rho\left(\Xi_{0} \cdot \bar{\Xi}_{i}^{i}\right)}}^{i} \tag{17}
\end{gather*}
$$
for some $\bar{\tau}_{i}^{i}$ and $\bar{\Xi}_{i}^{i}$ and $\rho$, where $\operatorname{err} \notin \bar{\Xi}_{i}^{i}$ and $\rho$ extends $\rho_{0}$ and $\operatorname{dom}(\rho) \backslash \operatorname{dom}\left(\rho_{0}\right)= \bigcup_{i}\left(\operatorname{dom}\left(\rho_{i}\right) \backslash \operatorname{dom}\left(\rho_{i-1}\right)\right)=\bigcup_{i} \operatorname{fresh}\left((16)_{i}\right)$. By I-ObJ on (16) and (8), we have:
$$
\begin{equation*}
\Xi_{0}, \Gamma \Vdash C\left\{{\overline{x_{i}=t_{i}}}^{i}\right\}: \# C \wedge\left\{{\overline{x_{i}: \tau_{i}^{\prime}}}^{i}\right\} \Rightarrow{\overline{\Xi_{i}}}^{i} \tag{19}
\end{equation*}
$$

By S-RcoDepth on (18), we have:
$$
\begin{equation*}
{\overline{\Xi \vdash\left\{x_{i}: \rho \tau_{i}^{\prime}\right\} \leqslant\left\{x_{i}: \tau_{i}\right\}}}^{i} \tag{20}
\end{equation*}
$$

By Lemma B.22> on S-Refl and (20), we have:
$$
\begin{array}{r}
\Xi \vdash \# C \wedge\left\{{\overline{x_{i}: \rho \tau_{i}^{\prime}}}^{i}\right\} \leqslant \# C \wedge\left\{\overline{x_{i}: \tau_{i}}\right\} \\
\text { i.e., } \quad \Xi \vdash \rho\left(\# C \wedge\left\{{\overline{x_{i}: \tau_{i}^{\prime}}}^{i}\right\}\right) \leqslant \# C \wedge\left\{\overline{x_{i}: \tau_{i}}\right\} \tag{21}
\end{array}
$$

Case T-Proj. Then $t=t^{\prime} . x$ for some $t^{\prime}$ and $x$. The premise of the rule is:
$$
\begin{equation*}
\Xi, \Gamma \vdash t^{\prime}:\{x: \tau\} \tag{22}
\end{equation*}
$$

By IH on (22), we have:
$$
\begin{gather*}
\Xi_{0}, \Gamma \Vdash t^{\prime}: \tau^{\prime} \Rightarrow \Xi_{1}  \tag{23}\\
\Xi \vdash \rho_{1} \tau^{\prime} \leqslant\{x: \tau\}  \tag{24}\\
\Xi \models \rho_{1}\left(\Xi_{0} \cdot \Xi_{1}\right) \tag{25}
\end{gather*}
$$
for some $\tau^{\prime}$ and $\Xi_{1}$ and $\rho_{1}$, where $\operatorname{err} \notin \Xi_{1}$ and $\rho_{1}$ extends $\rho_{0}$ and $\operatorname{dom}\left(\rho_{1}\right) \backslash \operatorname{dom}\left(\rho_{0}\right)=$ fresh $((23))$. Introduce a fresh $\alpha$ and let $\rho=\left[\alpha \mapsto \tau, \overline{\beta \mapsto \pi}^{(\beta \mapsto \pi) \in \rho_{1}}\right]$. Then we have:
$$
\begin{gather*}
\rho \tau^{\prime}=\rho_{1} \tau^{\prime}  \tag{26}\\
\rho(\{x: \alpha\})=\{x: \tau\}  \tag{27}\\
\rho\left(\Xi_{0} \cdot \Xi_{1}\right)=\rho_{1}\left(\Xi_{0} \cdot \Xi_{1}\right) \tag{28}
\end{gather*}
$$

Then (24) and (25) imply:
$$
\begin{gather*}
\Xi \vdash \rho \tau^{\prime} \leqslant \rho(\{x: \alpha\})  \tag{29}\\
\Xi \models \rho\left(\Xi_{0} \cdot \Xi_{1}\right) \tag{30}
\end{gather*}
$$

By Lemma 5.9 on (29) and (30), we have:
$$
\begin{equation*}
\Xi_{0} \cdot \Xi_{1}, \epsilon \vdash \tau^{\prime} \ll\{x: \alpha\} \Rightarrow \Xi_{2} \tag{31}
\end{equation*}
$$
for some $\Xi_{2}$, where $\boldsymbol{\operatorname { e r r }} \notin \Xi_{2}$ and $\Xi \vDash \rho \Xi_{2}$. Then by I-ProJ on (23) and (31), we have:
$$
\begin{equation*}
\Xi_{0}, \Gamma \Vdash t^{\prime} . x: \alpha \Rightarrow \Xi_{1} \cdot \Xi_{2} \tag{32}
\end{equation*}
$$

Since $\rho \alpha=\tau$, by S-Refl, we have:
$$
\begin{equation*}
\Xi \vdash \rho \alpha \leqslant \tau \tag{33}
\end{equation*}
$$
(30) and $\Xi \models \rho \Xi_{2}$ implies:
$$
\begin{equation*}
\Xi \vDash \rho\left(\Xi_{0} \cdot \Xi_{1} \cdot \Xi_{2}\right) \tag{34}
\end{equation*}
$$

Case T-Var1. Immediate by I-Var1.
Case T-Var2. Then $t=x$ and $\Gamma(x)=\forall \Xi_{1}$. $\tau_{1}$ for some $x$ and $\Xi_{1}$ and $\tau_{1}$. By the definition of $\leqslant^{\forall}$, we have:
$$
\begin{gather*}
\Xi \models \rho_{1} \Xi_{1}  \tag{35}\\
\Xi \vdash \rho_{1} \tau_{1} \leqslant \tau \tag{36}
\end{gather*}
$$
for some $\rho_{1}$, where $\operatorname{dom}\left(\rho_{1}\right)=T V\left(\Xi_{1}\right) \cup T V\left(\tau_{1}\right)=: S$. Introduce a fresh $\gamma_{\alpha}$ for each $\alpha \in S$. Then by I-Var2, we have:
$$
\begin{equation*}
\Xi_{0}, \Gamma \Vdash x:\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \tau_{1} \Rightarrow\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \Xi_{1} \tag{37}
\end{equation*}
$$

Let $\rho=\left[{\overline{\gamma \alpha} \rho_{1} \alpha}^{\alpha \in S}\right]$. Then we have:
$$
\begin{align*}
& \rho \circ\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \\
= & \rho_{1} \circ\left[\overline{\gamma \alpha}^{\alpha \in \alpha}\right.  \tag{38}\\
= & \rho_{1} \circ\left[\overline{\gamma \alpha}^{\alpha}\right] \circ\left[\bar{\alpha}^{\alpha \in S}\right]
\end{align*}
$$

Since $\overline{\gamma \alpha}^{\alpha} \in S$ are picked to be fresh, which means they could not have appeared in $\Xi_{1}$ and $\tau_{1}$, we have:
$$
\begin{align*}
{\left[\overline{\gamma \alpha}^{\alpha}{ }^{\alpha \in S}\right] \Xi_{1} } & =\Xi_{1}  \tag{39}\\
{\left[\overline{\gamma \alpha}^{\alpha}{ }^{\alpha \in S}\right] \tau_{1} } & =\tau_{1} \tag{40}
\end{align*}
$$

Then we have:
$$
\begin{align*}
\rho_{1} \Xi_{1} & =\rho_{1}\left(\left[\overline{\gamma \alpha}^{\alpha} \alpha \in S\right] \Xi_{1}\right) \\
& =\rho\left(\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \Xi_{1}\right)  \tag{41}\\
\rho_{1} \tau_{1} & =\rho_{1}\left(\left[\overline{\gamma \alpha}^{\alpha \in S}\right] \tau_{1}\right) \\
& =\rho\left(\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \tau_{1}\right) \tag{42}
\end{align*}
$$

Then (35) and (36) imply:
$$
\begin{gather*}
\Xi \vDash \rho\left(\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \Xi_{1}\right)  \tag{43}\\
\Xi \vdash \rho\left(\left[\overline{\alpha \longmapsto \gamma \alpha}^{\alpha \in S}\right] \tau_{1}\right) \leqslant \tau \tag{44}
\end{gather*}
$$

Case T-Abs. Then $t=\lambda$ x. $t^{\prime}$ and $\tau=\tau_{1} \rightarrow \tau_{2}$ for some $x$ and $t^{\prime}$ and $\tau_{1}$ and $\tau_{2}$. The premise of the rule is:
$$
\begin{equation*}
\Xi, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t^{\prime}: \tau_{2} \tag{45}
\end{equation*}
$$

Introduce a fresh $\alpha$. By Lemma B. 124 on (45), we have:
$$
\begin{gather*}
\Xi \cdot\left(\alpha \leqslant \tau_{1}\right), \Gamma \cdot(x: \alpha) \vdash t^{\prime}: \tau^{\prime}  \tag{46}\\
\Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime} \leqslant \tau_{2} \tag{47}
\end{gather*}
$$

By IH on (46), we have:
$$
\begin{gather*}
\Xi_{0}, \Gamma \cdot(x: \alpha) \Vdash t^{\prime}: \tau^{\prime \prime} \Rightarrow \Xi^{\prime}  \tag{48}\\
\Xi \cdot\left(\alpha \leqslant \tau_{1}\right) \vdash \rho_{1} \tau^{\prime \prime} \leqslant \tau^{\prime}  \tag{49}\\
\Xi \cdot\left(\alpha \leqslant \tau_{1}\right) \models \rho_{1}\left(\Xi_{0} \cdot \Xi^{\prime}\right) \tag{50}
\end{gather*}
$$
for some $\tau^{\prime \prime}$ and $\Xi^{\prime}$ and $\rho_{1}$, where $\operatorname{err} \notin \Xi^{\prime}$ and $\rho_{1}$ extends $\rho_{0}$ and $\operatorname{dom}\left(\rho_{1}\right) \backslash \operatorname{dom}\left(\rho_{0}\right)=$ fresh((48)). By I-Abs on (48), we have:
$$
\begin{equation*}
\Xi_{0}, \Gamma \Vdash \lambda x \cdot t^{\prime}: \alpha \rightarrow \tau^{\prime \prime} \Rightarrow \Xi^{\prime} \tag{51}
\end{equation*}
$$

By Lemma B.36, (49) and (50) imply:
$$
\begin{array}{cc} 
& {\left[\alpha \mapsto \tau_{1}\right]\left(\Xi \cdot\left(\alpha \leqslant \tau_{1}\right)\right) \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime}} \\
\text { i.e., } \quad\left[\alpha \mapsto \tau_{1}\right] \Xi \cdot\left(\tau_{1} \leqslant \tau_{1}\right) \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime} \\
& {\left[\alpha \mapsto \tau_{1}\right]\left(\Xi \cdot\left(\alpha \leqslant \tau_{1}\right)\right) \models\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1}\left(\Xi_{0} \cdot \Xi^{\prime}\right)} \\
\text { i.e., } \quad\left[\alpha \mapsto \tau_{1}\right] \Xi \cdot\left(\tau_{1} \leqslant \tau_{1}\right) \models\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1}\left(\Xi_{0} \cdot \Xi^{\prime}\right) \tag{53}
\end{array}
$$

By S-Cons on Lemma B. 25 and S-Refl, we have:
$$
\begin{equation*}
\left[\alpha \mapsto \tau_{1}\right] \Xi \models\left[\alpha \mapsto \tau_{1}\right] \Xi \cdot\left(\tau_{1} \leqslant \tau_{1}\right) \tag{54}
\end{equation*}
$$

By Lemma B. 30 with (53), (51) and (52) imply:
$$
\begin{gather*}
{\left[\alpha \mapsto \tau_{1}\right] \Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime}}  \tag{55}\\
{\left[\alpha \mapsto \tau_{1}\right] \Xi \models\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1}\left(\Xi_{0} \cdot \Xi^{\prime}\right)} \tag{56}
\end{gather*}
$$

Since $\alpha$ is picked to be fresh, which means it could not have appeared in $\Xi$, we have $[\alpha \mapsto \left.\tau_{1}\right] \Xi=\Xi$. Then (54) and (55) imply:
$$
\begin{gather*}
\Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime}  \tag{57}\\
\Xi \models\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1}\left(\Xi_{0} \cdot \Xi^{\prime}\right) \tag{58}
\end{gather*}
$$

By S-Trans on (57) and (47), we have:
$$
\begin{equation*}
\Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant \tau_{2} \tag{59}
\end{equation*}
$$

By S-FunDepth on S-Refl and (60), we have:
$$
\begin{align*}
& \Xi \vdash \tau_{1} \rightarrow\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1} \tau^{\prime \prime} \leqslant \tau_{1} \rightarrow \tau_{2} \\
& \text { i.e., } \quad \Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \circ \rho_{1}\left(\alpha \rightarrow \tau^{\prime \prime}\right) \leqslant \tau_{1} \rightarrow \tau_{2} \tag{60}
\end{align*}
$$

Cases T-App, T-Asc, T-Case1, T-Case2, T-Case3. Similar to case T-Proj.

Lemma B.124. If $\Xi, \Gamma \cdot\left(x: \tau_{1}\right) \vdash t: \tau$, then $\Xi \cdot\left(\alpha \leqslant \tau_{1}\right), \Gamma \cdot(x: \alpha) \vdash t: \tau^{\prime}$ and $\Xi \vdash\left[\alpha \mapsto \tau_{1}\right] \tau^{\prime} \leqslant \tau$ for any $\alpha$ fresh and some $\tau^{\prime}$.

Proof. By straightforward induction on typing derivations.
Proof 5.9 (Completeness of Constraining). By Theorem 5.7, we have:
$$
\begin{equation*}
\Xi_{0}, \epsilon \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi^{\prime} \tag{1}
\end{equation*}
$$
for some $\Xi^{\prime}$. The result then follows from Lemma B.125.
Lemma B. 125 (Necessity of Constraining).
(1) If $\Xi \vdash \rho \tau_{1} \leqslant \rho \tau_{2}$ and $\Xi$ cons. and $\Xi \models \rho \Xi_{0}$ and $\Xi_{0}, \Sigma \vdash \tau_{1} \ll \tau_{2} \Rightarrow \Xi^{\prime}$, then $\Xi \models \rho \Xi^{\prime}$.
(2) If $\Xi \vdash \rho \mathrm{D}^{0} \leqslant \perp$ and $\Xi$ cons. and $\Xi \vDash \rho \Xi_{0}$ and $\Xi_{0}, \Sigma \vdash \mathrm{D}^{0} \Rightarrow \Xi^{\prime}$, then $\Xi \vDash \rho \Xi^{\prime}$.

Proof. By induction on constraining derivations.
Cases C-Hyp, C-Bot, C-Cls1. Immediate by S-Empty since $\Xi^{\prime}=\epsilon$.
Case C-Assum. From the assumptions, we have:
$$
\begin{equation*}
\Xi \vdash \rho \tau_{1} \leqslant \rho \tau_{2} \tag{1}
\end{equation*}
$$

The premise of the rule is:
$$
\begin{equation*}
\Xi_{0}, \Sigma \cdot \triangleright\left(\tau_{1} \leqslant \tau_{2}\right) \vdash \mathrm{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \Rightarrow \Xi^{\prime} \tag{2}
\end{equation*}
$$

By Theorem B.20, (1) implies:
$$
\begin{align*}
& \Xi \vdash \rho \tau_{1} \wedge \neg \rho \tau_{2} \leqslant \perp \\
\text { i.e., } & \Xi \vdash \rho\left(\tau_{1} \wedge \neg \tau_{2}\right) \leqslant \perp \tag{3}
\end{align*}
$$

By Lemma 5.3, we have:
$$
\begin{equation*}
\tau_{1} \wedge \neg \tau_{2} \equiv \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \tag{4}
\end{equation*}
$$

By Lemma B.36, (4) implies:
$$
\begin{equation*}
\rho\left(\tau_{1} \wedge \neg \tau_{2}\right) \equiv \rho \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \tag{5}
\end{equation*}
$$

By S-Trans on (5) and (3), we have:
$$
\begin{equation*}
\Xi \vdash \rho \operatorname{dnf}^{0}\left(\tau_{1} \wedge \neg \tau_{2}\right) \leqslant \perp \tag{6}
\end{equation*}
$$

The result then follows from IH on (2) and (6).

Case C-Or. Then $\mathrm{D}^{0}=\mathrm{D}_{1}^{0} \vee \mathrm{C}^{0}$ for some $\mathrm{D}_{1}^{0}$ and $\mathrm{C}^{0}$. From the assumptions, we have:
$$
\begin{gather*}
\Xi-\rho\left(\mathrm{D}_{1}^{0} \vee \mathrm{C}^{0}\right) \leqslant \perp  \tag{7}\\
\Xi \models \rho \Xi_{0} \tag{8}
\end{gather*}
$$

The premises of the rule are:
$$
\begin{gather*}
\Xi_{0}, \Sigma \vdash \mathrm{D}_{1}^{0} \Rightarrow \Xi_{1}^{\prime}  \tag{9}\\
\Xi_{0} \cdot \Xi_{1}^{\prime}, \Sigma \vdash \mathrm{C}^{0} \Rightarrow \Xi_{2}^{\prime} \tag{10}
\end{gather*}
$$
for some $\Xi_{1}^{\prime}$ and $\Xi_{2}^{\prime}$, where $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}$. By S-AndOr11• and S-AndOr12• respectively, we have:
$$
\begin{array}{ll} 
& \rho \mathrm{D}_{1}^{0} \leqslant \rho \mathrm{D}_{1}^{0} \vee \rho \mathrm{C}^{0} \\
\text { i.e., } & \rho \mathrm{D}_{1}^{0} \leqslant \rho\left(\mathrm{D}_{1}^{0} \vee \mathrm{C}^{0}\right) \\
& \rho \mathrm{C}^{0} \leqslant \rho \mathrm{D}_{1}^{0} \vee \rho \mathrm{C}^{0} \\
\text { i.e., } & \rho \mathrm{C}^{0} \leqslant \rho\left(\mathrm{D}_{1}^{0} \vee \mathrm{C}^{0}\right) \tag{12}
\end{array}
$$

By S-Trans with (7), (11) and (12) respectively imply:
$$
\begin{align*}
& \Xi \vdash \rho \mathrm{D}_{1}^{0} \leqslant \perp  \tag{13}\\
& \Xi \vdash \rho \mathrm{C}^{0} \leqslant \perp \tag{14}
\end{align*}
$$

By IH on (13) and (8) and (9), we have:
$$
\begin{equation*}
\Xi \models \rho \Xi_{1}^{\prime} \tag{15}
\end{equation*}
$$
(8) and (15) imply:
$$
\begin{array}{ll} 
& \Xi \models \rho \Xi_{0} \cdot \rho \Xi_{1}^{\prime} \\
\text { i.e., } & \Xi \models \rho\left(\Xi_{0} \cdot \Xi_{1}^{\prime}\right) \tag{16}
\end{array}
$$

By IH on (14) and (16) and (10), we have:
$$
\begin{equation*}
\Xi \models \rho \Xi_{2}^{\prime} \tag{17}
\end{equation*}
$$
(15) and (17) imply:
$$
\begin{align*}
& \quad \Xi \models \rho \Xi_{1}^{\prime} \cdot \rho \Xi_{2}^{\prime} \\
& \text { i.e., } \quad \Xi \models \rho \Xi^{\prime} \tag{18}
\end{align*}
$$

Case C-NotBot. Then $\mathrm{D}^{0}=\mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R} \wedge \neg \perp$ for some $\mathcal{N}$ and $\mathcal{F}$ and $\mathcal{R}$. From the assumptions, we have:
$$
\begin{gather*}
\Xi \vdash \rho(\mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R} \wedge \neg \perp) \leqslant \perp \\
\text { i.e., } \quad \Xi \vdash \mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \wedge \neg \perp \leqslant \perp  \tag{19}\\
\Xi \text { cons. } \tag{20}
\end{gather*}
$$

By S-Trans on S-ToB• and Theorem B.11, we have:
$$
\begin{equation*}
\mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \leqslant \neg \perp \tag{21}
\end{equation*}
$$

By S-AndOr2D on S-Refl and (21), we have:
$$
\begin{equation*}
\mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \leqslant \mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \wedge \neg \perp \tag{22}
\end{equation*}
$$

By S-Trans on (22) and (19), we have:
$$
\begin{equation*}
\Xi \vdash \mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \leqslant \perp \tag{23}
\end{equation*}
$$

Since $T T V(\mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R}) \cup T T V(\perp)=\varnothing$, by Lemma B. 49 on (20) and (23), we have:
$$
\begin{equation*}
\triangleright \Xi \vdash \mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R} \leqslant \perp \tag{24}
\end{equation*}
$$

Notice that $\mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R}$ is in CDN-normalized form. Since none of $\{\mathcal{N}, \rho \mathcal{F}, \rho \mathcal{R}\}$ is a negation, $\mathcal{N} \wedge \rho \mathcal{F} \wedge \rho \mathcal{R}$ is complement-free. Then by Lemma B. 89 on (24), we have:
$$
\begin{equation*}
\perp \cong \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{25}
\end{equation*}
$$
for some ${\overline{\pi_{j}^{\prime}}}^{j}$ and ${\overline{D_{j}}}^{j}$ and ${\overline{V_{j}^{D_{j}}}}^{j}$, where $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. By S-AndOr12•, we have:
$$
\begin{equation*}
{\overline{V_{j}^{D_{j}}} \subseteq \pi_{j}^{\prime} \vee V_{j}^{D_{j}}}^{j} \tag{26}
\end{equation*}
$$

By Lemma B.22. on (26), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \bigwedge_{j}\left(\pi_{j}^{\prime} \vee V_{j}^{D_{j}}\right) \tag{27}
\end{equation*}
$$

By S-Trans on (27) and (25), we have:
$$
\begin{equation*}
\bigwedge_{j} V_{j}^{D_{j}} \subseteq \perp \tag{28}
\end{equation*}
$$
which is impossible since $\bigwedge_{j} V_{j}^{D_{j}}$ is complement-free. Therefore this case is impossible.
Case C-Cls2. Then $\mathrm{D}^{0}=\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right)$ for some $C_{1}$ and $C_{2}$ and $\mathcal{I}\left[\# C_{1}\right]$ and U . From the assumptions, we have:
$$
\begin{gather*}
\Xi \vdash \rho\left(\mathcal{I}\left[\# C_{1}\right] \wedge \neg\left(\mathrm{U} \vee \# C_{2}\right)\right) \leqslant \perp  \tag{29}\\
\Xi \text { cons. } \tag{30}
\end{gather*}
$$

The premises of the rule are:
$$
\begin{gather*}
C_{2} \notin \mathcal{S}\left(\# C_{1}\right)  \tag{31}\\
\Xi_{0}, \Sigma \vdash \mathcal{I}\left[\# C_{1}\right] \wedge \neg \mathrm{U} \Rightarrow \Xi^{\prime} \tag{32}
\end{gather*}
$$

By Theorem B. 20 on (29), we have:
$$
\begin{gather*}
\Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \leqslant \rho\left(\mathrm{U} \vee \# C_{2}\right) \\
\text { i.e., } \quad \Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \leqslant \rho \tau^{0} \vee \bigvee_{j} \# C_{j}^{\prime} \vee \# C_{2} \tag{33}
\end{gather*}
$$
for some $\tau^{0} \in\left\{\perp, \mathrm{D}_{1} \rightarrow \mathrm{D}_{2},\left\{y: \mathrm{D}_{1}\right\}\right\}$ and ${\overline{C_{j}^{\prime}}}_{j}^{j}$, where $\mathrm{U}=\rho \tau^{0} \vee \bigvee_{j} \# C_{j}^{\prime}$. Since $\operatorname{TTV}\left(\rho \mathcal{I}\left[\# C_{1}\right]\right) \cup T T V\left(\rho \tau^{0} \vee \bigvee_{j} \# C_{j}^{\prime} \vee \# C_{2}\right)=\varnothing$, by Lemma B. 49 on (30) and (33), we have:
$$
\begin{equation*}
\triangleright \Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \leqslant \rho \tau^{0} \vee \bigvee_{j} \# C_{j}^{\prime} \vee \# C_{2} \tag{34}
\end{equation*}
$$

By Lemma B. 89 on (34), we have:
$$
\begin{gather*}
\rho \mathcal{I}\left[\# C_{1}\right] \cong \bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right)  \tag{35}\\
\stackrel{\triangleright \Xi \vdash X_{i}^{C_{i}} \leq Y_{i}}{i} \tag{36}
\end{gather*}
$$
for some ${\overline{\tau_{i}^{\prime}}}^{i}$ and ${\overline{C_{i}}}^{i}$ and ${\overline{X_{i}^{C_{i}}}}^{i}$ and $\overline{Y_{i} \in\left\{\rho \tau^{0}, \# C_{2},{\overline{\# C_{j}^{\prime}}}^{j}\right\}}{ }^{i}$, where $\bigvee_{i} X_{i}^{C_{i}}$ is complement-free. By S-AndOr12>, we have:
$$
\begin{equation*}
{\overline{\tau_{i}^{\prime} \wedge X_{i}^{C_{i}} \subseteq X_{i}^{C_{i}}}}^{i} \tag{37}
\end{equation*}
$$

By Lemma B.22• on (37), we have:
$$
\begin{equation*}
\bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right) \subseteq \bigvee_{i} X_{i}^{C_{i}} \tag{38}
\end{equation*}
$$

By S-Trans on (35) and (38), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\# C_{1}\right] \cong \bigvee_{i} X_{i}^{C_{i}} \tag{39}
\end{equation*}
$$

By Corollary B.86, (39) implies:
$$
\begin{equation*}
\rho \mathcal{I}\left[\# C_{1}\right] \cong X_{k}^{C_{k}} \tag{40}
\end{equation*}
$$
for some $k$.
Case $C_{k} \in\{\perp, \not X\}$. Then we have:
$$
\begin{equation*}
X_{k}^{C_{k}} \equiv \perp \tag{41}
\end{equation*}
$$

By S-Trans on (40) and (41), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\# C_{1}\right] \leqslant \perp \tag{42}
\end{equation*}
$$

By S-AndOr11>, we have:
$$
\begin{align*}
& \rho \mathcal{I}\left[\# C_{1}\right] \wedge \rho(\neg \mathrm{U}) \leqslant \rho \mathcal{I}\left[\# C_{1}\right] \\
\text { i.e., } & \rho\left(\mathcal{I}\left[\# C_{1}\right] \wedge \neg \mathrm{U}\right) \leqslant \rho \mathcal{I}\left[\# C_{1}\right] \tag{43}
\end{align*}
$$

By S-Trans on (43) and (42), we have:
$$
\begin{equation*}
\rho\left(\mathcal{I}\left[\# C_{1}\right] \wedge \neg \cup\right) \leqslant \perp \tag{44}
\end{equation*}
$$

The result then follows from IH on (32) and (44).
Case $C_{k} \notin\{\perp, \not X\}$. Let $X_{k}^{C_{k}}=\bigwedge_{l} X_{k l}^{C_{k}}$ for some ${\overline{X_{k l}^{C_{k}}}}^{l}$ where ${\overline{X_{k l}^{C_{k}}}}^{l}$ are not intersections. By S-AndOr11> and S-AndOr12>, we have:
$$
\begin{equation*}
{\overline{X_{k}^{C_{k}} \subseteq X_{k l}^{C_{k}}}}^{l} \tag{45}
\end{equation*}
$$

By S-Trans on (40) and (45), we have:
$$
\begin{equation*}
{\overline{\rho \mathcal{I}\left[\# C_{1}\right] \subseteq X_{k l}^{C_{k}}}}^{l} \tag{46}
\end{equation*}
$$

Notice that $\rho \mathcal{I}\left[\# C_{1}\right]$ is in CDN-normalized form. Since none of the conjuncts of $\rho \mathcal{I}\left[\# C_{1}\right]$ is a negation, $\rho \mathcal{I}\left[\# C_{1}\right]$ is complement-free. Then by Lemma B.82, (46) implies:
$$
\begin{equation*}
\overline{\rho \tau_{l}^{0} \subseteq X_{k l}^{C_{k}}}{ }^{l} \tag{47}
\end{equation*}
$$
for some $\overline{\tau_{l}^{0} \in\{\mathcal{N}, \mathcal{F}, \mathcal{R}\}}{ }^{l}$, where $\mathcal{I}\left[\# C_{1}\right]=\mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R}$. By Lemma B.87, (47) implies:
$$
\begin{equation*}
{\overline{\tau_{l}^{0} \neq \top}}^{l} \tag{48}
\end{equation*}
$$

Then by Lemma B. 60 on (47), we have:
$$
\begin{equation*}
{\overline{X_{k l}^{C_{k}}=\rho \tau_{l}^{0}}}_{l}^{l} \tag{49}
\end{equation*}
$$

By the syntax of $X_{k}^{C_{k}}$ and (49), we have:
$$
\begin{equation*}
{\overline{\rho \tau_{l}^{0}=\rho \tau_{1}^{0}}}^{l} \tag{50}
\end{equation*}
$$

Then we have:
$$
\begin{equation*}
X_{k}^{C_{k}}=\bigwedge_{l} \rho \tau_{1}^{0} \tag{51}
\end{equation*}
$$

Then (36) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash \bigwedge_{l} \rho \tau_{1}^{0} \leq Y_{k} \tag{52}
\end{equation*}
$$

Since $\leq$ implies $\leqslant$, (52) implies:
$$
\begin{array}{ll} 
& \triangleright \Xi \vdash \bigwedge_{l} \rho \tau_{1}^{0} \leq Y_{k} \\
\text { i.e., } & \triangleright \Xi \vdash \rho \tau_{1}^{0} \leqslant Y_{k} \tag{53}
\end{array}
$$

By Theorem B. 88 on (53), (31) implies $Y_{k} \neq \# C_{2}$. By S-AndOr11 ◇ and S-AndOr12 ↓ , we have:
$$
\begin{gather*}
\rho \mathcal{I}\left[\# C_{1}\right]=\rho(\mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R}) \leqslant \rho \tau_{1}^{0}  \tag{54}\\
Y_{k} \leqslant \tau^{0} \vee \bigvee_{j} \# C_{j}^{\prime}=\mathrm{U} \tag{55}
\end{gather*}
$$

By S-Trans on (54) and (53) and (55), we have:
$$
\begin{equation*}
\triangleright \Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \leqslant \mathrm{U} \tag{56}
\end{equation*}
$$

By Theorem B.20, (56) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \wedge \neg \mathrm{U} \leqslant \perp \tag{57}
\end{equation*}
$$

By Lemma B. 30 with Lemma B.25, (57) implies:
$$
\begin{equation*}
\Xi \vdash \rho \mathcal{I}\left[\# C_{1}\right] \wedge \neg \cup \leqslant \perp \tag{58}
\end{equation*}
$$

The result then follows from IH on (32) and (58).
Case C-Cls3. Similar to case C-Cls2.
Case C-Fun1. Then $\mathrm{D}^{0}=\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right)$ for some ${\overline{\mathrm{D}_{i}}}^{i \in 1 . .4}$. From the assumptions, we have:
$$
\begin{gather*}
\Xi \vdash \rho\left(\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge \neg\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right)\right) \leqslant \perp  \tag{59}\\
\Xi \text { cons. }  \tag{60}\\
\Xi \models \rho \Xi_{0} \tag{61}
\end{gather*}
$$

The premises of the rule are:
$$
\begin{gather*}
\Xi_{0}, \triangleleft \Sigma \vdash \mathrm{D}_{3} \ll \mathrm{D}_{1} \Rightarrow \Xi_{1}^{\prime}  \tag{62}\\
\Xi_{0} \cdot \Xi_{1}^{\prime}, \triangleleft \Sigma \vdash \mathrm{D}_{2} \ll \mathrm{D}_{4} \Rightarrow \Xi_{2}^{\prime} \tag{63}
\end{gather*}
$$
for some $\Xi_{1}^{\prime}$ and $\Xi_{2}^{\prime}$, where $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}$. By Theorem B. 20 on (59), we have:
$$
\begin{equation*}
\Xi \vdash \rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \leqslant \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \tag{64}
\end{equation*}
$$

Since $\operatorname{TTV}\left(\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]\right) \cup \operatorname{TTV}\left(\rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right)\right)=\varnothing$, by Lemma B. 49 on (60) and (64), we have:
$$
\begin{equation*}
\triangleright \Xi \vdash \rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \leqslant \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \tag{65}
\end{equation*}
$$

By Lemma B. 89 on (65), we have:
$$
\begin{gather*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \cong \bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right)  \tag{66}\\
\triangleright \Xi \vdash X_{i}^{C_{i}} \leq \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \tag{67}
\end{gather*}
$$
for some ${\overline{\tau_{i}^{\prime}}}^{i}$ and ${\overline{C_{i}}}^{i}$ and ${\overline{X_{i}^{C_{i}}}}^{i}$, where $\bigvee_{i} X_{i}^{C_{i}}$ is complement-free. By S-AndOr12d, we have:
$$
\begin{equation*}
{\overline{\tau_{i}^{\prime} \wedge X_{i}^{C_{i}} \subseteq X_{i}^{C_{i}}}}^{i} \tag{68}
\end{equation*}
$$

By Lemma B.22• on (68), we have:
$$
\begin{equation*}
\bigvee_{i}\left(\tau_{i}^{\prime} \wedge X_{i}^{C_{i}}\right) \subseteq \bigvee_{i} X_{i}^{C_{i}} \tag{69}
\end{equation*}
$$

By S-Trans on (66) and (69), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \subseteq \bigvee_{i} X_{i}^{C_{i}} \tag{70}
\end{equation*}
$$

By Lemma B.59, (67) implies that each of $\bar{C}_{i}^{i}$ is either bottom, arrow, or a negated record field. By Corollary B.86, (70) implies:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \subseteq X_{k}^{C_{k}} \tag{71}
\end{equation*}
$$
for some $k$.
Case $C_{k} \in\{\perp, \not X\}$. Then we have:
$$
\begin{equation*}
X_{k}^{C_{k}} \equiv \perp \tag{72}
\end{equation*}
$$

By S-Trans on (71) and (72), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \leqslant \perp \tag{73}
\end{equation*}
$$
which is impossible by the same reasoning as case C-NotBot. Therefore this case is impossible.
Case $C_{k}=\rightarrow$. Let $X_{k}^{C_{k}}=\bigwedge_{l} X_{k l}^{C_{k}}$ for some ${\overline{X_{k l}^{C_{k}}}}^{l}$ where ${\overline{X_{k l}^{C_{k}}}}^{l}$ are not intersections. By SAndOr11 and S-AndOr12>, we have:
$$
\begin{equation*}
{\overline{X_{k}^{C_{k}} \subseteq X_{k l}^{C_{k}}}}^{l} \tag{74}
\end{equation*}
$$

By S-Trans on (71) and (74), we have:
$$
\begin{equation*}
{\overline{\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \subseteq X_{k l}^{C_{k}}}}^{l} \tag{75}
\end{equation*}
$$

Notice that $\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]$ is in CDN-normalized form. Since none of the conjuncts of $\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]$ is a negation, $\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]$ is complement-free. Then by Lemma B.82, (75) implies:
$$
\begin{equation*}
\overline{\rho \tau_{l}^{0} \subseteq X_{k l}^{C_{k}}} l \tag{76}
\end{equation*}
$$
for some $\overline{\tau_{l}^{0} \in\{\mathcal{N}, \mathcal{F}, \mathcal{R}\}}{ }^{l}$, where $\mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right]=\mathcal{N} \wedge \mathcal{F} \wedge \mathcal{R}$. By Lemma B.87, (76) implies:
$$
\begin{equation*}
{\overline{\tau_{l}^{0} \neq \top}}^{l} \tag{77}
\end{equation*}
$$

Then by Lemma B. 60 on (76), we have:
$$
\begin{equation*}
{\overline{X_{k l}^{C_{k}}=\rho \tau_{l}^{0}}}_{l}^{l} \tag{78}
\end{equation*}
$$

By the syntax of $X_{k}^{C_{k}}$ and (78), we have:
$$
\begin{equation*}
\overline{\rho \tau_{l}^{0}=\rho \tau_{1}^{0}} l \tag{79}
\end{equation*}
$$

Then we have:
$$
\begin{equation*}
X_{k}^{C_{k}}=\bigwedge_{l} \rho \tau_{1}^{0} \tag{80}
\end{equation*}
$$

Then (67) implies:
$$
\begin{equation*}
\triangleright \Xi \vdash \bigwedge_{l} \rho \tau_{1}^{0} \leq \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \tag{81}
\end{equation*}
$$

Since $\leq$ implies $\leqslant$, (81) implies:
$$
\begin{array}{ll} 
& \triangleright \Xi \vdash \bigwedge_{l} \rho \tau_{1}^{0} \leqslant \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \\
\text { i.e., } & \triangleright \Xi \vdash \rho \tau_{1}^{0} \leqslant \rho\left(\mathrm{D}_{3} \rightarrow \mathrm{D}_{4}\right) \tag{82}
\end{array}
$$

By Theorem B. 88 on (82), we have:
$$
\begin{align*}
\tau_{1}^{0}=\mathrm{D}_{1} & \rightarrow \mathrm{D}_{2}  \tag{83}\\
\Xi \vdash \rho \mathrm{D}_{3} & \leqslant \rho \mathrm{D}_{1}  \tag{84}\\
\Xi \vdash \rho \mathrm{D}_{2} & \leqslant \rho \mathrm{D}_{4} \tag{85}
\end{align*}
$$

By IH on (84) and (61) and (62), we have:
$$
\begin{equation*}
\Xi \models \rho \Xi_{1}^{\prime} \tag{86}
\end{equation*}
$$
(61) and (86) imply:
$$
\begin{array}{ll} 
& \Xi \models \rho \Xi_{0} \cdot \rho \Xi_{1}^{\prime} \\
\text { i.e., } & \Xi \models \rho\left(\Xi_{0} \cdot \Xi_{1}^{\prime}\right) \tag{87}
\end{array}
$$

By IH on (85) and (87) and (63), we have:
$$
\begin{equation*}
\Xi \vDash \rho \Xi_{2}^{\prime} \tag{88}
\end{equation*}
$$
(86) and (88) imply:
$$
\begin{array}{ll} 
& \Xi \models \rho \Xi_{1}^{\prime} \cdot \rho \Xi_{2}^{\prime} \\
\text { i.e., } & \Xi \models \rho\left(\Xi_{1}^{\prime} \cdot \Xi_{2}^{\prime}\right) \tag{89}
\end{array}
$$

Case $C_{k}=\chi$. Then $X_{k}^{C_{k}}=\neg \bigvee_{j}\left\{x: \pi_{j}\right\}$ for some $\bar{\pi}_{j}{ }^{j}$. Then (71) implies:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \cong \neg \bigvee_{j}\left\{x: \pi_{j}\right\} \tag{90}
\end{equation*}
$$

By S-AndOr11•, we have:
$$
\begin{equation*}
\left\{x: \pi_{1}\right\} \subseteq \bigvee_{j}\left\{x: \pi_{j}\right\} \tag{91}
\end{equation*}
$$

By S-NegInv on (91), we have:
$$
\begin{equation*}
\neg \bigvee_{j}\left\{x: \pi_{j}\right\} \subseteq \neg\left\{x: \pi_{1}\right\} \tag{92}
\end{equation*}
$$

By S-Trans on (90) and (92), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \subseteq \neg\left\{x: \pi_{1}\right\} \tag{93}
\end{equation*}
$$

By Theorem B. 20 on (93), we have:
$$
\begin{equation*}
\rho \mathcal{I}\left[\mathrm{D}_{1} \rightarrow \mathrm{D}_{2}\right] \wedge\left\{x: \pi_{1}\right\} \subseteq \perp \tag{94}
\end{equation*}
$$
which is impossible by the same reasoning as case C-NotBot. Therefore this case is impossible.
Case C-Rcd1. Similar to case C-Fun1.
Cases C-Fun2, C-Rcd2, C-Rcd3. Similar to case C-NotBot.
Case C-Var1. Then $\mathrm{D}^{0}=\mathrm{C} \wedge \alpha$ and $\Xi^{\prime}=\Xi_{1}^{\prime} \cdot(\alpha \leqslant \neg \mathrm{C})$ for some C and $\alpha$ and $\Xi_{1}^{\prime}$. From the assumptions, we have:
$$
\begin{gather*}
\Xi \vdash \rho(\mathrm{C} \wedge \alpha) \leqslant \perp  \tag{95}\\
\Xi \text { cons. }  \tag{96}\\
\Xi \models \rho \Xi_{0} \tag{97}
\end{gather*}
$$

The premise of the rule is:
$$
\begin{equation*}
\Xi_{0} \cdot(\alpha \leqslant \neg \mathrm{C}), \Sigma \vdash l b_{\Xi_{0}}(\alpha) \ll \neg \mathrm{C} \Rightarrow \Xi_{1}^{\prime} \tag{98}
\end{equation*}
$$

By Theorem B.20, (95) implies:
$$
\begin{align*}
\quad \Xi \vdash \rho \alpha & \leqslant \neg \rho \mathrm{C} \\
\text { i.e., } \quad \Xi \vdash \rho \alpha & \leqslant \rho(\neg \mathrm{C}) \tag{99}
\end{align*}
$$

By S-AndOr2 on S-Hyp, we have:
$$
\begin{equation*}
\Xi_{0} \vdash l b_{\Xi_{0}}(\alpha) \leqslant \alpha \tag{100}
\end{equation*}
$$

By S-Hyp, we have:
$$
\begin{equation*}
(\alpha \leqslant \neg \mathrm{C}) \vdash \alpha \leqslant \neg \mathrm{C} \tag{101}
\end{equation*}
$$

By S-Trans on (100) and (101), we have:
$$
\begin{equation*}
\Xi_{0} \cdot(\alpha \leqslant \neg \mathrm{C}) \vdash l b_{\Xi_{0}}(\alpha) \leqslant \neg \mathrm{C} \tag{102}
\end{equation*}
$$

By Lemma B.36, (102) implies:
$$
\begin{equation*}
\rho\left(\Xi_{0} \cdot(\alpha \leqslant \neg \mathrm{C})\right) \vdash \rho l b_{\Xi_{0}}(\alpha) \leqslant \rho(\neg \mathrm{C}) \tag{103}
\end{equation*}
$$

By S-Cons on (97) and (99), we have:
$$
\begin{align*}
& \quad \Xi \models \rho \Xi_{0} \cdot(\rho \alpha \leqslant \rho(\neg \mathrm{C})) \\
& \text { i.e., } \quad \Xi \models \rho\left(\Xi_{0} \cdot(\alpha \leqslant \mathrm{C})\right) \tag{104}
\end{align*}
$$

By Lemma B. 30 with (104), (103) implies:
$$
\begin{equation*}
\Xi \vdash \rho l b_{\Xi_{0}}(\alpha) \leqslant \rho(\neg \mathrm{C}) \tag{105}
\end{equation*}
$$

By IH on (105) and (104) and (98), we have:
$$
\begin{equation*}
\Xi \models \rho \Xi_{1}^{\prime} \tag{106}
\end{equation*}
$$

By S-Cons on (106) and (99), we have:
$$
\begin{align*}
& \Xi \models \rho \Xi_{1}^{\prime} \cdot(\rho \alpha \leqslant \rho(\neg \mathrm{C})) \\
& \text { i.e., } \quad \Xi \models \rho \Xi^{\prime} \tag{107}
\end{align*}
$$

Case C-Var2. Similar to case C-Var1.