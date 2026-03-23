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

