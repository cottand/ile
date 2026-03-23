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

- [1 Introduction](introduction.md)
- [2 Presentation of MLstruct](presentation_of_mlstruct.md)
- [3 Inferring Principal Types for MLstruct](inferring_principal_types_for_mlstruct.md)
- [4 Formal Semantics of MLstruct](formal_semantics_of_mlstruct.md)
- [5 Principal Type Inference for lambda-not](principal_type_inference_for_lambda.md)
- [6 Related Work](related_work.md)
- [7 Conclusion and Future Work](conclusion_and_future_work.md)
- [References](references.md)
- [A Formalization, Continued](formalization_continued.md)
- [B Formal Correctness Proofs](formal_correctness_proofs.md)
