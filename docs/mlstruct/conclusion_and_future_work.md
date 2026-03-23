\section*{7 CONCLUSION AND FUTURE WORK}

In this paper, we saw that polymorphic type inference for first-class union, intersection, and negation types is possible, enabling class-instance matching patterns yielding very precise types, comparable in expressiveness to row-polymorphic variants. We saw that this type inference approach relies on two crucial aspects of MLstruct's type system: 1. using the full power of Boolean algebras to normalize types and massage constraints into shapes amenable to constraint solving without backtracking; and 2. approximating some unions and intersections, most notably unions of records and intersections of functions, in order to remove potential ambiguities during constraint solving without threatening the soundness of the system.

Future Work. In the future, we intend to explore more advanced forms of polymorphism present in MLscript, such as first-class polymorphism, as well as how to remove some of the limitations of regular types, which currently prevent fully supporting object-oriented programming idioms.

Acknowledgements. We would like to sincerely thank the anonymous reviewers as well as François Pottier, Didier Rémy, Alan Mycroft, Bruno C. d. S. Oliveira, Andong Fan, and Anto Chen for their constructive and helpful comments on earlier versions of this paper. We are particularly grateful to Stephen Dolan, who gave us some invaluable feedback and mathematical intuitions on the development of this new algebraic subtyping system.

