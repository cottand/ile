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

