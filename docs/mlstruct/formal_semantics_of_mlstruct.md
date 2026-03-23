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

