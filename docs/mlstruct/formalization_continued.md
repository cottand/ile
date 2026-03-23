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

