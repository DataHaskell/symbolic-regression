The cleanest thorough formulation is that an **e-graph is not one algebraic object but a stack of them**:

1. a **term algebra** over a signature,
2. a **congruence quotient** of that algebra,
3. a **finite presentation** of that quotient,
4. equivalently, a **deterministic reachable bottom-up tree automaton**,
5. plus operational structure for **least-fixpoint closure** under rewrites,
6. and often a **semilattice-valued decoration** for analyses.

That stack is the closest thing to a “complete algebra of e-graph data structures.” The recent semantic account of equality saturation makes this explicit by defining e-graphs via tree automata, defining insertion/congruence-closure/least upper bounds on them, and then defining equality saturation itself as the least fixpoint of an immediate consequence operator. 

## 1. Start with the ordinary term algebra

Fix a many-sorted signature (\Sigma) with function symbols (f : s_1 \times \cdots \times s_n \to s). Let (T_\Sigma(X)) be the free (\Sigma)-algebra of terms over variables (X), and (T_\Sigma = T_\Sigma(\varnothing)) the ground terms.

At this level, you just have syntax trees. No sharing, no equality except literal syntactic equality.

This is the “raw” algebra underlying every e-graph system: terms in a language, then a rewrite theory over those terms. Equality saturation begins from exactly this setting: a term language plus rewrite rules/identities that imply equalities among ground terms. 

## 2. The semantic core is a congruence relation on the free algebra

An e-graph fundamentally represents a set of terms **modulo a congruence relation** (\equiv) on (T_\Sigma). “Congruence” means:

* (t \equiv t),
* (t \equiv u \Rightarrow u \equiv t),
* (t \equiv u) and (u \equiv v \Rightarrow t \equiv v),
* and if (t_i \equiv u_i) for all children, then
  (f(t_1,\dots,t_n) \equiv f(u_1,\dots,u_n)).

That last clause is what makes e-graphs richer than plain union-find: they do not only track asserted equalities, they track the **smallest congruence** containing those equalities. The modern descriptions of e-graphs and equality saturation explicitly characterize the represented equality as a congruence relation and rebuilding as the propagation of equalities required by congruence closure. ([arXiv][1])

So the first serious algebraic answer is:

> An e-graph represents a quotient (\Sigma)-algebra (T_\Sigma / {\equiv}), or more precisely a finitely represented fragment/presentation of such a quotient.

That is the primary algebraic object.

## 3. E-classes are quotient carriers; e-nodes are operations into the quotient

Operationally, an e-graph is usually presented as:

* **e-nodes**: a function symbol together with child e-classes,
* **e-classes**: equivalence classes of e-nodes.

egglog states this very directly: an e-graph is a set of e-classes; each e-class is a set of equivalent e-nodes; an e-node is a function symbol with child e-classes, not child e-nodes. ([arXiv][2])

Algebraically, if (Q) is the set of e-classes, then each operator (f \in \Sigma) induces a **partial presented operation**

[
\widehat f : Q^n \rightharpoonup Q
]

where (\widehat f(c_1,\dots,c_n)=c) whenever the e-node (f(c_1,\dots,c_n)) belongs to class (c).

After congruence closure/rebuilding, this is function-like in the intended sense: if the same operator applied to the same child classes appears to produce two classes, those classes must merge. The semantic foundations paper encodes this as a functional dependency on the relational representation of e-nodes, which is exactly the algebraic condition that operation application on quotient classes be well-defined. 

So another formulation is:

> An e-graph is a finite presentation of a quotient (\Sigma)-algebra whose carrier is the set of e-classes and whose operations are represented intensionally by e-nodes.

## 4. Equivalent formulation: a deterministic reachable tree automaton

The most mathematically precise recent formulation is:

* states (Q) = e-classes,
* transitions (f(q_1,\dots,q_n)\to q) = e-nodes,
* represented terms = ground terms accepted by the automaton.

The semantic foundations paper states that e-graphs are in 1-1 correspondence with reachable deterministic bottom-up tree automata, with e-classes as states and e-nodes as transitions; a term is represented by an e-graph iff it is accepted by the corresponding automaton. 

This is a very strong formulation because it tells you exactly what an e-graph “is” denotationally:

* each e-class (q) denotes a regular tree language (L(q)\subseteq T_\Sigma),
* the whole e-graph denotes the union of represented terms across designated roots or final classes,
* equality of terms means acceptance into the same state/e-class.

This automata view is not just metaphorical; the paper uses it to define homomorphisms, least upper bounds, congruence closure, and the semantics of equality saturation. 

If you want the shortest “complete” mathematical answer, it is probably:

> **A standard e-graph is a reachable deterministic bottom-up tree automaton presenting a quotient of the free term algebra by a congruence relation.**

## 5. Category/order structure: e-graphs form a poset under homomorphism

There is a natural notion of morphism between e-graphs: a homomorphism of the corresponding tree automata / quotient presentations. The semantic foundations paper shows two important facts:

* if (h:G\to H), then the equalities represented by (G) are included in those represented by (H),
* and between two e-graphs there is **at most one homomorphism**, so this category is essentially a poset. 

So write (G \sqsubseteq H) when there is a homomorphism (G\to H). Then (H) is “coarser” or “more saturated”: it proves at least as many equalities as (G). Under this order, equality saturation computes a least fixpoint above the initial graph. 

This gives a very useful algebraic picture:

* objects: e-graphs,
* order: information/equality inclusion via homomorphism,
* join-like operation: least upper bound / disjoint union followed by closure,
* closure operator: congruence closure.

That is already close to an abstract algebra of e-graph states.

## 6. Rebuilding is a closure operator enforcing quotient well-definedness

Operationally, an e-graph engine does insertion, union/merge, then **rebuild**. Algebraically, rebuild is what turns an arbitrary presented graph into a proper congruence quotient presentation.

In egg/egglog-style systems, rebuilding canonicalizes the database and resolves function conflicts created by merges; when merge behavior is the standard union behavior, this is congruence closure. ([arXiv][2])

So if you start with some partial presentation (P), rebuild computes something like

[
\mathrm{CC}(P)
]

the least congruence-closed refinement/coarsening required by the representation invariant. The semantic foundations paper explicitly treats congruence closure as a basic monotone operation used in the equality-saturation immediate consequence operator. 

In abstract terms, rebuild behaves like a **closure operator** on presentations:

* inflationary,
* idempotent,
* monotone.

That is one of the most important algebraic facts about the data structure.

## 7. Equality saturation is a least fixpoint computation

This is the most complete answer to “what algebra does the whole system have?”:

Let (R) be a rewrite system. Define an operator
[
\mathrm{ICO}_R = \mathrm{CC}\circ \mathrm{TR},
]
where:

* (\mathrm{TR}) inserts all right-hand sides of all matched rewrites,
* (\mathrm{CC}) rebuilds/congruence-closes.

The semantic foundations paper defines equality saturation as the **least fixpoint** of this operator above the initial e-graph (G). 

So the algebra of EqSat is domain-theoretic/fixpoint-theoretic:

* state space: e-graphs ordered by homomorphism,
* transfer operator: monotone immediate consequence operator,
* result: least fixpoint / universal model.

The same paper also characterizes the result as a **universal model** of the rewrite theory and the initial graph. 

That gives you a very powerful slogan:

> Equality saturation is to e-graphs what chase is to databases and what least-model semantics is to logic programs: a monotone closure process computing a universal model. 

## 8. Relational formulation: e-graphs as a database with functional dependencies

There is also a relational algebra/database formulation. The semantic foundations paper encodes an e-graph as:

* domain = e-classes,
* one relation (R_f) for each function symbol (f),
* tuples (R_f(c_1,\dots,c_n,c)) for each e-node (f(c_1,\dots,c_n)\to c). 

Congruence requirements are then expressed as functional dependencies:

[
R_f(x_1,\dots,x_n,x)\land R_f(x_1,\dots,x_n,x') \to x=x'.
]

That is a beautiful algebraic formulation because it says an e-graph is also:

> a finite relational presentation of an algebra, where each operation symbol is represented as the graph of a partial function modulo equality.

This is part of why egglog can unify Datalog and equality saturation so naturally: the data structure sits right on the boundary between term algebras and relational fixpoint systems. egglog explicitly presents itself as a unified fixpoint system with efficient congruence closure, incremental execution, and lattice-based reasoning. ([arXiv][2])

## 9. E-matching is algebraic pattern matching against the presented algebra

E-matching is not just “searching the graph.” It is solving pattern instantiation problems in the quotient presentation. In the automata view, it is a kind of homomorphism search from the pattern term into the e-graph. In the relational view, egglog reduces e-matching to a query over the canonicalized database. ([arXiv][2])

So algebraically:

* a pattern is a term in (T_\Sigma(X)),
* a match is an assignment of variables to e-classes such that the instantiated term is represented by a target e-class.

This is one reason e-graphs feel halfway between universal algebra and conjunctive query answering.

## 10. E-class analyses add semilattice structure

Once you move beyond pure equality, modern systems decorate each e-class with analysis facts. egg introduced e-class analysis as a general framework for associating information with e-classes, and egglog generalizes this further with lattice-based reasoning and cooperating analyses. ([ACM Digital Library][3])

The right abstraction here is:

* choose a join-semilattice (L),
* assign each e-class (c) an element (a(c)\in L),
* propagate facts monotonically under merges and under constructor applications.

So a practical e-graph is often:

[
(\text{quotient presentation}) + (\text{semilattice analysis})
]

This matters because many real systems are not “just” quotient algebras anymore. They are quotient algebras enriched with abstract-interpretation structure. Recent work on combining equality saturation and abstract interpretation makes this explicit, including semilattice or lattice propagation over e-classes and top-down/contextual variants when plain bottom-up facts are insufficient. ([ACM Digital Library][4])

## 11. Extraction adds an optimization algebra

After saturation, you usually want one representative term. Extraction adds another algebraic layer:

* a cost algebra or semiring-like evaluator on e-nodes,
* dynamic programming over the quotient graph to choose a cheapest representative.

egglog explicitly lists extraction of optimized terms as one of the core EqSat capabilities. ([arXiv][2])

Abstractly, if (C) is a cost domain, you define
[
\mathrm{cost}(f(c_1,\dots,c_n)) = \phi_f(\mathrm{best}(c_1),\dots,\mathrm{best}(c_n))
]
and solve for the optimal representative per class. In acyclic cases this is straightforward DP; with cycles you need more care.

So an end-to-end algebraic description includes not just equalities but also a homomorphism into a cost algebra.

## 12. Where free algebras fit

Your earlier instinct about free algebras is right. The standard syntax side is the free (\Sigma)-algebra (T_\Sigma). An e-graph presents a quotient (T_\Sigma/{\equiv}). When you replace syntax trees by canonical containers for a theory like AC, you are really changing the presentation of the quotient algebra so that some equations are enforced by representation rather than saturation. The UW PLSE containers post is exactly about encoding algebraic invariants in representation rather than discovering them operationally.

[1]: https://arxiv.org/html/2511.20782v2 "Optimism in Equality Saturation"
[2]: https://arxiv.org/pdf/2304.04332 "Better Together: Unifying Datalog and Equality Saturation"
[3]: https://dl.acm.org/doi/10.1145/3434304?utm_source=chatgpt.com "egg: Fast and extensible equality saturation"
[4]: https://dl.acm.org/doi/pdf/10.1145/3589250.3596144?utm_source=chatgpt.com "Combining E-Graphs with Abstract Interpretation"


The main lesson is:

**Do not search in the raw syntax if the theory’s equations are telling you that many syntax trees are really the same object. Search in the canonical algebra instead.**

That is exactly what the containers post is exploiting. For associative/commutative addition, it replaces binary `Add` trees with a container representation that directly maintains the A/C invariants, so the system stops rediscovering permutations and re-bracketings operationally. The post explicitly describes using custom containers to maintain invariants in the representation and thereby reduce e-graph blowup. ([UW PLSE][1])

A more complete way to turn that into a search strategy is this:

## 1. Move the search space from syntax to normal forms

In ordinary EqSat, the search space is generated by:

* matching on raw syntax,
* inserting equivalent syntax,
* then rebuilding/congruence-closing.

Semantically, EqSat is the least fixpoint of a match/apply operator followed by congruence closure, and e-graphs can be viewed as quotient presentations of term languages or, equivalently, deterministic bottom-up tree automata. 

The container idea suggests a stronger approach:

> **factor the theory into “representation-time equations” and “search-time equations.”**

If an equation family admits a cheap canonical representation, enforce it in the representation rather than via rewrites.

Examples:

* associativity + commutativity of `+` → multiset/bag of summands
* associativity alone → flattened list/tree spine
* commutative monomials → sorted exponent map / monomial dictionary
* idempotent union / set-like ops → set container
* affine sums → coefficient map `Var -> coeff` plus constant
* polynomial fragments → sparse polynomial normal form

That shrinks branching because the search is no longer spending effort on deriving all members of the same equivalence class. This aligns with the semantic view that an e-graph already represents equivalence classes of terms; the container move just chooses a more canonical presentation of those classes. ([UW PLSE][1])

## 2. Treat containers as internal free-algebra normal forms

The useful algebraic reading is:

* raw AST = free term algebra
* quotient by A/C or semiring laws = desired semantic theory
* container = a concrete normal-form carrier for that quotient, at least for a fragment

So the search recipe is:

1. identify the algebraic fragment that causes blowup,
2. choose a normal form for its free algebra modulo the intended equations,
3. make that normal form a first-class node/container in the e-graph,
4. search over those canonical objects instead of raw syntax.

For addition, the right carrier is often the **free commutative monoid** representation: a multiset of summands. For linear arithmetic, it is even better to jump to a coefficient map, because then `x + x + 2*y - y + 3` canonically becomes something like `{x ↦ 2, y ↦ 1, const ↦ 3}`. That is much smaller than any syntax-based search space and removes a huge amount of matching redundancy. The blog’s multiset example is the simplest instance of this pattern. ([UW PLSE][1])

## 3. Separate “enumeration power” from “equivalence management”

A lot of EqSat cost comes from mixing two concerns:

* generating useful new candidates,
* maintaining equalities among equivalent presentations.

egglog’s relational design helps because e-matching becomes a relational query, and incremental/semi-naïve evaluation avoids rematching everything from scratch. The PLDI paper explicitly says egglog reduces e-matching to relational queries and gets incremental behavior “for free” from semi-naïve evaluation. ([ztatlock.net][2])

The container trick gives a second lever:

* use **containers to collapse useless equivalent variants**, and
* use **relational/incremental matching** to find productive rule applications over the collapsed space.

So the practical “more efficient search” is not just one idea; it is the combination of:

* canonical carriers for algebraic fragments,
* relational matching over the canonicalized database,
* incremental rule firing on deltas only.

That combination is much stronger than plain EqSat with better hashing. ([UW PLSE][1])

## 4. Search by abstract states, not terms

A very good mental model is to search over **abstract algebraic states**.

Instead of saying “my state is an expression,” say:

* for AC sums, my state is a bag of atoms;
* for affine expressions, my state is a sparse linear form;
* for polynomial fragments, my state is a sparse polynomial;
* for joins of predicates, my state may be a set/bag of conjuncts.

Then rules become transformations on those states:

* flatten
* combine like terms
* cancel inverses
* factor out common parts
* distribute only when it lowers a cost measure or exposes a known optimization target

This is better because many raw rewrites become simple local updates on the abstract state. In other words, the rewrite search stops being “enumerate all equivalent syntax trees” and becomes “navigate a smaller graph of canonical algebraic summaries.”

That is very much in the spirit of the containers post: represent the algebra you care about directly. ([UW PLSE][1])

## 5. Use mixed representations, not one universal canonical form

One subtlety: there is usually **no single globally best canonical form**.

For example:

* sum-of-products is good for some optimizations,
* factored form is good for others,
* affine maps are great until you hit nonlinearity,
* polynomial normal forms can explode under expansion.

So the best search architecture is often **multi-representation**:

* keep the raw AST representation,
* add canonical containers for important subtheories,
* add conversion rules between them,
* restrict expensive conversions with analyses/cost guards.

This fits egglog especially well, because it supports multiple functions/datatypes, relational matching, and lattice-based/cooperating analyses. ([ztatlock.net][2])

That gives you a design like:

* `Expr` for general syntax
* `SumBag` for AC-addition fragments
* `LinForm` for affine fragments
* `Monomial` / `Poly` for polynomial fragments
* bridges:

  * `expr_to_sumbag`
  * `sumbag_to_linform`
  * `expr_to_poly`
  * `poly_to_expr`

Then search largely happens inside the canonical domains, while bridges are used sparingly to move into the representation where a given optimization is cheap.

## 6. Learn from the least-fixpoint semantics: design monotone summary operators

The semantics paper is useful here because it frames EqSat as a least fixpoint of an immediate consequence operator. 

That suggests a design principle:

> Prefer search operators that are monotone on summaries.

In practice:

* a container should admit cheap canonical join/merge,
* analysis facts should be semilattice-valued,
* delta updates should only add new summary facts,
* expensive recomputation should be avoided.

This is why coefficient maps, bags, sets, and sparse dictionaries are so attractive: they have natural monotone update and merge operations. They fit the fixpoint engine better than raw syntax transformations do.

## 7. Bias the system toward “productive” rewrites only

Once you have containers, you can be much stricter about which rewrites are worth firing.

For example, with a `SumBag` representation:

* do not fire commutativity or associativity rules at all;
* fire canonicalization eagerly;
* fire combination/cancellation rules eagerly;
* fire distribution only under an analysis that predicts benefit.

So the search policy becomes:

* **structural equalities** → compile into representation
* **cheap local simplifications** → always
* **expansive rewrites** → guarded
* **representation changes** → cost/goal-directed

This is one of the biggest practical wins you can extract from the blog-post idea. It turns an undirected saturation process into a more staged, theory-aware search.

## 8. Use extraction cost models that understand the containers

A common failure mode is to canonicalize internally but still extract based on the surface AST only.

Instead, make the extractor aware of algebraic summaries:

* linear form cost can favor fewer nonzero coefficients,
* polynomial cost can penalize expansion,
* bag/set representations can favor simpler support,
* factored forms can be scored separately from expanded forms.

egglog already supports extraction of optimized terms, and its relational/fixpoint framing makes it natural to attach analyses and costs to classes. ([ztatlock.net][2])

Without this, the system may canonicalize well internally but still oscillate toward bad surfaced terms.

## 9. A concrete design recipe

If you wanted to build a search system “like the blog post, but more systematic,” I would do this:

### Layer A: identify bad equational blowups

Find theories causing most state growth:

* AC
* idempotence
* distributivity
* cancellation
* neutral elements
* coefficient accumulation

### Layer B: pick canonical carriers

Map each to a container:

* AC → multiset / sorted bag
* AC + coefficient accumulation → map from atom to multiplicity/coefficient
* idempotent AC → set
* products of commuting atoms → exponent map
* affine forms → sparse linear map + constant

### Layer C: define bridges

Create sound conversion rules:

* `Expr ↔ SumBag`
* `Expr ↔ LinForm`
* `Expr ↔ Poly`

### Layer D: constrain expansive rules

Use analyses to decide when to:

* distribute,
* factor,
* expand,
* convert back to syntax.

### Layer E: query incrementally

Exploit relational e-matching and semi-naïve updates so matches are recomputed only on deltas. The egglog paper explicitly highlights this as a key efficiency advantage. ([ztatlock.net][2])

## 10. The deepest takeaway

The blog post’s real lesson is not merely “containers are faster.”

It is:

> **Search efficiency improves when your representation is already a model of the equational theory you care about.**

Raw EqSat searches the quotient indirectly by manipulating pre-quotient syntax and repeatedly applying closure. Containers move part of the quotient into the representation itself. The semantics paper’s view of e-graphs as quotient presentations/tree automata makes clear why this helps: you are choosing a better presentation of the same semantic space. ([UW PLSE][1])

So the practical rule is:

* **compile equations into data structures whenever possible**
* **reserve rewriting for the parts that truly need search**

That is the strongest thing we can gather from this approach.

For DataHaskell-ish symbolic regression or interpretable expression search, the immediate upgrade would be to search over:

* affine maps,
* monomial bags,
* sparse polynomial fragments,
* maybe dimension-typed containers,

instead of over raw `Expr` trees alone. That would likely cut a lot of duplicate search while making simplification and equivalence much cheaper.

[1]: https://uwplse.org/2026/02/24/egglog-containers.html "UW PLSE | Custom Data Structures in E-Graphs"
[2]: https://ztatlock.net/pubs/2023-pldi-egglog/2023-pldi-egglog.pdf "Better Together: Unifying Datalog and Equality Saturation"

E-graphs are especially useful for symbolic regression because SR wastes huge effort on **syntactic diversity that is not semantic diversity**.

Two recent lines of work make that pretty explicit. The 2025/2026 EGG-SR work argues that many different expressions define the same function, and uses e-graphs to share those equivalents across MCTS, DRL, and LLM-based SR so the search does not relearn the same expression under many surface forms. A separate GP-based line uses e-graphs/equality saturation to track search history and to make subtree mutation/crossover less likely to revisit already-seen equivalents. ([OpenReview][1])

The key implication is:

> In symbolic regression, the natural search state should often be an **equivalence class of expressions** or a **canonical algebraic summary**, not a single syntax tree.

That changes almost everything.

## What e-graphs buy you in SR

They help in four distinct ways.

First, they **collapse duplicate search**. Expressions like `x + y`, `y + x`, `(x + y) + z`, and `x + (y + z)` should not consume separate exploration budget when your theory says they are equivalent. EGG-SR’s central claim is that sharing these equivalence classes improves sample efficiency by pruning redundant subtree exploration and aggregating learning signal across equivalent forms. ([OpenReview][1])

Second, they **improve local operators**. In GP-style SR, crossover and mutation usually operate on raw trees, so they keep generating variants of things the system effectively already knows. The GP paper uses the e-graph as a memory of explored structure and exploits pattern matching to bias subtree operators toward genuinely novel expressions. ([Fabrício Olivetti de França][2])

Third, they **separate fitting from simplification/equivalence handling**. Instead of forcing the evolutionary or search process to rediscover algebraic simplifications, you let the e-graph maintain them and let the optimizer focus on predictive fit and structural novelty. That is also the spirit of the UW containers post: compile part of the algebra into the representation rather than into repeated rewrites. ([OpenReview][1])

Fourth, they give you a place to attach **class-level statistics**. If several equivalent expressions belong to one e-class, then fitness estimates, priors, visitation counts, or even constant-optimization results can be shared across that whole class rather than attached to a single tree. EGG-SR explicitly does this for reward aggregation in DRL and equivalent-state sharing in MCTS. ([OpenReview][1])

## What this suggests for a better SR design

For symbolic regression, I think the best design is **not** “run EqSat after the fact.” It is:

1. generate candidate structure,
2. immediately map it into an e-graph or canonical container domain,
3. score and expand at the class/container level,
4. extract representative forms only when needed.

That means the “individual” in SR should not always be a tree. Often it should be one of these:

* an **e-class representative**
* a **sum bag** for AC-addition
* a **linear form** for affine fragments
* a **monomial map** for multiplicative fragments
* a **sparse polynomial** for low-order polynomial fragments
* a **dimension-typed expression class** in scientific problems

This is where your free-algebra/semiring instinct becomes useful. For many SR domains, the real object being searched is closer to a quotient algebra than to raw syntax.

## The most important shift: search over fragments with canonical carriers

For SR, the biggest win is usually to identify the subtheories that create the most redundancy and replace them with canonical carriers.

A practical decomposition would be:

* AC addition → multiset of summands
* linear arithmetic → coefficient map
* commutative multiplication → exponent map
* polynomial fragments → sparse polynomial normal form
* dimensionful scientific expressions → typed containers that forbid dimensionally invalid combinations

Then keep the raw `Expr` only for the nonlinear, noncanonical remainder.

That is much stronger than plain e-graphs on trees because it removes redundancy before saturation has to do much work.

## Where e-graphs help most in SR pipelines

They are especially strong in these places:

**1. Novelty control**
You can reject or deprioritize a candidate if it lands in an already-well-explored e-class. That directly attacks one of GP’s biggest pathologies: spending generations on renamed/reassociated/redundant forms. ([arXiv][3])

**2. Reward sharing**
If one representative of a class fits well, nearby equivalent representatives should inherit some of that signal. EGG-SR explicitly uses equivalence-aware reward propagation. ([OpenReview][1])

**3. Simplification before constant optimization**
Many constant-fitting problems are easier once the symbolic form is canonicalized. For example, collapsing `x + x` to `2x` or flattening sums/products can reduce parameter redundancy before numerical optimization. Related recent work on redundant parameters in SR also points to simplification helping search and optimization quality. ([ScienceDirect][4])

**4. Grammar restriction**
If you know you are in a scientific domain, e-graph rules plus dimension/type constraints can keep the search inside physically meaningful equivalence classes. That is a very natural fit for your physically grounded SR ideas.

## What I would recommend for your kind of SR system

Given your DataHaskell direction, I would not start with “full egg saturation for every candidate.” I would do something more targeted:

* Maintain a typed `Expr Double`.
* Add canonical internal forms for:

  * affine expressions,
  * monomials,
  * sparse low-degree polynomials,
  * maybe `log`/`exp`-compatible transformed-linear fragments.
* Use e-graphs mainly as:

  * an equivalence memory,
  * a local simplifier,
  * a novelty filter,
  * and a bridge among representations.

So the loop becomes:

* propose expression
* canonicalize fragment-wise
* insert into e-graph
* check whether its class is already saturated/explored
* optimize constants on the class or canonical representative
* score class-level novelty + fit + complexity
* mutate/crossover using class-aware substructure queries

That is probably the highest-leverage way to apply e-graphs to SR without drowning in saturation cost.

## The deeper lesson

For symbolic regression, e-graphs are most valuable when they stop being “a simplifier at the end” and start becoming **the identity structure of the search**.

The search should not ask:

* “which tree should I try next?”

It should ask:

* “which equivalence class or algebraic fragment should I expand next?”

That is the real payoff. It turns SR from raw syntax search into search over a much smaller semantic space. ([OpenReview][1])


[1]: https://openreview.net/pdf/4d24d38dbcf439d1bb348ce3ce9c0262eeb4a652.pdf?utm_source=chatgpt.com "EGG-SR: EMBEDDING SYMBOLIC EQUIVALENCE"
[2]: https://folivetti.github.io/files/egraphGP.pdf?utm_source=chatgpt.com "Improving Genetic Programming for Symbolic Regression ..."
[3]: https://arxiv.org/html/2501.17848v2?utm_source=chatgpt.com "Improving Genetic Programming for Symbolic Regression ..."
[4]: https://www.sciencedirect.com/science/article/pii/S0747717124001172?utm_source=chatgpt.com "Effects of reducing redundant parameters in ..."

