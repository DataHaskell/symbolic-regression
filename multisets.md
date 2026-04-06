Quotient Types / Quotienting by Equations                                                                                                                                                                                                         
   
  The most precise framing: a binary tree of + nodes is an element of the free algebra. The A/C axioms define an equivalence relation on that algebra. The paper replaces the free algebra with its quotient — multisets are the quotient of        
  sequences by commutativity and associativity. Structurally equal multisets correspond exactly to A/C-equivalent expressions. This is the type-theoretic notion of a quotient type: choosing a representation where equal things are identical by
  construction.                                                                                                                                                                                                                                     
                                                        
  Built-in Theories (from Automated Reasoning)

  In the term rewriting and unification literature, this is called building a theory into the data structure. AC-unification and AC-matching have been studied for decades — instead of treating associativity and commutativity as rewrite rules,  
  you make the matching algorithm theory-aware. The paper does the same thing but at the representation level rather than the algorithm level, which is arguably cleaner.
                                                                                                                                                                                                                                                    
  Normalization by Construction                         

  Rather than computing a normal form via repeated rewriting (normalize after), you choose a representation where the normal form is the only form (normalize before). Related to the type theory slogan "make illegal states unrepresentable." The 
  multiset can't represent two different orderings of the same sum — there's only one representation.
                                                                                                                                                                                                                                                    
  Choosing the Right Free Object                        

  The tree is the free magma (no equations). The multiset is the free commutative monoid (A/C built in). The nested multiset-of-multisets for polynomials is the free commutative semiring. The general principle: pick the free algebraic structure
   that already satisfies your axioms, so you don't pay for quotienting at runtime.

  ┌──────────────────┬─────────────────────┬─────────┬────────────────────┐
  │      Metric      │ symbolic-regression │ symregg │        PySR        │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ Mean R²          │ 0.8017              │ 0.7054  │ 0.9700             │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ R² >= 0.99       │ 39                  │ 15      │ 76                 │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ R² >= 0.90       │ 48                  │ 32      │ 91                 │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ Avg time/problem │ 7.0s                │ ~10s    │ 4.1s               │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ Language         │ Haskell             │ Haskell │ Julia (via Python) │
  ├──────────────────┼─────────────────────┼─────────┼────────────────────┤
  │ Max size         │ 7                   │ 15      │ 20                 │
  └──────────────────┴─────────────────────┴─────────┴────────────────────┘
  