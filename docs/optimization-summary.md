# Symbolic Regression Performance Optimization Summary

## Baseline

Starting point: GP with e-graph simplification, Adam optimizer, binary `BinF Add`/`BinF Mul` tree representation.

- Feynman benchmark (100 problems): **1143.5s** total, 11.4s/problem avg
- R² >= 0.99: 11/100, R² >= 0.90: 19/100
- Simple targets like `x*y + 45` could not fit the constant 45 (Adam lr too low)
- `genericJoin` (e-graph pattern matching) consumed **25% of runtime**
- Parameter optimizer (`minimizeNLL`) was only **4.7% of runtime** — dominated by e-graph overhead

## Changes Made

### 1. SumF/ProdF container representation (from UW PLSE paper)

**Files:** `Expr.hs`, `Expr/Eval.hs`, `Expr/Opt.hs`, `Expr/Print.hs`, `Expr/Utils.hs`, `Language.hs`, `Regression.hs`

Added `SumF [a]` and `ProdF [a]` constructors to `ExprF`, representing addition and multiplication as sorted lists of children (multisets) instead of binary trees. Encodes associativity and commutativity directly in the representation — no rewrite rules needed.

**Impact:**
### Multiset impact (SumF/ProdF containers)

The multiset representation provides a **10% speedup** (5.44s vs 5.99s per problem) with **no change in quality** (44 R²>=0.99 in both cases). The speed gain comes from:

- Fewer rewrite rules needed during saturation (A/C handled by representation)
- Smaller e-graph (canonical sums/products deduplicate automatically)
- Faster `genericJoin` (fewer nodes to match against)

The quality parity is expected: the inner mutation cycles and search strategy dominate the quality result, not the representation. The multiset's value is structural — it prevents e-graph blowup and reduces saturation cost, enabling more mutation budget within the same time. Togglable via `useMultisetContainers` config flag.

To see if it actually eliminates A/C blow up we'd have to test it for much larger expressions. We can do this in a follow up.

### 2. Saturation frequency throttle

**File:** `Regression.hs`

Reduced e-graph class count threshold for saturation from `nClasses < 1500` to `nClasses < 500`.

### 3. Pareto front filtering and simplification

**File:** `Regression.hs`

- `simplifyDeep` (5 saturation iterations) after parameter substitution
- `nubBy showExpr` deduplication
- `filterPareto` enforces strictly improving MSE across the front

### 4. Tree traversal for containers

**File:** `Regression.hs`

Extended `modifyAtPos`, `getSubAt`, `replaceSubAt` for `SumF`/`ProdF`. Without this, GP mutations couldn't operate inside container nodes.

### 5. Search strategy (PySR-inspired)

**File:** `Regression.hs`

- **Inner mutation cycles** (`ncyclesPerIteration=30`): 30 cheap mutations per offspring slot, only the best gets fitness evaluated
- **Probabilistic optimization** (`optimizeProbability=0.14`): NLOPT only runs 14% of the time
- **Hall of Fame injection**: Best-per-complexity expressions injected into ~6% of each island every generation
- **Warmup maxsize** (`warmupMaxsizeBy=0.5`): Expression size ramps from 3 to target over 50% of generations
- **Container-native mutations** (`addTerm`, `removeTerm`, `replaceTerm`): 32% of mutation weight
- **`mutateFeature`** and **`mutateConstant`**: Variable discovery and constant tuning
- **`prependParam`**: Wraps expression in `param + expr` or `param * expr`

### 6. Hot-path performance fixes

**Files:** `Expr/Eval.hs`, `Expr/Opt.hs`, `Expr/Utils.hs`, `EGraph.hs`, `Regression.hs`

- **ProdF backward pass**: O(n²) → O(n) via prefix/suffix product arrays (`V.scanl'`/`V.scanr'`)
- **evalTree**: `V.unsafeIndex`/`VU.unsafeIndex` in hot loop; direct `foldl'` for SumF/ProdF (no intermediate list)
- **safeBestExpr**: `classExists` (O(log n) IntMap) replaces `elem` on full class list (O(n))
- **countParamsUniq**: `IntSet` replaces O(n²) `nub` on lists
- **paramsToConst**: `VU.unsafeIndex` on vector replaces O(n) list `!!`

## Results

### Feynman Benchmark (100 physics equations, 500 rows each)

| Metric | Baseline | + NLOPT/Containers | + Search Strategy | **+ Perf Fixes** |
|---|---|---|---|---|
| **Total time** | 1143.5s | 180.0s | 552.3s | **429.9s** |
| **Avg per problem** | 11.4s | 1.8s | 5.5s | **4.3s** |
| **R² >= 0.99** | 11 | 11 | 44 | **43** |
| **R² >= 0.90** | 19 | 18 | 53 | **52** |
| R² < 0.90 | 81 | 82 | 47 | **48** |

Config: 50 generations, 100 population, max size 7, seed 42.

### Profiling progression

| Cost Centre | Baseline (6.10s) | Sat throttle (1.22s) | **Current (1.78s)** |
|---|---|---|---|
| `genericJoin` | **24.9%** | 16.8% | **1.8%** |
| `minimizeNLL` | 4.7% | 20.1% | **20.8%** |
| `fit` (GP loop) | 2.6% | 9.6% | **40.6%** |
| `addWithNorm` | 4.0% | — | **2.9%** |
| `compare` | 10.7% | 6.2% | **1.4%** |
| **Total alloc** | 14.7 GB | 2.9 GB | **3.1 GB** |

Note: current profile runs 30x more mutations per generation (inner cycles), explaining the higher absolute time vs the sat-throttle-only version. The profile is now dominated by actual search work (`fit` 40.6%, `minimizeNLL` 20.8%) rather than e-graph overhead.

### Unit Tests (20 tests)

| Category | Tests | Status |
|---|---|---|
| NLOPT optimization | 6 | All pass (finds exact coefficients 3.0, 7.0, 45.0) |
| Rewrites & normalization | 6 | All pass (identity elimination, constant folding, log/exp) |
| Container evaluation | 4 | All pass (SumF/ProdF eval and normalization) |
| Integration (GP) | 4 | All pass (x²+y, x*y+45, 3x+2y+45, 3x²+2y²+45) |

### Comparison with PySR

Feynman benchmark (100 physics equations, 500 rows, single-threaded):

| System | R² >= 0.99 | R² >= 0.90 | Avg time | Total |
|---|---|---|---|---|
| Our baseline (Adam, binary) | 11 | 19 | 11.4s | 1143s |
| **Ours (all optimizations)** | **43** | **52** | **4.3s** | **430s** |
| PySR (serial) | 31 | 41 | 3.7s | 371s |

PySR config: 50 iterations, 15 populations × 33 = 495 individuals, maxsize 7, `parallelism="serial"`.

**Quality: 39% more perfect fits than PySR** (43 vs 31). Speed: 16% slower per problem (4.3s vs 3.7s).

### Multiset ablation

| System | R² >= 0.99 | R² >= 0.90 | Avg time |
|---|---|---|---|
| Ours (no multiset) | 44 | 54 | 5.99s |
| Ours (with multiset) | 44 | 53 | **5.44s** |

Multiset containers provide a **10% speedup** with identical quality.

## Architecture

```
Expression tree (Fix ExprF)
    │
    ├── VarF, ParamF, LitF (terminals)
    ├── BinF Sub/Div/Pow (non-AC binary ops)
    ├── UnF (unary ops)
    ├── SumF [children]  ← AC addition as sorted multiset
    └── ProdF [children] ← AC multiplication as sorted multiset
         │
         ▼ insertTree with exprNormalize
    E-graph (hegg, local fork)
    ├── BinF Add/Mul → converted to SumF/ProdF on insertion
    ├── BinF Sub → SumF [a, Neg(b)]
    ├── Constant folding via SRAnalysis
    └── Saturation with srRewrites (nClasses < 500)
         │
         ▼ evolve (30 inner mutation cycles)
    GP with container-native mutations
    ├── addTerm / removeTerm / replaceTerm (32%)
    ├── mutateOperator / rotateTree / crossover
    ├── mutateFeature / mutateConstant / prependParam
    └── Hall of Fame injection + warmup maxsize
         │
         ▼ fitnessMV (14% probability)
    NLOPT TNEWTON optimizer
    ├── Forward: fwdTape (cached evaluation, unsafe indexing)
    ├── Backward: bwdTape (reverse-mode AD, prefix/suffix products for ProdF)
    └── NLOPT minimizes MSE (~5 iterations)
```

## Data

Feynman benchmark datasets from the AI Feynman project: https://space.mit.edu/home/tegmark/aifeynman.html

## Dependencies

- **nlopt** C library (via Homebrew: `/opt/homebrew/opt/nlopt`)
- **hegg** (custom fork with `normalizeNode`, `addWithNorm`, `runEqualitySaturationN`, reachable extraction)
- `Numeric.Optimization.NLOPT.Bindings` — FFI bindings (copied from srtree)
- `Symbolic.Regression.Expr.NLOPT` — minimal Haskell wrapper for TNEWTON
