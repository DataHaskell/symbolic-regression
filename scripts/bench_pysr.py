"""Run PySR on the same Feynman datasets for comparison."""
import os
import sys
import time
import numpy as np
import pandas as pd
from pysr import PySRRegressor

FEYNMAN_DIR = os.path.expanduser("~/code/Feynman_with_units")
N_ROWS = 500
SEED = 42

def load_feynman(path, n_rows=500):
    """Load a Feynman dataset (space-separated, no header)."""
    df = pd.read_csv(path, sep=r'\s+', header=None, nrows=n_rows)
    X = df.iloc[:, :-1].values
    y = df.iloc[:, -1].values
    return X, y

def r2_score(y_true, y_pred):
    ss_res = np.sum((y_true - y_pred) ** 2)
    ss_tot = np.sum((y_true - np.mean(y_true)) ** 2)
    return 1 - ss_res / ss_tot if ss_tot > 0 else 1.0

def run_one(name, path):
    X, y = load_feynman(path, N_ROWS)
    n_vars = X.shape[1]

    model = PySRRegressor(
        niterations=50,
        populations=15,
        population_size=33,
        maxsize=7,
        binary_operators=["+", "-", "*", "/", "^"],
        unary_operators=["neg", "abs", "square", "cube",
                         "sqrt", "exp", "log", "sin", "cos"],
        random_state=SEED,
        deterministic=True,
        parallelism="serial",
        verbosity=0,
        progress=False,
        timeout_in_seconds=30,
    )

    t0 = time.time()
    model.fit(X, y)
    elapsed = time.time() - t0

    try:
        y_pred = model.predict(X)
        r2 = r2_score(y, y_pred)
    except Exception:
        r2 = float('-inf')

    best = str(model.get_best().equation) if len(model.equations_) > 0 else "N/A"
    return name, n_vars, r2, elapsed, best

def main():
    datasets = sorted(os.listdir(FEYNMAN_DIR))
    datasets = [d for d in datasets if not d.startswith('.')]

    print(f"{'Dataset':<20} {'Vars':>5} {'R²':>10} {'Time':>8}")
    print("-" * 48)

    results = []
    total_t0 = time.time()

    for i, name in enumerate(datasets, 1):
        path = os.path.join(FEYNMAN_DIR, name)
        if not os.path.isfile(path):
            continue
        try:
            r = run_one(name, path)
            results.append(r)
            print(f"{r[0]:<20} {r[1]:>5} {r[2]:>10.4f} {r[3]:>7.2f}s  [{i}/{len(datasets)}]")
            sys.stdout.flush()
        except Exception as e:
            print(f"{name:<20} {'':>5} {'ERR':>10} {'':>8}  [{i}/{len(datasets)}] {str(e)[:60]}")
            sys.stdout.flush()

    total_elapsed = time.time() - total_t0

    # Summary
    r2s = [r[2] for r in results if r[2] > float('-inf')]
    good = sum(1 for r in r2s if r >= 0.99)
    ok = sum(1 for r in r2s if 0.90 <= r < 0.99)
    poor = sum(1 for r in r2s if r < 0.90)

    print(f"\n{len(results)}/{len(datasets)} succeeded | total {total_elapsed:.1f}s | avg {total_elapsed/max(1,len(results)):.2f}s/problem")
    print(f"R² >= 0.99: {good}/{len(results)}")
    print(f"R² >= 0.90: {good+ok}/{len(results)}")
    print(f"R² < 0.90: {poor}/{len(results)}")

    # Write TSV
    with open("/tmp/feynman_pysr.tsv", "w") as f:
        f.write("dataset\tvars\tr2\ttime_seconds\tformula\n")
        for r in results:
            f.write(f"{r[0]}\t{r[1]}\t{r[2]:.10f}\t{r[3]:.10f}\t{r[4]}\n")
    print("Results written to /tmp/feynman_pysr.tsv")

if __name__ == "__main__":
    main()
