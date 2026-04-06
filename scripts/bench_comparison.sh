#!/bin/bash
set -euo pipefail

FEYNMAN_DIR="${1:-../Feynman_with_units}"
OUTDIR="outputs/bench_comparison"
mkdir -p "$OUTDIR"

GENS=50
POP=100
MAXSZ=7
SEED=42

echo "============================================="
echo "Feynman Benchmark Comparison"
echo "Config: gens=$GENS pop=$POP maxsz=$MAXSZ seed=$SEED"
echo "Dataset: $FEYNMAN_DIR"
echo "============================================="
echo

# ---------- 1. Without multisets ----------
echo "[1/3] Running without multisets (binary Add/Mul)..."
cabal run feynman-bench -- "$FEYNMAN_DIR" $GENS $POP $MAXSZ $SEED \
    --no-multiset \
    --tsv-out "$OUTDIR/no_multiset.tsv" \
    --markdown-out "$OUTDIR/no_multiset.md" \
    2>&1 | tee "$OUTDIR/no_multiset.log" | tail -5
echo

# ---------- 2. With multisets ----------
echo "[2/3] Running with multisets (SumF/ProdF)..."
cabal run feynman-bench -- "$FEYNMAN_DIR" $GENS $POP $MAXSZ $SEED \
    --tsv-out "$OUTDIR/with_multiset.tsv" \
    --markdown-out "$OUTDIR/with_multiset.md" \
    2>&1 | tee "$OUTDIR/with_multiset.log" | tail -5
echo

# ---------- 3. PySR ----------
echo "[3/3] Running PySR (serial)..."
source .venv/bin/activate 2>/dev/null || true
python scripts/bench_pysr.py 2>&1 | tee "$OUTDIR/pysr.log" | tail -5
cp /tmp/feynman_pysr.tsv "$OUTDIR/pysr.tsv" 2>/dev/null || true
echo

# ---------- Summary ----------
echo "============================================="
echo "RESULTS SUMMARY"
echo "============================================="
echo

for label_file in "No Multiset:no_multiset:4:5" "With Multiset:with_multiset:4:5" "PySR:pysr:3:4"; do
    IFS=: read -r label file r2col timecol <<< "$label_file"
    tsv="$OUTDIR/$file.tsv"
    if [ -f "$tsv" ]; then
        total_time=$(awk -F'\t' -v c="$timecol" 'NR>1 {sum+=$c} END {printf "%.1f", sum}' "$tsv")
        n=$(awk -F'\t' 'NR>1 {n++} END {print n}' "$tsv")
        avg=$(awk -F'\t' -v c="$timecol" 'NR>1 {sum+=$c; n++} END {printf "%.2f", sum/n}' "$tsv")
        good=$(awk -F'\t' -v c="$r2col" 'NR>1 && $c+0 >= 0.99 {n++} END {print n+0}' "$tsv")
        ok=$(awk -F'\t' -v c="$r2col" 'NR>1 && $c+0 >= 0.90 {n++} END {print n+0}' "$tsv")
        poor=$(awk -F'\t' -v c="$r2col" 'NR>1 && $c+0 < 0.90 {n++} END {print n+0}' "$tsv")

        printf "%-15s | %5s problems | %7ss total | %5ss/prob | R²≥0.99: %2s | R²≥0.90: %2s | R²<0.90: %2s\n" \
            "$label" "$n" "$total_time" "$avg" "$good" "$ok" "$poor"
    else
        printf "%-15s | TSV not found at %s\n" "$label" "$tsv"
    fi
done

echo
echo "Detailed results in $OUTDIR/"
