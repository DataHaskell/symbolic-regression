#!/usr/bin/env python3
"""
Compare the local Haskell symbolic-regression benchmark against PySR on
Feynman_with_units, writing machine-readable outputs plus a markdown summary.

This script is intended to be run from a Python virtual environment.
Use scripts/profile_feynman.sh as the entrypoint so the venv is activated first.
"""

from __future__ import annotations

import argparse
import csv
import cProfile
import io
import os
import pstats
import resource
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import numpy as np


@dataclass
class RunResult:
    command: list[str]
    returncode: int
    stdout: str
    stderr: str
    wall_seconds: float
    max_rss_kb: int | None


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Profile symbolic-regression and PySR on Feynman_with_units."
    )
    parser.add_argument(
        "feynman_path",
        nargs="?",
        default="../Feynman_with_units",
        help="Directory or single dataset file to benchmark.",
    )
    parser.add_argument(
        "--output-dir",
        default="bench/profile_out",
        help="Directory for reports and raw outputs.",
    )
    parser.add_argument(
        "--cabal-dir",
        default=None,
        help="Optional CABAL_DIR override, useful in restricted environments.",
    )
    parser.add_argument(
        "--datasets",
        nargs="*",
        default=None,
        help="Optional dataset basenames to restrict the run, e.g. I.12.1 I.14.3.",
    )
    parser.add_argument("--rows", type=int, default=500)
    parser.add_argument("--seed", type=int, default=42)

    parser.add_argument("--haskell-gens", type=int, default=50)
    parser.add_argument("--haskell-pop", type=int, default=100)
    parser.add_argument("--haskell-maxsize", type=int, default=7)
    parser.add_argument("--no-egraph", action="store_true")
    parser.add_argument(
        "--haskell-profile-dataset",
        default=None,
        help="Dataset basename to run with RTS profiling using sr-profile.",
    )

    parser.add_argument("--skip-haskell", action="store_true")
    parser.add_argument("--skip-pysr", action="store_true")

    parser.add_argument("--pysr-repo", default="../pysr")
    parser.add_argument("--pysr-procs", type=int, default=0)
    parser.add_argument("--pysr-populations", type=int, default=8)
    parser.add_argument("--pysr-population-size", type=int, default=48)
    parser.add_argument("--pysr-iterations", type=int, default=40)
    parser.add_argument("--pysr-maxsize", type=int, default=20)
    parser.add_argument("--pysr-model-selection", default="best")
    parser.add_argument("--pysr-cprofile", action="store_true")
    parser.add_argument(
        "--pysr-binary-operators",
        nargs="*",
        default=["+", "-", "*", "/"],
    )
    parser.add_argument(
        "--pysr-unary-operators",
        nargs="*",
        default=["cos", "exp", "log", "sin", "sqrt", "inv(x) = 1/x"],
    )

    return parser.parse_args()


def ensure_venv() -> str:
    venv = os.environ.get("VIRTUAL_ENV")
    if not venv:
        raise SystemExit(
            "This script must run inside a Python virtual environment. "
            "Use scripts/profile_feynman.sh --venv <path> ..."
        )
    return venv


def dataset_entries(root: Path, selected: list[str] | None) -> list[tuple[str, Path]]:
    if root.is_dir():
        names = sorted(p.name for p in root.iterdir() if not p.name.startswith("."))
        if selected:
            wanted = set(selected)
            names = [name for name in names if name in wanted]
        return [(name, root / name) for name in names]
    name = root.name
    if selected and name not in set(selected):
        return []
    return [(name, root)]


def mkdir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)


def run_command(command: list[str], cwd: Path, extra_env: dict[str, str] | None = None) -> RunResult:
    start = time.perf_counter()
    env = os.environ.copy()
    if extra_env:
        env.update(extra_env)
    proc = subprocess.run(
        command,
        cwd=cwd,
        text=True,
        capture_output=True,
        check=False,
        env=env,
    )
    wall = time.perf_counter() - start
    stderr = proc.stderr
    max_rss_kb = None
    if stderr:
        max_rss_kb = parse_macos_time_maxrss(stderr)
    return RunResult(
        command=command,
        returncode=proc.returncode,
        stdout=proc.stdout,
        stderr=stderr,
        wall_seconds=wall,
        max_rss_kb=max_rss_kb,
    )


def parse_macos_time_maxrss(stderr: str) -> int | None:
    for line in stderr.splitlines():
        parts = line.strip().split()
        if len(parts) >= 2 and parts[0].isdigit() and "maximum resident set size" in line:
            return int(parts[0])
    return None


def read_tsv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def write_tsv(path: Path, rows: list[dict[str, Any]], fieldnames: list[str]) -> None:
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, delimiter="\t")
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def read_dataset(path: Path, rows: int) -> tuple[np.ndarray, np.ndarray]:
    matrix = np.loadtxt(path, max_rows=rows)
    if matrix.ndim == 1:
        matrix = matrix.reshape(1, -1)
    return matrix[:, :-1], matrix[:, -1]


def compute_r2(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    y_true = np.asarray(y_true, dtype=float)
    y_pred = np.asarray(y_pred, dtype=float)
    ss_tot = float(np.sum((y_true - np.mean(y_true)) ** 2))
    ss_res = float(np.sum((y_true - y_pred) ** 2))
    if ss_tot == 0.0:
        return 1.0
    return 1.0 - (ss_res / ss_tot)


def maybe_import_pysr(repo: Path) -> tuple[Any | None, str | None]:
    sys.path.insert(0, str(repo.resolve()))
    try:
        from pysr import PySRRegressor  # type: ignore
    except Exception as exc:  # pragma: no cover - environment dependent
        return None, f"{type(exc).__name__}: {exc}"
    return PySRRegressor, None


def run_haskell_bench(args: argparse.Namespace, output_dir: Path, repo_root: Path) -> dict[str, Any]:
    markdown_path = output_dir / "haskell_results.md"
    tsv_path = output_dir / "haskell_results.tsv"
    command = [
        "/usr/bin/time",
        "-l",
        "cabal",
        "run",
        "feynman-bench",
        "--",
        args.feynman_path,
        str(args.haskell_gens),
        str(args.haskell_pop),
        str(args.haskell_maxsize),
        str(args.seed),
        "--rows",
        str(args.rows),
        "--markdown-out",
        str(markdown_path),
        "--tsv-out",
        str(tsv_path),
    ]
    if args.no_egraph:
        command.append("--no-egraph")

    extra_env = {"CABAL_DIR": args.cabal_dir} if args.cabal_dir else None
    result = run_command(command, repo_root, extra_env=extra_env)
    (output_dir / "haskell.stdout.log").write_text(result.stdout)
    (output_dir / "haskell.stderr.log").write_text(result.stderr)

    if result.returncode != 0:
        return {
            "status": "error",
            "error": "Haskell benchmark command failed",
            "command": shlex.join(command),
            "stderr_log": str(output_dir / "haskell.stderr.log"),
        }

    rows = read_tsv(tsv_path)
    return {
        "status": "ok",
        "command": shlex.join(command),
        "wall_seconds": result.wall_seconds,
        "max_rss_kb": result.max_rss_kb,
        "tsv_path": str(tsv_path),
        "markdown_path": str(markdown_path),
        "rows": rows,
    }


def run_haskell_profile(
    args: argparse.Namespace,
    output_dir: Path,
    repo_root: Path,
    dataset_path: Path,
) -> dict[str, Any]:
    command = [
        "/usr/bin/time",
        "-l",
        "cabal",
        "run",
        "sr-profile",
        "--",
        str(dataset_path),
        str(args.haskell_gens),
        str(args.haskell_pop),
        str(args.haskell_maxsize),
        str(args.seed),
        "--rows",
        str(args.rows),
        "+RTS",
        "-p",
        "-s",
        "-hy",
        "-RTS",
    ]
    if args.no_egraph:
        command.insert(-4, "--no-egraph")

    extra_env = {"CABAL_DIR": args.cabal_dir} if args.cabal_dir else None
    result = run_command(command, repo_root, extra_env=extra_env)
    (output_dir / "haskell-profile.stdout.log").write_text(result.stdout)
    (output_dir / "haskell-profile.stderr.log").write_text(result.stderr)

    artifacts: dict[str, str] = {}
    for suffix in (".prof", ".hp", ".eventlog", ".ps", ".aux"):
        src = repo_root / f"sr-profile{suffix}"
        if src.exists():
            dst = output_dir / src.name
            src.replace(dst)
            artifacts[suffix] = str(dst)

    return {
        "status": "ok" if result.returncode == 0 else "error",
        "command": shlex.join(command),
        "wall_seconds": result.wall_seconds,
        "max_rss_kb": result.max_rss_kb,
        "artifacts": artifacts,
        "stderr_log": str(output_dir / "haskell-profile.stderr.log"),
    }


def fit_one_pysr(
    PySRRegressor: Any,
    dataset_name: str,
    dataset_path: Path,
    args: argparse.Namespace,
    output_dir: Path,
) -> dict[str, Any]:
    X, y = read_dataset(dataset_path, args.rows)
    n_procs = args.pysr_procs if args.pysr_procs > 0 else None
    equation_file = output_dir / f"pysr_{dataset_name}.csv"
    model = PySRRegressor(
        model_selection=args.pysr_model_selection,
        niterations=args.pysr_iterations,
        binary_operators=args.pysr_binary_operators,
        unary_operators=args.pysr_unary_operators,
        maxsize=args.pysr_maxsize,
        populations=args.pysr_populations,
        population_size=args.pysr_population_size,
        random_state=args.seed,
        deterministic=True,
        progress=False,
        procs=n_procs,
        temp_equation_file=True,
        equation_file=str(equation_file),
    )

    cpu_before = time.process_time()
    rss_before = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    wall_start = time.perf_counter()

    if args.pysr_cprofile:
        profile = cProfile.Profile()
        profile.enable()
        model.fit(X, y)
        profile.disable()
        stats_stream = io.StringIO()
        pstats.Stats(profile, stream=stats_stream).sort_stats("cumtime").print_stats(40)
        (output_dir / f"pysr_{dataset_name}.cprofile.txt").write_text(stats_stream.getvalue())
    else:
        model.fit(X, y)

    wall_seconds = time.perf_counter() - wall_start
    cpu_seconds = time.process_time() - cpu_before
    rss_after = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss

    y_pred = np.asarray(model.predict(X), dtype=float)
    r2 = compute_r2(y, y_pred)

    equation = ""
    complexity: Any = ""
    loss: Any = ""
    equations = getattr(model, "equations_", None)
    if equations is not None and len(equations) > 0:
        best_row = equations.iloc[-1]
        equation = str(best_row.get("equation", ""))
        complexity = best_row.get("complexity", "")
        loss = best_row.get("loss", "")

    return {
        "dataset": dataset_name,
        "vars": X.shape[1],
        "rows": X.shape[0],
        "r2": f"{r2:.10f}",
        "wall_seconds": f"{wall_seconds:.10f}",
        "cpu_seconds": f"{cpu_seconds:.10f}",
        "max_rss_kb": max(rss_before, rss_after),
        "complexity": complexity,
        "loss": loss,
        "equation": equation,
        "equation_file": str(equation_file),
    }


def run_pysr(args: argparse.Namespace, output_dir: Path, entries: list[tuple[str, Path]]) -> dict[str, Any]:
    PySRRegressor, error = maybe_import_pysr(Path(args.pysr_repo))
    if PySRRegressor is None:
        return {
            "status": "unavailable",
            "error": error,
            "hint": (
                "Activate a venv with pysr, juliacall, and Julia available, "
                "or point --pysr-repo at a checkout usable from that venv."
            ),
        }

    rows: list[dict[str, Any]] = []
    failures: list[dict[str, str]] = []
    for dataset_name, dataset_path in entries:
        try:
            rows.append(fit_one_pysr(PySRRegressor, dataset_name, dataset_path, args, output_dir))
        except Exception as exc:  # pragma: no cover - runtime dependent
            failures.append({"dataset": dataset_name, "error": f"{type(exc).__name__}: {exc}"})

    tsv_path = output_dir / "pysr_results.tsv"
    fieldnames = [
        "dataset",
        "vars",
        "rows",
        "r2",
        "wall_seconds",
        "cpu_seconds",
        "max_rss_kb",
        "complexity",
        "loss",
        "equation",
        "equation_file",
    ]
    write_tsv(tsv_path, rows, fieldnames)
    return {
        "status": "ok" if not failures else "partial",
        "tsv_path": str(tsv_path),
        "rows": rows,
        "failures": failures,
    }


def summarise_r2(rows: list[dict[str, Any]], key: str) -> tuple[float, int, int]:
    values = [float(row[key]) for row in rows if row.get(key) not in ("", None)]
    if not values:
        return 0.0, 0, 0
    return (
        sum(values) / len(values),
        sum(value >= 0.99 for value in values),
        sum(value >= 0.90 for value in values),
    )


def merge_rows(
    haskell_rows: list[dict[str, Any]] | None,
    pysr_rows: list[dict[str, Any]] | None,
) -> list[dict[str, Any]]:
    by_name: dict[str, dict[str, Any]] = {}
    for row in haskell_rows or []:
        name = row["dataset"]
        by_name.setdefault(name, {"dataset": name})
        by_name[name]["haskell_r2"] = row["r2"]
        by_name[name]["haskell_time"] = row["time_seconds"]
        by_name[name]["haskell_formula"] = row["formula"]
    for row in pysr_rows or []:
        name = row["dataset"]
        by_name.setdefault(name, {"dataset": name})
        by_name[name]["pysr_r2"] = row["r2"]
        by_name[name]["pysr_time"] = row["wall_seconds"]
        by_name[name]["pysr_formula"] = row["equation"]
    merged = []
    for name in sorted(by_name):
        row = by_name[name]
        hr2 = row.get("haskell_r2")
        pr2 = row.get("pysr_r2")
        if hr2 is not None and pr2 is not None:
            row["delta_r2"] = f"{float(hr2) - float(pr2):.10f}"
        else:
            row["delta_r2"] = ""
        merged.append(row)
    return merged


def write_markdown_summary(
    path: Path,
    args: argparse.Namespace,
    venv: str,
    haskell: dict[str, Any] | None,
    pysr: dict[str, Any] | None,
    merged: list[dict[str, Any]],
) -> None:
    lines: list[str] = []
    lines.append("# Feynman Profile Comparison")
    lines.append("")
    lines.append(f"- Dataset root: `{args.feynman_path}`")
    lines.append(f"- Python virtualenv: `{venv}`")
    lines.append(f"- Rows per dataset: `{args.rows}`")
    lines.append(f"- Seed: `{args.seed}`")
    lines.append("")

    if haskell:
        lines.append("## Haskell")
        lines.append("")
        lines.append(f"- Status: `{haskell['status']}`")
        if haskell.get("command"):
            lines.append(f"- Command: `{haskell['command']}`")
        if haskell.get("wall_seconds") is not None:
            lines.append(f"- Wall time: `{haskell['wall_seconds']:.2f}s`")
        if haskell.get("max_rss_kb") is not None:
            lines.append(f"- Max RSS: `{haskell['max_rss_kb']} KB`")
        if haskell["status"] == "ok":
            mean_r2, count_99, count_90 = summarise_r2(haskell["rows"], "r2")
            lines.append(f"- Mean R2: `{mean_r2:.4f}`")
            lines.append(f"- R2 >= 0.99: `{count_99}`")
            lines.append(f"- R2 >= 0.90: `{count_90}`")
        if haskell.get("error"):
            lines.append(f"- Error: `{haskell['error']}`")
        lines.append("")

    if pysr:
        lines.append("## PySR")
        lines.append("")
        lines.append(f"- Status: `{pysr['status']}`")
        if pysr.get("tsv_path"):
            lines.append(f"- Results TSV: `{pysr['tsv_path']}`")
        if pysr["status"] in {"ok", "partial"}:
            mean_r2, count_99, count_90 = summarise_r2(pysr["rows"], "r2")
            lines.append(f"- Mean R2: `{mean_r2:.4f}`")
            lines.append(f"- R2 >= 0.99: `{count_99}`")
            lines.append(f"- R2 >= 0.90: `{count_90}`")
        if pysr.get("error"):
            lines.append(f"- Error: `{pysr['error']}`")
        if pysr.get("hint"):
            lines.append(f"- Hint: {pysr['hint']}")
        lines.append("")

    if merged:
        lines.append("## Per-dataset")
        lines.append("")
        lines.append("| Dataset | Haskell R2 | PySR R2 | Delta R2 | Haskell Time (s) | PySR Time (s) |")
        lines.append("|---|---:|---:|---:|---:|---:|")
        for row in merged:
            lines.append(
                f"| {row['dataset']} | "
                f"{row.get('haskell_r2', '')} | "
                f"{row.get('pysr_r2', '')} | "
                f"{row.get('delta_r2', '')} | "
                f"{row.get('haskell_time', '')} | "
                f"{row.get('pysr_time', '')} |"
            )
        lines.append("")

    path.write_text("\n".join(lines) + "\n")


def main() -> None:
    args = parse_args()
    venv = ensure_venv()
    repo_root = Path(__file__).resolve().parents[1]
    output_dir = repo_root / args.output_dir
    mkdir(output_dir)

    entries = dataset_entries(Path(args.feynman_path), args.datasets)
    if not entries:
        raise SystemExit("No matching datasets found.")

    haskell_result: dict[str, Any] | None = None
    if not args.skip_haskell:
        haskell_result = run_haskell_bench(args, output_dir, repo_root)

    if args.haskell_profile_dataset:
        lookup = {name: path for name, path in entries}
        dataset_path = lookup.get(args.haskell_profile_dataset)
        if dataset_path is None:
            raise SystemExit(
                f"--haskell-profile-dataset {args.haskell_profile_dataset!r} was not found "
                "in the selected dataset set."
            )
        profile_result = run_haskell_profile(args, output_dir, repo_root, dataset_path)
        (output_dir / "haskell_profile_summary.txt").write_text(str(profile_result) + "\n")

    pysr_result: dict[str, Any] | None = None
    if not args.skip_pysr:
        pysr_result = run_pysr(args, output_dir, entries)

    merged = merge_rows(
        haskell_result.get("rows") if haskell_result and haskell_result.get("rows") else None,
        pysr_result.get("rows") if pysr_result and pysr_result.get("rows") else None,
    )
    if merged:
        fieldnames = [
            "dataset",
            "haskell_r2",
            "pysr_r2",
            "delta_r2",
            "haskell_time",
            "pysr_time",
            "haskell_formula",
            "pysr_formula",
        ]
        write_tsv(output_dir / "comparison.tsv", merged, fieldnames)

    write_markdown_summary(
        output_dir / "comparison.md",
        args,
        venv,
        haskell_result,
        pysr_result,
        merged,
    )


if __name__ == "__main__":
    main()
