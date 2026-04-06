#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

usage() {
  echo "Usage: $0 --venv /path/to/venv [profile_feynman.py args...]"
}

if [[ $# -lt 2 ]]; then
  usage
  exit 1
fi

if [[ "$1" != "--venv" ]]; then
  usage
  exit 1
fi

VENV_PATH="$2"
shift 2

if [[ ! -d "$VENV_PATH" ]]; then
  echo "Virtual environment not found: $VENV_PATH" >&2
  exit 1
fi

# shellcheck disable=SC1090
source "$VENV_PATH/bin/activate"

export JULIA_DEPOT_PATH="${JULIA_DEPOT_PATH:-$REPO_ROOT/.venv/julia_depot}"
mkdir -p "$JULIA_DEPOT_PATH"

if [[ -z "${CABAL_DIR:-}" ]]; then
  export CABAL_DIR="$REPO_ROOT/.cabal"
fi
mkdir -p "$CABAL_DIR"

cd "$REPO_ROOT"
exec python scripts/profile_feynman.py "$@"
