#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
VENV_PATH="${1:-$REPO_ROOT/.venv}"

if [[ ! -d "$VENV_PATH" ]]; then
  echo "Virtual environment not found: $VENV_PATH" >&2
  return 1 2>/dev/null || exit 1
fi

# shellcheck disable=SC1090
source "$VENV_PATH/bin/activate"

export JULIA_DEPOT_PATH="${JULIA_DEPOT_PATH:-$REPO_ROOT/.venv/julia_depot}"
export CABAL_DIR="${CABAL_DIR:-$REPO_ROOT/.cabal}"

mkdir -p "$JULIA_DEPOT_PATH" "$CABAL_DIR"

echo "Activated virtualenv: $VENV_PATH"
echo "JULIA_DEPOT_PATH=$JULIA_DEPOT_PATH"
echo "CABAL_DIR=$CABAL_DIR"
