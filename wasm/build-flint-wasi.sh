#!/usr/bin/env bash
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
FLINTELL="${FLINTELL_ROOT:-$HERE/../../flintell}"
exec "$FLINTELL/scripts/build-deps.sh" wasi
