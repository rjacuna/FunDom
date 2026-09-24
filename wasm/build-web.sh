#!/usr/bin/env bash
# FunDom — Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
# Build the drawer as a wasm32-wasi reactor module with FLINT linked in.
#
#   ./wasm/build-flint-wasi.sh   # once: GMP, MPFR, FLINT for wasm32-wasi
#   ./wasm/build-web.sh          # the module, its JS glue, the tables and the pre-rendered pages
#   ./wasm/build-web.sh pages    # only the tables and the pages (needs the native binary: cabal build exe:fundom)
#   ./wasm/build-web.sh wasm     # only the module and its JS glue
#   python3 -m http.server -d web 8092
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$HERE")"
OUT="$ROOT/web"
BUILD="${FUNDOM_WASM_BUILD:-$HOME/.cache/fundom-wasm}"
PREFIX="$BUILD/local"
HS="$BUILD/hs"
mkdir -p "$HS" "$OUT"

if [ "${1:-}" != "pages" ]; then
. ~/.ghc-wasm/env

# the C shims, with the same compiler GHC links with
wasm32-wasi-clang -O2 -I"$PREFIX/include" -I"$ROOT/cbits" -D_WASI_EMULATED_SIGNAL \
  -c "$ROOT/cbits/fundom_shims.c" -o "$HS/fundom_shims.o"

# the Haskell, as a reactor: no main, exports via the JSFFI
wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor \
  -i"$ROOT/src" -i"$ROOT/app" -outputdir "$HS" \
  -optl-L"$PREFIX/lib" -optl-lflint -optl-lmpfr -optl-lgmp \
  -optl-lwasi-emulated-signal -optl-lwasi-emulated-process-clocks -optl-lwasi-emulated-getpid \
  "$ROOT/app/Web.hs" "$HS/fundom_shims.o" -o "$OUT/fundom.wasm"

# the JS side of the JSFFI (from the unoptimised module: post-link reads the
# JSFFI import/export tables, which wasm-opt then keeps)
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$OUT/fundom.wasm" -o "$OUT/fundom.js"

# 4.8 MB -> 3 MB; the feature flags match what GHC's wasm backend emits
wasm-opt -Os --enable-bulk-memory --enable-reference-types --enable-simd \
  --enable-nontrapping-float-to-int --enable-sign-ext --enable-mutable-globals \
  "$OUT/fundom.wasm" -o "$OUT/fundom.wasm"
fi

# The tables and the pre-rendered pages, from the native binary.  web/pre/
# holds the pages listed by `fundom prequeries` (the default page, the empty
# tables and generators pages, the worked examples, the families in all five
# views), which the app shows at once instead of computing them; index.html
# is the default page itself, with the module script and the list of those
# pages added.  The pages are rendered by $JOBS processes at once, each
# reading the tables a single time; then wasm/parts.py takes the two large
# layers out of every page into web/pre/parts/, one file per distinct layer.
[ "${1:-}" = "wasm" ] && exit 0
BIN="$(ls "$ROOT"/dist-newstyle/build/*/ghc-*/fundom-*/x/fundom/build/fundom/fundom 2>/dev/null | head -1)"
if [ -n "$BIN" ]; then
  JOBS="${FUNDOM_JOBS:-$(sysctl -n hw.ncpu 2>/dev/null || nproc)}"
  [ -d "$ROOT/csg" ] && (cd "$ROOT" && "$BIN" csg-export > "$OUT/csg.json")
  rm -rf "$OUT/pre"; mkdir -p "$OUT/pre"
  (cd "$ROOT" && "$BIN" prequeries) | awk '{ printf "pre/%d.html\t%s\n", NR - 1, $0 }' > "$OUT/pre/list.tsv"
  seq 0 $((JOBS - 1)) | (cd "$ROOT" && xargs -P "$JOBS" -I{} "$BIN" prerender "$OUT/pre" {} "$JOBS")
  (cd "$ROOT" && "$BIN" page '') | python3 "$HERE/shell.py" "$OUT/index.html" "$OUT/pre/list.tsv"
  python3 "$HERE/parts.py" "$OUT/pre/parts" "$OUT"/pre/*.html "$OUT/index.html"
fi
ls -la "$OUT" "$OUT/pre"
