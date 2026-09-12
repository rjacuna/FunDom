#!/usr/bin/env bash
# FunDom — Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later
# Build the drawer as a wasm32-wasi reactor module with FLINT linked in.
#
#   ./wasm/build-flint-wasi.sh   # once: GMP, MPFR, FLINT for wasm32-wasi
#   ./wasm/build-web.sh          # the module, its JS glue, and the tables
#   python3 -m http.server -d web 8092
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$HERE")"
OUT="$ROOT/web"
BUILD="${FUNDOM_WASM_BUILD:-$HOME/.cache/fundom-wasm}"
PREFIX="$BUILD/local"
HS="$BUILD/hs"
. ~/.ghc-wasm/env
mkdir -p "$HS" "$OUT"

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

# the tables, and the page shell, from the native binary: index.html is the
# default page pre-rendered, with a spinner where the picture goes, so the
# controls are on screen while the module downloads
BIN="$(ls "$ROOT"/dist-newstyle/build/*/ghc-*/fundom-*/x/fundom/build/fundom/fundom 2>/dev/null | head -1)"
if [ -n "$BIN" ]; then
  [ -d "$ROOT/csg" ] && (cd "$ROOT" && "$BIN" csg-export > "$OUT/csg.json")
  (cd "$ROOT" && "$BIN" page '') | python3 "$HERE/shell.py" "$OUT/index.html"
fi
ls -la "$OUT"
