#!/usr/bin/env bash
# Build GMP + MPFR + FLINT for wasm32-wasi, with the wasi-sdk that ghc-wasm-meta
# installs, so they can be linked straight into the GHC reactor module.
#
# This is not the CriticalValues build (wasm/build-flint-wasm.sh there): that
# one uses Emscripten and lives in its own module behind a string boundary,
# which suits a program that calls FLINT once per request. This program calls
# FLINT on every step, so FLINT has to be in the same module as the Haskell —
# same ABI, same linear memory — and that means the same compiler GHC's wasm
# backend links with.
set -e
GMP_VERSION=6.3.0
MPFR_VERSION=4.2.2
FLINT_VERSION=3.4.0

HERE="$(cd "$(dirname "$0")" && pwd)"
SDK="$HOME/.ghc-wasm/wasi-sdk"
export CC="$SDK/bin/wasm32-wasi-clang" CXX="$SDK/bin/wasm32-wasi-clang++"
export AR="$SDK/bin/llvm-ar" RANLIB="$SDK/bin/llvm-ranlib" NM="$SDK/bin/llvm-nm" LD="$SDK/bin/wasm-ld" STRIP="$SDK/bin/llvm-strip"
# WASI has no signals, process clocks or getpid; GMP raises SIGFPE on division
# by zero and FLINT's profiler asks for the clock. wasi-libc emulates all three
# behind these macros and libraries; the GHC link line repeats the -l flags.
export CFLAGS="-O2 -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_GETPID"
export LDFLAGS="-lwasi-emulated-signal -lwasi-emulated-process-clocks -lwasi-emulated-getpid"
# GMP's configure refuses a build directory with a space or apostrophe in it.
BUILD="${FUNDOM_WASM_BUILD:-$HOME/.cache/fundom-wasm}"
PREFIX="$BUILD/local"
CACHE="$HOME/.cache/crit-wasm"        # the tarballs are already there
BUILDHOST="$(uname -m | sed 's/arm64/aarch64/')-apple-darwin"
mkdir -p "$BUILD" "$PREFIX"
cd "$BUILD"

fetch() { [ -f "$2" ] || { [ -f "$CACHE/$2" ] && cp "$CACHE/$2" "$2"; } || curl -sSL "$1" -o "$2"; }

# --- GMP -------------------------------------------------------------------
if [ ! -f "$PREFIX/lib/libgmp.a" ]; then
  fetch "https://ftp.gnu.org/gnu/gmp/gmp-$GMP_VERSION.tar.xz" gmp.tar.xz
  rm -rf "gmp-$GMP_VERSION"; tar xf gmp.tar.xz
  cd "gmp-$GMP_VERSION"
  ./configure --host=wasm32-wasi --build="$BUILDHOST" --disable-assembly --disable-shared \
      --enable-static --prefix="$PREFIX" CC_FOR_BUILD=/usr/bin/clang HOST_CC=/usr/bin/clang
  make -j8
  make install
  cd "$BUILD"
fi

# --- MPFR ------------------------------------------------------------------
if [ ! -f "$PREFIX/lib/libmpfr.a" ]; then
  fetch "https://www.mpfr.org/mpfr-$MPFR_VERSION/mpfr-$MPFR_VERSION.tar.xz" mpfr.tar.xz
  rm -rf "mpfr-$MPFR_VERSION"; tar xf mpfr.tar.xz
  cd "mpfr-$MPFR_VERSION"
  ./configure --host=wasm32-wasi --build="$BUILDHOST" --with-gmp="$PREFIX" --disable-shared \
      --enable-static --prefix="$PREFIX"
  make -j8
  make install
  cd "$BUILD"
fi

# --- FLINT -----------------------------------------------------------------
if [ ! -f "$PREFIX/lib/libflint.a" ]; then
  fetch "https://github.com/flintlib/flint/releases/download/v$FLINT_VERSION/flint-$FLINT_VERSION.tar.gz" flint.tar.gz
  rm -rf "flint-$FLINT_VERSION"; tar xf flint.tar.gz
  cd "flint-$FLINT_VERSION"
  # No threads in a wasm32-wasi module (GHC's RTS is single-threaded there),
  # no assembly, and the getrusage-based profiler off, as in the Emscripten build.
  sed -i '' \
    's|^#if (defined(__unix__) \&\& !defined(__CYGWIN__)) \|\| defined(__APPLE__)$|#if ((defined(__unix__) \&\& !defined(__CYGWIN__)) \|\| defined(__APPLE__)) \&\& !defined(__wasi__)|' \
    src/profiler.h
  # wasm rounds to nearest only, and wasi-libc's fenv.h has no FE_DOWNWARD or
  # FE_UPWARD; fmpz_lll's floating-point LLL certification switches rounding
  # modes. Give it the names, as the only mode there is. That module is not
  # used here; the arithmetic this program calls is unaffected.
  ./configure --host=wasm32-wasi --build="$BUILDHOST" --disable-assembly --disable-pthread --disable-thread-safe \
      --disable-shared --enable-static --with-gmp="$PREFIX" --with-mpfr="$PREFIX" --prefix="$PREFIX" \
      CFLAGS="$CFLAGS -DFE_DOWNWARD=FE_TONEAREST -DFE_UPWARD=FE_TONEAREST -DFE_TOWARDZERO=FE_TONEAREST -include $HERE/wasi-compat.h"
  make -j8
  make install
  cd "$BUILD"
fi
echo "built $PREFIX/lib/{libgmp,libmpfr,libflint}.a for wasm32-wasi"
