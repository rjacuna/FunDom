# Fundamental domains, on FLINT

A port of **Helena A. Verrill's Fundamental Domain Drawer** — the Java
applet in `../` (`FunDomain.java`), implementing her *Algorithm for Drawing
Fundamental Domains* (January 2001) — to Haskell, with the numerics in FLINT
3.4, a server-side HTML UI, and the same library compiled to WebAssembly for
<https://rjacuna.github.io/FunDom/>. Same architecture as CriticalValues: a
pure library with the mathematics, a thin native server, and a `wasm32-wasi`
reactor export — here with FLINT linked into the module.

## Licence

GPL-3.0-or-later (`LICENSE`). Mode 1 is a translation of Helena A. Verrill's
`FunDomain`, which she published under the GNU GPL, version 2 or later
(Copyright (C) 2001 Helena A. Verrill); her original nine source files, her
README and her copy of the licence are in `java/`, unchanged. The "or
later" in her licence is what allows this derivative to be GPL-3.0-or-later;
FLINT is LGPL-3.0-or-later.

## Attribution

Mode 1 is Verrill's applet: the five group types and their intersections,
the breadth-first walk over `T, T⁻¹, S` that lays out one triangle per coset,
the table of side pairings and the reading of cusps, elliptic points and
genus from it, the edit mode (redraw a paired triangle across a side) and
the link mode (curves between paired sides), the triangle explorer with its
`MT, MS, TM, …` buttons, and the arrangement of the controls. Her published
source (`java/`, from the copy at wstein.org/Tables/fundomain/) carries the
copyright and licence notice; the merged single-file copy the port was read
from had lost it.

What is new here is the implementation and the rest: FLINT for the
numerics, exact arcs instead of sampled polygons, moves that keep the
domain connected, the disk view and its tilings, the Cummins–Pauli tables
(mode 2), and the enumeration from generators with Hsu's congruence test
(mode 3).

Other sources: C. Cummins and S. Pauli, *Congruence subgroups of PSL(2,Z)
of genus less than or equal to 24*, Experiment. Math. 12 (2003), and their
tables; T. Hsu, *Identifying congruence subgroups of the modular group*,
Proc. AMS 124 (1996); the transcription of Hsu's relations follows Sage's
`is_congruence` (C. Kurth's KFarey).

```sh
brew install flint                        # FLINT 3.4 (GMP, MPFR); the tables go in csg/
export PATH="$HOME/.ghcup/bin:$PATH" CC=/usr/bin/clang PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig
cabal build all && cabal test
cabal run fundom -- info G0 11            # index 12, genus 1, cusps ∞·1 0·11
cabal run fundom -- db 11A1               # a Cummins–Pauli group, checked against their table
cabal run fundom -- level 11              # every group of level 11 in the tables
cabal run fundom -- gens '1 1 0 1; 1 0 2 1' # ⟨T, [1 0; 2 1]⟩: index 3, level 2, congruence
cabal run fundom-server -- 8091 csg       # http://localhost:8091/?g1=G0&n=11&links=1
```

Types on the command line and in URLs: `G0` Γ₀, `G1` Γ₁, `Gu0` Γ⁰, `Gu1` Γ¹,
`G` Γ. Two groups intersect: `?g1=Gu0&n=6&g2=G1&m=4`. A Cummins–Pauli name
overrides both: `?db=11A1`. The disk is the view it opens in; `?view=uhp` is
the upper half-plane.

## What it does

Three modes, on a slider at the top of the left panel; the half-plane/disk
toggle and everything under "Show" apply to all three.

1. **Families.** Pick Γ_x(N) ∩ Γ_y(M) from Γ₀, Γ₁, Γ⁰, Γ¹, Γ.
2. **Tables.** Browse Cummins–Pauli by genus, then level, then index; the
   groups in that class are listed on the right (`11B1`, `11C1`, …), and
   choosing one draws it and checks the computed invariants against the
   record. Or type a name.
3. **Generators.** Type integer matrices, one per line. The subgroup they
   generate is enumerated — if its index is finite — Hsu's criterion says
   whether it is a congruence subgroup, and the domain is drawn either way,
   with the invariants of the coset action checked against those of the
   domain. `?app=3&gens=…`.

In every mode the page draws a fundamental domain — one translate
`g·F` of the standard triangle per coset of Γ in PSL₂(ℤ) — and reports the
projective index, the genus, the cusps with their widths, and the elliptic
points. Click a triangle for its matrix and which triangle each of its three
sides is identified with. "Side pairings" draws a curve between paired sides
that are not adjacent. "Edit" puts a marker on every such side; clicking one
redraws the paired triangle across that side — together with everything that
was attached to the picture only through it, moved rigidly by the same
element of PSL₂(ℤ), so the domain stays one region. (The applet moved the
one triangle alone, which cuts loose whatever hung off it: one click on
Γ₀(11) left it in two pieces touching at the cusp 0.) A side paired with
itself — an elliptic point of order two, or the two sides of a width-one
cusp — gets a grey marker and cannot be moved. "Triangle explorer" draws
`M·F` for a typed-in matrix and its neighbours under `T, T⁻¹, S, R`, on the
left or the right.

**The disk**, which is what opens. It draws the domain under the Cayley
transform `z ↦ (z − i)/(z + i)`, behind it the classic two-coloured (2,3,∞)
tessellation of the disk — the images of the half-triangle `{i, ρ, ∞}`
under PSL₂(ℤ) in one colour over a disk of the other — and, with "tile by
Γ", the disk filled by Γ-translates of the domain: words in the side
pairings (which generate Γ), coloured by the parity of the word length, with
every translate's boundary drawn through. Both are on to begin with, so the
first thing one sees is Γ tiling the disk over the modular tessellation.
Colours are `c1`/`c2` for the tessellation and `fill`/`fill2` for the
translates, banana and chocolate to begin with, with `outline` for the sides;
`view=uhp` goes back to the half-plane, `tile=0` and `bg=0` turn the two
tilings off.

Each view keeps its own settings under **View**, and only while it is the one
shown: the half-plane its scale, centre and the domain's two colours, the
disk its two tilings and their colours. **Group** is the group alone. The cusps sit on the
circle, so nothing is truncated.

Everything is in the URL. There is no JavaScript: every control is a link
or a GET form, so a picture is a permalink and the page is a pure function
`Params → HTML` (`Render.Page.renderPage`). `/svg?…` is the picture alone.

## Where FLINT is

| what | FLINT type | module |
|---|---|---|
| elements of PSL₂(ℤ) | `psl2z_t` — matrices in a normal form, so projective equality is `==` | `Flint.SL2` |
| cusps, ideal endpoints, arc centres and radii | `fmpq_t`, exact | `Flint.SL2.act`, `Modular.Geometry` |
| the vertices `g(ρ)`, `g(ρ+1)` and the side markers | `acb_modular_transform` into `acb_t` balls, projected to the screen in `arb` at a precision chosen from the zoom; in the disk, the Cayley transform in `acb` too | `Flint.Ball` |
| a group from the tables | its coset names are minima over `H = ⟨generators, −I⟩ ≤ SL₂(ℤ/N)`, closed under multiplication mod N | `Modular.Group.fromMatrices` |
| units of ℤ/N for canonical coset keys | `n_gcd` | `Modular.Group` |
| integer and rational matrices: `mul`, `det`, `inv`, `solve`, `rref`, `nullspace` | `fmpz_mat_t`, `fmpq_mat_t` | `Flint.Mat` |

The Java did all of this in `int` and `double`, and stored polygons in
`int` screen coordinates — which is why its zoom stopped at 2·10⁸ and its
arcs had to be sampled into 101 points on a logarithmic table. Here the sides
of a triangle are the geodesics through `g(−½), g(½), g(∞), g(−1), g(1)`,
all rational or ∞, so every side is an exact SVG arc or line with a rational
centre and radius; only the two vertices are approximate, and those are balls
at whatever precision the zoom needs.

The binding is `Flint.FFI`, about a hundred lines: opaque pointers, every
object allocated and freed by the C shims in `cbits/fundom_shims.c`, and
plain `ccall`s to documented FLINT functions. No struct is ever read from
Haskell — sizes come from `sizeof` in C and fields from FLINT's accessors —
so it is right for FLINT 3 (whose `fmpz_mat_struct` swapped `rows` for
`stride`, the change that broke the Hackage `Flint2` binding's matrix layer)
and it cross-compiles. This project started on the CriticalValues fork of
`Flint2`, patched for FLINT 3.4; it was replaced by `Flint.FFI` when the
browser build needed a binding with no `hsc2hs` step.

`Flint.Mat` is the "fast matrix solvers" layer. Today the drawer uses it
only to cross-check the `psl2z` path (`test/Spec.hs`); it is there for the
modular symbols that are the natural next step.

## Mode 3: from generators to a certificate

`Modular.Word` writes a matrix as a reduced word in `S` and `R = S·T` —
PSL₂(ℤ) is the free product `⟨S⟩ * ⟨R⟩` of a C₂ and a C₃ — by Euclid on
the first column. `Modular.Coset` traces each generator as a loop at the
base coset (Todd–Coxeter with subgroup generators), propagates the
identifications, and closes every R-chain `v → w → x` with `x → v`. In a
free product of finite groups nothing else is ever needed: when the table
stops changing it is either complete — every coset has an S-neighbour and
an R-image, and it is then the coset table of the subgroup — or some coset
lacks one, and the Schreier graph continues from it into an infinite
hanging tree, so the index is infinite.

With the action in hand, `Modular.Congruence` applies Hsu's criterion
(Proc. AMS 124, 1996): let `N` be the lcm of the cusp widths (the level, if
the group is congruence, by Wohlfahrt), and test a presentation of
PSL₂(ℤ/N) — one relation for `N` odd, three for `N` a power of two, seven
otherwise — on the permutations of `L = [1 1; 0 1]` and `R = [1 0; 1 1]`.
The relations are transcribed from Sage's `is_congruence`. `bruteForce` is
the definition itself (the orbit of (base coset, I mod N) has size
`[PSL₂(ℤ) : Γ ∩ Γ(N)]`, which is `|PSL₂(ℤ/N)|` iff `Γ ⊇ Γ(N)`), and the
test-suite checks the criterion against it on some 1,600 random coset
actions of small level, a fifth of them non-congruence.

A group given this way is drawn from its coset action alone — the name of
the coset of `g` is the image of the base coset under the word of `g` — so
non-congruence subgroups get domains too.

## The mathematics, briefly

Right cosets `Γ·g` are named by a residue of `g` mod N (`Modular.Group`):
the point `(c : d)` of P¹(ℤ/N) for Γ₀(N), the bottom row up to sign for
Γ₁(N), the top row for Γ⁰ and Γ¹, the whole matrix up to sign for Γ(N); an
intersection is named by the pair. The enumeration (`Modular.Domain`) is the
applet's: breadth-first from the identity, right-multiplying by `T, T⁻¹, S`
in that order, each new triangle placed against the one that found it. That
walk fills a table `(triangle, side) ↦ (paired triangle, glued?)`. Cusps are
the orbits of `T` on it and their widths the orbit sizes; `e₂` counts sides
paired with themselves under `S`, `e₃` the triangles fixed by `T·S`; the
genus is `1 + (μ − 3e₂ − 4e₃ − 6c)/12`.

The index is the projective one, `[PSL₂(ℤ) : ±Γ]`, as in the applet; the
page says so, with the SL₂ index, when −I ∉ Γ.

`cabal test` checks all of that against the closed formulas for Γ₀(N) to
N = 60, Γ₁(N) to 30, Γ(N) to 13 (index, cusps, e₂, e₃, genus), the classical
genera of X₀(11), X₀(37), X₁(13), X(7), X(11), intersections against the
groups they equal, the consistency and connectedness of the gluing table
before and after edits, every Cummins–Pauli group of levels 6, 7, 11, 12
against its record, the disk geometry, the two FLINT matrix paths against
each other, the URL round trip, and mode 3: words round-trip through
matrices, every classical group and table group re-enumerates from its
side pairings to the right index and level, Hsu's criterion agrees with
the brute-force definition on random actions, and Schreier generators of
a random action re-enumerate to the same action. 469 checks.

## Reading the Java

Three things in `FunDomain.java` are not what they look like, and the port
does not reproduce them:

* `RepList.grouptype` is never assigned, so the branch that conjugates by
  `S` and the whole Γ(N) special case in `makeup` are dead; every group goes
  through the same walk.
* `findnextdirections` writes to its `int` parameters, which Java passes by
  value, so the "attach to the nearest triangle" heuristic always attaches to
  the discovering triangle. The effective algorithm is plain breadth-first
  search, which is what is implemented here on purpose.
* The `rows` field the Hackage `Flint2` binding peeks does not exist in
  FLINT 3. Nothing here peeks a struct.

## Layout

```
src/Flint/FFI.hs        the FLINT binding: opaque pointers and ccalls over cbits/
src/Flint/Z.hs          Integer/Rational ⇄ fmpz/fmpq marshalling
src/Flint/SL2.hs        PSL₂(ℤ) on psl2z_t; the Möbius action on P¹(ℚ)
src/Flint/Ball.hs       acb_modular_transform, and the only place a double is made
src/Flint/Mat.hs        fmpz_mat / fmpq_mat, typed for FLINT 3
src/Modular/Group.hs    the five group types and canonical coset keys
src/Modular/Domain.hs   the enumeration, the gluing table, cusps, genus, edits
src/Modular/Geometry.hs triangles as exact arcs
src/Modular/Disk.hs     the same in the disk; the tessellation; Γ-translates
src/Modular/CP.hs       the Cummins–Pauli tables
src/Modular/Word.hs     matrices ⇄ reduced words in S, R
src/Modular/Coset.hs    coset enumeration from generators; the coset action
src/Modular/Congruence.hs  Hsu's criterion, and the brute-force definition
src/Modular/Generators.hs  mode 3: parse, enumerate, certify, package
src/Render/Svg.hs       the picture
src/Render/Page.hs      the page
src/Web/Params.hs       the URL
src/Web/Context.hs      what the page is rendered from
src/Web/Group.hs        URL → context; the only IO before the picture (table lookups)
app/Main.hs             fundom: info | table | db | level | gens | svg | page
app/Server.hs           fundom-server: warp, no state
test/Spec.hs
cbits/fundom_shims.c    allocation, psl2z normal form and entries, matrix entry access; the wasi stubs
wasm/                   build-flint-wasi.sh, build-web.sh, wasi-compat.h
web/                    app.js, wasi-shim.js; index.html, the module and the tables are built
java/                   Helena A. Verrill's FunDomain, as she published it (GPL-2.0-or-later)
```

## `csg/` — the Cummins–Pauli database

`csg/` holds Cummins and Pauli's tables of all congruence subgroups of
PSL₂(ℤ) of genus ≤ 24 (8109 groups, up to conjugacy in PGL₂(ℤ)), in their
Magma format: `csg<genus>-lev<L>.dat` lists the groups of that genus with
level in `[L, L+8)`, each a `CongSubGrp` record with generators (as words in
`S`, `T` and as matrices mod the level), level, index, genus,
`contains_minus_one`, the cycle types of `T`, `S` and `R` on the cosets
(`cusps`, `c2`, `c3` — so the cusp widths and, by counting 1s, e₂ and e₃),
the name (`level`+`label`+`genus`, e.g. `11A1`) and the sub/supergroup
lattice. `csg.m`, `pre.m`, `func.m`, `table.m`, `html.m` are their Magma
code for building and querying the tables.

`Modular.CP` reads a record straight from the file its name points to
(Magma wraps at 80 columns, so whitespace is folded first), and
`Modular.Group.fromMatrices` turns the generators into a group: Γ is the
preimage of `H = ⟨generators, −I⟩` in SL₂(ℤ/N), a coset `Γ·g` is the coset
`H·(g mod N)`, and its name is the smallest matrix in it. That is a minimum
over `H` per coset — fine at the sizes in the tables (96A3, index 144,
level 96: 66 ms) — and the enumeration then runs unchanged. The page shows
the record's index, genus, cusp widths, e₂, e₃ next to the computed ones,
and `|H| · index = |SL₂(ℤ/N)|`. `cabal test` does this for every group of
levels 6, 7, 11 and 12.

The tables give one representative of each conjugacy class; their `11A1`
is a conjugate of Γ₀(11), and its domain is drawn for that conjugate.

## The browser build

The same library runs in the browser: <https://rjacuna.github.io/FunDom/>.
It is a `wasm32-wasi` reactor module with FLINT linked in — not the
CriticalValues arrangement of a separate Emscripten `flint.wasm` behind a
string boundary, which suits a program that calls FLINT once per request;
this one calls it on every step, so FLINT has to share the module's linear
memory. That is why the binding is `Flint.FFI`: opaque pointers, allocation
in C, no `hsc2hs`, so the identical source cross-compiles.

```sh
./wasm/build-flint-wasi.sh    # once: GMP 6.3.0, MPFR 4.2.2, FLINT 3.4.0 for wasm32-wasi, in ~/.cache/fundom-wasm
./wasm/build-web.sh           # web/fundom.wasm (3 MB after wasm-opt), web/fundom.js, web/csg.json
python3 -m http.server -d web 8092
```

`build-flint-wasi.sh` needs three accommodations for WASI, all recorded in
the script: wasi-libc's emulated signals, clocks and `getpid` (GMP raises
SIGFPE on division by zero); FLINT's LLL certification switches floating
rounding modes that wasm does not have (the constants are defined to
round-to-nearest, and that module is never called here); and FLINT's
quadratic sieve wants a temporary file (`mkstemp` is stubbed to fail, and
the sieve is never called either).

`index.html` is the default page — the full modular group — pre-rendered
by the native binary, picture included, so it is on screen before the
module has downloaded; `web/pre/` holds a few more pages rendered the same
way (`fundom prequeries` lists them: the empty tables and generators pages
and the worked examples), and the script shows those at once too. The
module is compiled in the background meanwhile, streamed as it downloads;
any other page waits for it, with a spinner over the plot while it works.

`app/Web.hs` exports `render`, `svg` and `identifyKey`, the first two taking the query
string and — for the tables — one record and the summaries of one genus
in the compact formats of `Modular.CP`. `web/app.js` owns the URL: it
intercepts the page's relative links and GET forms, calls the module, and
puts the HTML on the page with `history.pushState`, so every state is
still a shareable URL. The tables ship as `web/csg.json` (4.7 MB, 330 KB
gzipped), exported by `fundom csg-export` and fetched the first time mode 2
is used; JavaScript filters it and hands the module the pieces it needs.

The site is published from the `gh-pages` branch, which holds the contents
of `web/` after a build.
