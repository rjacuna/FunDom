/* FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
 * Copyright (C) 2026 RJ Acuña. SPDX-License-Identifier: GPL-3.0-or-later */
/* Force-included when compiling FLINT for wasm32-wasi. wasi-libc has no
 * temporary files; FLINT's quadratic sieve wants one for its relations.
 * The sieve is never called here, so the call resolves to a stub that fails. */
int fundom_mkstemp_stub(char *tmpl);
#define mkstemp fundom_mkstemp_stub
