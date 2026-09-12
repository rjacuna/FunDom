/* Force-included when compiling FLINT for wasm32-wasi. wasi-libc has no
 * temporary files; FLINT's quadratic sieve wants one for its relations.
 * The sieve is never called here, so the call resolves to a stub that fails. */
int fundom_mkstemp_stub(char *tmpl);
#define mkstemp fundom_mkstemp_stub
