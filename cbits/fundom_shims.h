#ifndef FUNDOM_SHIMS_H
#define FUNDOM_SHIMS_H
#include <flint/flint.h>
#include <flint/acb_modular.h>
#include <flint/fmpz_mat.h>
#include <flint/fmpq_mat.h>

void   fundom_psl2z_normal_form(psl2z_t g);

fmpz_mat_struct *fundom_fmpz_mat_new(slong r, slong c);
void             fundom_fmpz_mat_free(fmpz_mat_struct *m);
fmpz            *fundom_fmpz_mat_entry(const fmpz_mat_struct *m, slong i, slong j);

fmpq_mat_struct *fundom_fmpq_mat_new(slong r, slong c);
void             fundom_fmpq_mat_free(fmpq_mat_struct *m);
fmpq            *fundom_fmpq_mat_entry(const fmpq_mat_struct *m, slong i, slong j);


/* Allocation and access shims: every FLINT object this program touches is
 * made and freed here, so the Haskell side never knows a struct's size or
 * layout — which is also what makes the same code build for wasm32-wasi. */
#include <flint/fmpq.h>
#include <flint/arb.h>
#include <flint/acb.h>
fmpz         *fundom_fmpz_new(void);
void          fundom_fmpz_free(fmpz *x);
fmpq         *fundom_fmpq_new(void);
void          fundom_fmpq_free(fmpq *x);
arb_struct   *fundom_arb_new(void);
void          fundom_arb_free(arb_struct *x);
acb_struct   *fundom_acb_new(void);
void          fundom_acb_free(acb_struct *x);
psl2z_struct *fundom_psl2z_new(void);
void          fundom_psl2z_free(psl2z_struct *g);
fmpz         *fundom_psl2z_entry(psl2z_struct *g, int k);
arf_struct   *fundom_arb_midref(arb_struct *x);
int           fundom_arf_rnd_near(void);
#endif
