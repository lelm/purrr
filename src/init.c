#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP coerce_impl(SEXP, SEXP);
extern SEXP pluck_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP flatten_impl(SEXP);
extern SEXP map_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP map2_impl(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP pmap_impl(SEXP, SEXP, SEXP, SEXP);
extern SEXP slide_impl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP slide_impl_overwrite_window(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP transpose_impl(SEXP, SEXP);
extern SEXP vflatten_impl(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"coerce_impl",    (DL_FUNC) &coerce_impl,    2},
    {"pluck_impl",   (DL_FUNC) &pluck_impl,   4},
    {"flatten_impl",   (DL_FUNC) &flatten_impl,   1},
    {"map_impl",       (DL_FUNC) &map_impl,       4},
    {"map2_impl",      (DL_FUNC) &map2_impl,      5},
    {"pmap_impl",      (DL_FUNC) &pmap_impl,      4},
    {"slide_impl",     (DL_FUNC) &slide_impl,     7},
    {"transpose_impl", (DL_FUNC) &transpose_impl, 2},
    {"vflatten_impl",  (DL_FUNC) &vflatten_impl,  2},
    {"slide_impl_overwrite_window", (DL_FUNC) &slide_impl_overwrite_window, 7},
    {NULL, NULL, 0}
};

void R_init_purrr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
