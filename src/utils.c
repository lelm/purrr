#define R_NO_REMAP
#include <Rinternals.h>
#include <stdbool.h>
#include "conditions.h"

SEXP sym_protect(SEXP x) {
  if (TYPEOF(x) == LANGSXP || TYPEOF(x) == SYMSXP) {
    SEXP quote_prim = Rf_eval(Rf_install("quote"), R_BaseEnv);
    return(Rf_lang2(quote_prim, x));
  } else {
    return x;
  }
}

// The return value might be garbage-collected so should be used in
// non-jumpy context
const char* friendly_typeof(SEXP x) {
  SEXP head = Rf_lang3(Rf_install(":::"), Rf_install("purrr"), Rf_install("friendly_type_of"));
  SEXP call = Rf_lang2(PROTECT(head), PROTECT(sym_protect(x)));

  PROTECT(call);
  SEXP type = Rf_eval(call, R_BaseEnv);
  UNPROTECT(2);

  return CHAR(STRING_ELT(type, 0));
}

bool is_vector(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return true;
  default:
    return false;
  }
}

SEXP list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x) {
  PROTECT(s);
  s = Rf_cons(s, Rf_list5(t, u, v, w, x));
  UNPROTECT(1);
  return s;
}

SEXP lang7(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x, SEXP y) {
  PROTECT(s);
  s = Rf_lcons(s, list6(t, u, v, w, x, y));
  UNPROTECT(1);
  return s;
}
SEXP list7(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x, SEXP y) {
  PROTECT(s);
  s = Rf_cons(s, list6(t, u, v, w, x, y));
  UNPROTECT(1);
  return s;
}
SEXP lang8(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x, SEXP y, SEXP z) {
  PROTECT(s);
  s = Rf_lcons(s, list7(t, u, v, w, x, y, z));
  UNPROTECT(1);
  return s;
}

void copy_names(SEXP from, SEXP to) {
  if (Rf_length(from) != Rf_length(to))
    return;

  SEXP names = Rf_getAttrib(from, R_NamesSymbol);
  if (Rf_isNull(names))
    return;

  Rf_setAttrib(to, R_NamesSymbol, names);
}

void check_vector(SEXP x, const char *name) {
  if (Rf_isNull(x) || Rf_isVector(x) || Rf_isPairList(x)) {
    return;
  }

  stop_bad_type(x, "a vector", NULL, name);
}