#define R_NO_REMAP
#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include "coerce.h"
#include "conditions.h"
#include "utils.h"

// Set elements of vector x between i and j-1 to skalar `fill'
void fill_range(SEXP x, SEXP fill, int i, int j) {
  switch(TYPEOF(x)) {
  case LGLSXP:
    {
      int fill_value = Rf_asLogical(fill);
      int *p_x;
      p_x = LOGICAL(x);
      for (; i < j; ++i) p_x[i] = fill_value;
    }
    break;
  case INTSXP:
    {
      int fill_value = Rf_asInteger(fill);
      int *p_x;
      p_x = INTEGER(x);
      for (; i < j; ++i) p_x[i] = fill_value;
    }
    break;
  case REALSXP:
    {
      double fill_value = Rf_asReal(fill);
      double *p_x;
      p_x = REAL(x);
      for (; i < j; ++i) p_x[i] = fill_value;
    }
    break;
  case STRSXP:
    {
      SEXP fill_value = Rf_asChar(fill);
      for (; i < j; ++i) SET_STRING_ELT(x, i, fill_value);
    }
    break;
  case VECSXP:
    {
      SEXP fill_value = VECTOR_ELT(fill, 0);
      for (; i < j; ++i) SET_VECTOR_ELT(x, i, fill_value);
    }
    break;
  case RAWSXP:
    {
      Rbyte fill_value = RAW(fill)[0];
      Rbyte *p_x;
      p_x = RAW(x);
      for (; i < j; ++i) p_x[i] = fill_value;
    }
    break;
  default:
    Rf_error("Unknown type `%s' in `fill_range'.", Rf_type2char(TYPEOF(x)));
  }
}

// Copy k elements starting from position i in vector 'from' into a vector 'to'
// of same type
void copy_range(SEXP to, SEXP from, int i, int k) {
  // Discussion on efficient copying of a vector:
  // http://r.789695.n4.nabble.com/suggestion-how-to-use-memcpy-in-duplicate-c-td2019184.html
  if (TYPEOF(from) != TYPEOF(to)) {
    Rf_error("Types do not match: from %s to %s",
      Rf_type2char(TYPEOF(from)), Rf_type2char(TYPEOF(to)));
  }
  switch(TYPEOF(from)) {
  case LGLSXP:
    {
      int *p_from, *p_to;
      p_from = LOGICAL(from);
      p_to   = LOGICAL(to);
      for (int j = 0; j < k; j++) p_to[j] = p_from[i+j];
    }
    break;
  case INTSXP:
    {
      int *p_from, *p_to;
      p_from = INTEGER(from);
      p_to   = INTEGER(to);
      for (int j = 0; j < k; j++) p_to[j] = p_from[i+j];
    }
    break;
  case REALSXP:
    {
      double *p_from, *p_to;
      p_from = REAL(from);
      p_to   = REAL(to);
      for (int j = 0; j < k; j++) p_to[j] = p_from[i+j];
    }
    break;
  case STRSXP:
    for (int j = 0; j < k; j++) SET_STRING_ELT(to, j, STRING_ELT(from, i + j));
    break;
  case VECSXP:
    for (int j = 0; j < k; j++) SET_VECTOR_ELT(to, j, VECTOR_ELT(from, i + j));
    break;
  case RAWSXP:
    {
      Rbyte *p_from, *p_to;
      p_from = RAW(from);
      p_to   = RAW(to);
      for (int j = 0; j < k; j++) p_to[j] = p_from[i+j];
    }
    break;
  default:
    Rf_error("Unknown type `%s' in `copy_range'.", Rf_type2char(TYPEOF(from)));
  }
}

SEXP slide_impl(SEXP env, SEXP x_name_, SEXP f_name_, SEXP width_, SEXP align_, SEXP fill_, SEXP type_) {
  const char* x_name = CHAR(Rf_asChar(x_name_));
  const char* f_name = CHAR(Rf_asChar(f_name_));

  SEXP x = Rf_install(x_name);
  SEXP f = Rf_install(f_name);
  int width = Rf_asInteger(width_);
  int align = Rf_asInteger(align_);
  SEXPTYPE type = Rf_str2type(CHAR(Rf_asChar(type_)));
  if (fill_ != R_NilValue && Rf_length(fill_) != 1)
    Rf_error("`.fill' needs to be a vector of length one or NULL.");

  SEXP x_val = Rf_eval(x, env);
  check_vector(x_val, ".x");

  int n = Rf_length(x_val);
  if (n == 0) {
    SEXP out = PROTECT(Rf_allocVector(type, 0));
    if (fill_ != R_NilValue) {
      copy_names(x_val, out);
    }
    UNPROTECT(1);
    return out;
  }
  if (width < 1) {
    Rf_error(".n must be a positive intger.");
  }
  if (width > n) {
    SEXP out;
    if (fill_ == R_NilValue) {
      out = PROTECT(Rf_allocVector(type, 0));
    } else {
      out = PROTECT(Rf_allocVector(type, n));
      fill_range(out, fill_, 0, n);
      copy_names(x_val, out);
    }
    UNPROTECT(1);
    return out;
  }

  // offset between start of sliding window and position in resulting vector
  int offset;
  if (fill_ == R_NilValue) {
     offset = 0;
  } else {
    switch(align) {
    case 1: offset = 0; break; // LEFT align
    case 2:
      // CENTER align
      offset = width / 2;
      if (width % 2 == 0)
        Rf_warning(".align = 'center' with even numbered window size, aligning to the center left.");
      break;
    case 3: offset = width - 1; break; // RIGHT align
    default: Rf_error("Illegal value for _align.");
    }
  }

  // Construct a call f(x_window, ...) where x_window holds the portion of x in
  // the sliding window. x_window is populated with x[1:width], x[2:(width+1)],
  // etc. while iterating over the resulting vector.
  SEXP x_window = Rf_install("x_window");
  SEXP f_call = PROTECT(Rf_lang3(f, x_window, R_DotsSymbol));

  int out_length = (fill_ == R_NilValue) ? (n - width + 1) : n;
  SEXP out = PROTECT(Rf_allocVector(type, out_length));

  if (fill_ != R_NilValue) {
    // pad result to the left
    fill_range(out, fill_, 0, offset);
  }

  for (int i = 0; i <= n - width; ++i) {
    if (i % 1024 == 0)
      R_CheckUserInterrupt();

    // copy portion of x in the current position of the sliding window
    SEXP x_window_val = PROTECT(Rf_allocVector(TYPEOF(x_val), width));
    copy_range(x_window_val, x_val, i, width);
    Rf_defineVar(x_window, x_window_val, env);

    // out[i + offset] <- f(x_window, ...)
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 3)
    SEXP res = PROTECT(R_forceAndCall(f_call, 1, env));
#else
    SEXP res = PROTECT(Rf_eval(f_call, env));
#endif
    if (type != VECSXP && Rf_length(res) != 1) {
      SEXP ptype = PROTECT(Rf_allocVector(type, 0));
      stop_bad_element_vector(res, i + 1, ptype, 1, "Result", NULL, false);
    }
    set_vector_value(out, i + offset, res, 0);
    UNPROTECT(2);
  }

  if (fill_ != R_NilValue) {
    // pad result to the right
    fill_range(out, fill_, n - width + 1 + offset, n);
    copy_names(x_val, out);
  }

  UNPROTECT(2);

  return out;
}
