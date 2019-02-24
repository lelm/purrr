#ifndef SLIDE_H
#define SLIDE_H

extern "C" {
  SEXP slide_impl(SEXP env, SEXP x_name_, SEXP f_name_, SEXP width_, SEXP align_, SEXP fill_, SEXP type_);
}

#endif
