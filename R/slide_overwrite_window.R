#' Apply a function to a sliding window of a vector
#'
#' @description
#'
#' `slide_overwrite_window` is an alternative implementation of `slide` that
#' allocates a vector for the rolling window only once and overwrites its values
#' in each iteration.
#'
#' This leads to an unexpected behaviour in (the unusual) case the
#' vector passed to .f is assigned during the call. For example:
#' `slide_overwrite_window(1:3, identity, 2)` returns `list(NULL, 2:3, 2:3)`
#' instead of `list(NULL, 1:2, 2:3)`.
#'
#' Reference counting would allow to detect whether the vector is referenced
#' after calling .f and allocate a new vector in this case.
#' However, while proposed in 2014, reference counting for R is not implemented
#' (yet). See notes on reference counting by Luke Tierney:
#' \url{https://developer.r-project.org/Refcnt.html}.

#' @inheritParams as_mapper
#' @param .x A list or atomic vector.
#' @param .n A positive integer; the length of the sliding window. Needs to be
#'   an odd number for centered alignment.
#' @param .align One of `"left"` (the default), `"center"` or `"right"`;
#'   Assign the result of function calls to the position at the left, center
#'   or right of the sliding window. Ignored if `fill = NULL`.
#' @param .fill length-one vector or `NULL`; value assigned to the result where
#'   the sliding window exceeds the first or last element of `.x`. `NULL` means
#'   no filling.
#' @param ... Additional arguments passed on to the mapped function.
#' @export
#' @family roll variants
#' @examples
#' # different than expected return value: list(NULL, 1:2, 2:3)
#' slide_overwrite_window(1:3, identity, 2)
#'
#' a <- list()
#' slide_overwrite_window_dbl(1:3, function(x) {a <<- c(a, list(x)); sum(x)}, 2)
#' str(a)
#' # differs from expected return value: list(NULL, 1:2, 2:3)

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window <- function(.x, .f, .n, .align = "right", .fill = list(NULL), ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "list")
}

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window_lgl <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "logical")
}

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window_chr <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "character")
}

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window_int <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "integer")
}

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window_dbl <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "double")
}

#' @rdname slide_overwrite_window
#' @export
slide_overwrite_window_raw <- function(.x, .f, .n, .align = "right", .fill = as.raw(0), ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl_overwrite_window, environment(), ".x", ".f", .n, align, .fill, "raw")
}
