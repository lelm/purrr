#' Apply a function to a sliding window of a vector
#'
#' @description
#'
#' The slide functions apply a function .f over a sliding (or rolling) window of
#' a vector and (optionally) pad the result such that the length of the
#' resulting vector matches the input length.
#'
#' * `slide()` always returns a list.
#'
#' * `slide_lgl()`, `slide_int()`, `slide_dbl()`, `slide_chr()` and
#'    slide_raw()` each return an atomic vector of the indicated type
#'    (or die trying).
#'
#'    The return value of `.f` must be of length one for each window of
#'    `.x`. If `.f` uses an extractor function shortcut, `.default`
#'    can be specified to handle values that are absent or empty.  See
#'    [as_mapper()] for more on `.default`.
#'
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
#' @return All functions return a vector the same length as `.x` unless
#'   `.fill = NULL`.
#'
#'   `slide()` returns a list, `slide_lgl()` a logical vector, `slide_int()` an
#'   integer vector, `slide_dbl()` a double vector, `slide_chr()` a
#'   character vector, and `slide_raw()` a raw vector.
#'   The output of `.f` will be automatically typed upwards,
#'   e.g. logical -> integer -> double -> character.
#'
#'   If `.x` has `names()`, the return value preserves those names unless
#'   `.fill = NULL`.
#' @export
#' @family roll variants
#' @examples
#' rnorm(n = 40) %>%
#'   slide_dbl(mean, 20)
#'
#' # Or use an anonymous function
#' rnorm(n = 40) %>%
#'   slide_dbl(function(x) mean(x[x > median(x)]), 20)
#'
#' # Or a formula
#' rnorm(n = 40) %>%
#'   slide_dbl(~ mean(.x[.x > median(.x)]), 20)
#'
slide <- function(.x, .f, .n, .align = "right", .fill = list(NULL), ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "list")
}

#' @rdname slide
#' @export
slide_lgl <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "logical")
}

#' @rdname slide
#' @export
slide_chr <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "character")
}

#' @rdname slide
#' @export
slide_int <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "integer")
}

#' @rdname slide
#' @export
slide_dbl <- function(.x, .f, .n, .align = "right", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "double")
}

#' @rdname slide
#' @export
slide_raw <- function(.x, .f, .n, .align = "right", .fill = as.raw(0), ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  align <- match(.align, c("left", "center", "right"))
  .Call(slide_impl, environment(), ".x", ".f", .n, align, .fill, "raw")
}
