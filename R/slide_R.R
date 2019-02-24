#' R implementation of rolling window calculation
#'
#' R implementation of \code{\link{slide_dbl}} for speed comparison during
#' development of the `slide` functions.
#'
#' @inheritParams slide
#' @examples
#' x <- rnorm(n = 40)
#' x %>% slide_dbl_R(mean, 20)
#'
#' # Or use an anonymous function
#' x %>% slide_dbl_R(function(x) mean(x[x > median(x)]), 20)
#'
#' # Or a formula
#' x %>% slide_dbl_R(~ mean(.x[.x > median(x)]), 20)


#' @rdname slide_R
#' @export
slide_dbl_R <- function(.x, .f, .n, .align = "left", .fill = NA, ...) {
  .f <- as_mapper(.f, ...)
  .align <- arg_match(.align, c("left", "center", "right"))
  if (.align == "center" && .n %% 2 != 1) {
    warning(".align = 'center' with even numbered window size, aligning to the center left.")
  }

  if (length(.x) < .n) {

    if (is.null(.fill)) {
      res <- numeric()
    } else {
      res <- rep(as.numeric(.fill), length(.x))
    }

  } else {

    # main loop: iterate over rolling windows and apply .f
    res <- map_dbl(seq_len(length(.x) - .n + 1L),
      function(i) .f(.x[i:(i + .n - 1L)], ...))

    # pad to the left or right such that result is of same length as input
    if (!is.null(.fill)) {
      fill <- as.numeric(.fill)
      res <- switch(.align,
        "left" = c(res, rep(fill, .n - 1L)),
        "center" = c(rep(fill, (.n-1L)%/%2), res, rep(fill, .n-1L-(.n-1L)%/%2)),
        "right" = c(rep(fill, .n - 1L), res),
        stop('`.align` must be either "left",  or "right".'))
    }

  }
  res
}
