#' this function does
#'
#' @param x a parameter.
#'    indented comments
a <- function(x) {
  test_that("I want to test", {
    out <- c(1, c(
      22 + 1
    ))
    if (x > 10) {
      for (x in 22) { # FIXME in operator only to be surrounded by one space. What about %in%?
        prin(x)
      }
    }
  })
  # we like comments too
  c(
    list(x + 2),
    c(c(
      26 ^ 2,
      8,
      7
  )))

  call(
    1, 2,
    23 + Inf - 99, call(
      16
  ))
}
# comments everywhere
