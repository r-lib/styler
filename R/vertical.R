#' Construct an object of class vertical
#'
#' Sole puropse of the class vertical is to have a print method that
#' aligns the output vertically.
#' @param x A character vector or an object of class "vertical".
construct_vertical <- function(x) {
  stopifnot(inherits(x, what = c("utf8", "character", "vertical")))
  structure(x, class = "vertical")
}

#' @export
print.vertical <- function(x, ...) {
  cat(x, sep = "\n")
}
