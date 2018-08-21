#' Construct an object of class vertical
#'
#' Sole purpose of the class vertical is to have a print method that
#' aligns the output vertically.
#' @param x A character vector or an object of class "vertical".
#' @keywords internal
construct_vertical <- function(x) {
  stopifnot(inherits(x, what = c("utf8", "character", "vertical")))
  structure(x, class = "vertical")
}

#' Print styled code
#'
#' @param x A character vector, one element corresponds to one line of code.
#' @param ... Not currently used.
#' @param colored Whether or not the output should be colored with
#'   `prettycode::highlight()`.
#' @param style Passed to `prettycode::highlight()`.
#' @importFrom rlang is_installed
#' @export
print.vertical <- function(x, ...,
                           colored = getOption("styler.colored_print.vertical"),
                           style = prettycode::default_style()) {
  if (colored) {
    if (is_installed("prettycode")) {
      x <- prettycode::highlight(x, style = style)
    } else {
      warn(c(
        "Could not use colored = TRUE, as the package prettycode is not",
        "installed. Please install it if you want to see colored output."
      ))
    }
  }
  cat(x, sep = "\n")
}
