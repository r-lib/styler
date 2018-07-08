#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package
#' (code and tests).
#' Carefully examine the results after running this function!
#' @examples
#' style_pkg(style = tidyverse_style, strict = TRUE)
#' style_pkg(
#'   scope = "line_breaks",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'")
#' )
a <- call
