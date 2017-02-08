#' @api
#' @import tibble
#' @import dplyr
#' @import tidyr
NULL

#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package
#' (code and tests).
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package
#'
#' @export
style_pkg <- function(pkg = ".", transformers = get_transformers()) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

#' @export
style_text <- function(text, transformers = get_transformers()) {
  transformer <- make_transformer(transformers)
  transformer(text)
}

#' @export
get_transformers <- function(strict = TRUE) {
  c(
    if (strict) set_space_around_op else add_space_around_op,
    remove_space_after_unary_pm,
    fix_quotes,
    remove_space_after_opening_paren,
    remove_space_before_closing_paren,
    if (strict) set_space_after_comma else add_space_after_comma,
    NULL)
}

prettify_local <- function(transformers) {
  #files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)

  transform_files(files, transformers)
}
