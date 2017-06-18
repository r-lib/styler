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
#' @inheritParams style_text
#' @export
style_pkg <- function(pkg = ".", transformers = get_transformers()) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

prettify_local <- function(transformers) {
  r_files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  r_files <- grep("/RcppExports[.]R$", r_files, invert = TRUE, value = TRUE)
  test_files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- c(r_files, test_files)

  transform_files(files, transformers)
}


#' Style a string
#'
#' Styles a character vector
#' @param text A character vector with text to style.
#' @param transformers A list with functions to be applied to the parsed data.
#' @export
style_text <- function(text, transformers = get_transformers()) {
  transformer <- make_transformer(transformers)
  transformer(text)
}

#' Prettify arbitrary R code
#'
#' Performs various substitutions in all `.R` files in a directory.
#' Carefully examine the results after running this function!
#' @param path Path to a directory with files to transform.
#' @param transformers A list of transformer functions to be applied to the
#'   files in `path`.
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   of `path` should be styled as well.
#' @export
style_src <- function(path = ".", transformers = get_transformers(), recursive = TRUE) {
  withr::with_dir(path, prettify_any(transformers, recursive = recursive))
}

#' Prettify R code in current working directory
#'
#' This is a helper function for style_src.
#' @inheritParams style_src
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   should be styled as well.
#' @keywords internal
prettify_any <- function(transformers, recursive) {
  files <- dir(path = ".", pattern = "[.][rR]$", recursive = recursive, full.names = TRUE)
  transform_files(files, transformers)

}
