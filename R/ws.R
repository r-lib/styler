#' @api
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import rex
NULL

#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package.
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package
#'
#' @export
styler <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  transformers <- c(
    add_space_around_equal,
    fix_quotes,
    NULL)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

prettify_local <- function(transformers) {
  #files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)

  transform_files(files, transformers)
}
