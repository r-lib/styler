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
#' @family stylers
style_pkg <- function(pkg = ".",
  flat = FALSE,
  transformers = get_transformers(flat = flat)) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, prettify_local(transformers, flat = flat))
}

prettify_local <- function(transformers, flat) {
  r_files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  r_files <- grep("/RcppExports[.]R$", r_files, invert = TRUE, value = TRUE)
  test_files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- c(r_files, test_files)

  transform_files(files, transformers, flat)
}


#' Style a string
#'
#' Styles a character vector
#' @param text A character vector with text to style.
#' @param transformers A list with functions to be applied to the parsed data.
#' @param flat Whether to do the styling with a flat approach or with a nested
#'   approach.
#' @family stylers
#' @export
style_text <- function(text,
  flat = FALSE,
  transformers = get_transformers(flat = flat)) {
  transformer <- make_transformer(transformers, flat = flat)
  transformer(text)
}

#' Prettify arbitrary R code
#'
#' Performs various substitutions in all `.R` files in a directory.
#' Carefully examine the results after running this function!
#' @param path Path to a directory with files to transform.
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   of `path` should be styled as well.
#' @inheritParams style_text
#' @family stylers
#' @export
style_src <- function(path = ".",
  flat = FALSE,
  recursive = TRUE,
  transformers = get_transformers(flat = flat)) {
  withr::with_dir(path, prettify_any(transformers,
      recursive = recursive,
      flat = flat))
}

#' Prettify R code in current working directory
#'
#' This is a helper function for style_src.
#' @inheritParams style_src
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   should be styled as well.
#' @keywords internal
prettify_any <- function(transformers, recursive, flat) {
  files <- dir(path = ".", pattern = "[.][rR]$", recursive = recursive, full.names = TRUE)
  transform_files(files, transformers, flat = flat)

}

#' Style a file
#'
#' Performs various substitutions in the `.R` file specified.
#'   Carefully examine the results after running this function!
#' @param path A path to a file to style.
#' @inheritParams style_text
#' @family stylers
#' @export
style_file <- function(path,
                       flat = FALSE,
                       transformers = get_transformers(flat = flat)) {
  withr::with_dir(dirname(path),
                 prettify_one(transformers, flat, basename(path)))
}

#' Prettify one R file
#'
#' This is a helper function for style_src.
#' @inheritParams style_src
#' @param path The path to a file that should be styled.
prettify_one <- function(transformers, flat, path) {
  if (!grepl("\\.[Rr]$", path)) stop(path, " is not a .R file")
  transform_files(path, transformers, flat = flat)
}

#' Style the active file
#'
#' Helper function fot RStudio Add-in.
style_active_file <- function() {
  file <- rstudioapi::getActiveDocumentContext()$path
  style_file(file,
             flat = FALSE,
             transformers = get_transformers(flat = FALSE))
}
