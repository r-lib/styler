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
#' @param pkg Path to a (subdirectory of an) R package.
#' @param ... Passed on to the `style` function.
#' @param style The unquoted name of a style guide to use. Will not be used
#'   further except to construct the argument `transformers`. See
#'   [style_guides()] for details.
#' @param transformers A set of transformer functions.
#' @family stylers
#' @examples
#' \dontrun{
#' style_pkg()
#' }
#' @export
#' @family stylers
style_pkg <- function(pkg = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...)) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

prettify_local <- function(transformers) {
  r_files <- dir(
    path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE
  )

  r_files <- grep("/RcppExports[.]R$", r_files, invert = TRUE, value = TRUE)
  test_files <- dir(
    path = "tests/testthat", pattern = "[.][rR]$",
    recursive = TRUE, full.names = TRUE
  )

  files <- c(r_files, test_files)

  transform_files(files, transformers)
}


#' Style a string
#'
#' Styles a character vector
#' @param text A character vector with text to style.
#' @inheritParams style_pkg
#' @family stylers
#' @examples
#' style_text("call( 1)")
#' style_text("1    + 1", strict = FALSE)
#' @export
style_text <- function(text,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...)) {

  transformer <- make_transformer(transformers)
  transformer(text)
}

#' Prettify arbitrary R code
#'
#' Performs various substitutions in all `.R` files in a directory.
#' Carefully examine the results after running this function!
#' @param path Path to a directory with files to transform.
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   of `path` should be styled as well.
#' @inheritParams style_pkg
#' @family stylers
#' @export
style_dir <- function(path = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      recursive = TRUE) {
  withr::with_dir(
    path, prettify_any(transformers, recursive = recursive)
  )
}

#' Prettify R code in current working directory
#'
#' This is a helper function for style_dir.
#' @inheritParams style_pkg
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   should be styled as well.
prettify_any <- function(transformers, recursive) {
  files <- dir(path = ".", pattern = "[.][rR]$", recursive = recursive, full.names = TRUE)
  transform_files(files, transformers)

}

#' Style a file
#'
#' Performs various substitutions in the `.R` file specified.
#'   Carefully examine the results after running this function!
#' @param path A path to a file to style.
#' @inheritParams style_pkg
#' @family stylers
#' @export
style_file <- function(path,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...)) {
  withr::with_dir(
    dirname(path),
    prettify_one(transformers, basename(path))
  )
}

#' Prettify one R file
#'
#' This is a helper function for style_dir.
#' @inheritParams style_dir
#' @param path The path to a file that should be styled.
prettify_one <- function(transformers, path) {
  if (!grepl("\\.[Rr]$", path)) stop(path, " is not a .R file")
  transform_files(path, transformers)
}

#' Style the active file
#'
#' Helper function fot RStudio Add-in.
style_active_file <- function() {
  file <- rstudioapi::getActiveDocumentContext()$path
  style_file(file, style = tidyverse_style)
}
