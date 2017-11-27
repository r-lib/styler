#' @api
#' @import tibble
#' @importFrom magrittr %>%
NULL

#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package
#' (code and tests).
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package.
#' @param ... Arguments passed on to the `style` function.
#' @param style A function that creates a style guide to use, by default
#'   [tidyverse_style()] (without the parentheses). Not used
#'   further except to construct the argument `transformers`. See
#'   [style_guides()] for details.
#' @param transformers A set of transformer functions. This argument is most
#'   conveniently constructed via the `style` argument and `...`. See
#'   'Examples'.
#' @param exclude_files Character vector with paths to files that should be
#'   excluded from styling.
#' @section Warning:
#' This function overwrites files (if styling results in a change of the
#' code to be formatted). It is strongly suggested to only style files
#' that are under version control or to create a backup copy.
#' @inheritSection transform_files Value
#' @family stylers
#' @examples
#' \dontrun{
#' # the following is identical but the former is more convenient:
#' style_pkg(style = tidyverse_style, strict = TRUE)
#' style_pkg(transformers = tidyverse_style(strict = TRUE))
#' }
#' @export
style_pkg <- function(pkg = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      exclude_files = "R/RcppExports.R") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  changed <- withr::with_dir(pkg_root, prettify_local(transformers, exclude_files))
  invisible(changed)
}

prettify_local <- function(transformers, exclude_files) {
  r_files <- dir(
    path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE
  )

  r_files <- grep("/RcppExports[.]R$", r_files, invert = TRUE, value = TRUE)
  test_files <- dir(
    path = "tests/testthat", pattern = "[.][rR]$",
    recursive = TRUE, full.names = TRUE
  )

  files <- setdiff(c(r_files, test_files), exclude_files)

  transform_files(files, transformers)
}


#' Style a string
#'
#' Styles a character vector. Each element of the character vector corresponds
#' to one line of code.
#' @param text A character vector with text to style.
#' @inheritParams style_pkg
#' @family stylers
#' @examples
#' style_text("call( 1)")
#' style_text("1    + 1", strict = FALSE)
#' style_text("a%>%b", scope = "spaces")
#' style_text("a%>%b; a", scope = "line_breaks")
#' style_text("a%>%b; a", scope = "tokens")
#' # the following is identical but the former is more convenient:
#' style_text("a<-3++1", style = tidyverse_style, strict = TRUE)
#' style_text("a<-3++1", transformers = tidyverse_style(strict = TRUE))
#' @export
style_text <- function(text,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...)) {

  transformer <- make_transformer(transformers)
  styled_text <- transformer(text)
  construct_vertical(styled_text)
}

#' Prettify arbitrary R code
#'
#' Performs various substitutions in all `.R` files in a directory.
#' Carefully examine the results after running this function!
#' @param path Path to a directory with files to transform.
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   of `path` should be styled as well.
#' @inheritParams style_pkg
#' @inheritSection transform_files Value
#' @inheritSection style_pkg Warning
#' @family stylers
#' @export
style_dir <- function(path = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      recursive = TRUE,
                      exclude_files = NULL) {
  changed <- withr::with_dir(
    path, prettify_any(transformers, recursive, exclude_files)
  )
  invisible(changed)
}

#' Prettify R code in current working directory
#'
#' This is a helper function for style_dir.
#' @inheritParams style_pkg
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   should be styled as well.
prettify_any <- function(transformers, recursive, exclude_files) {
  files <- dir(
    path = ".", pattern = "[.][rR]$", recursive = recursive, full.names = TRUE
  )
  transform_files(setdiff(files, exclude_files), transformers)

}

#' Style `.R` and/or `.Rmd` files
#'
#' Performs various substitutions in the files specified.
#'   Carefully examine the results after running this function!
#' @param path A character vector with paths to files to style.
#' @inheritParams style_pkg
#' @inheritSection transform_files Value
#' @inheritSection style_pkg Warning
#' @examples
#' # the following is identical but the former is more convenient:
#' file <- tempfile("styler", fileext = ".R")
#' enc::write_lines_enc("1++1", file)
#' style_file(file, style = tidyverse_style, strict = TRUE)
#' style_file(file, transformers = tidyverse_style(strict = TRUE))
#' enc::read_lines_enc(file)
#' unlink(file)
#' @family stylers
#' @export
style_file <- function(path,
                        ...,
                        style = tidyverse_style,
                        transformers = style(...)) {
  changed <- withr::with_dir(
    dirname(path),
    transform_files(basename(path), transformers)
  )
  invisible(changed)
}
