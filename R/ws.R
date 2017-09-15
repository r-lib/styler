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
#' @section Warning:
#' This function overwrites files (if styling results in a change of the
#' code to be formatted). It is strongly suggested to only style files
#' that are under version control or to create a backup copy.
#' @family stylers
#' @examples
#' \dontrun{
#' # the following is identical but the former is more convenient:
#' style_pkg(style = tidyverse_style, strict = TRUE)
#' style_pkg(transformers = tidyverse_style(strict = TRUE))
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
#' @inheritSection style_pkg Warning
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
#' @inheritSection style_pkg Warning
#' @examples
#' \dontrun{
#' # the following is identical but the former is more convenient:
#' style_file("file.R", style = tidyverse_style, strict = TRUE)
#' style_text("file.R", transformers = tidyverse_style(strict = TRUE))
#' }
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
