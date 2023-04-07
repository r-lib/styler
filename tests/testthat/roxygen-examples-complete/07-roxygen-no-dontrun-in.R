#' Style `.R` and/or `.Rmd` files
#'
#' Performs various substitutions in the files specified.
#'   Carefully examine the results after running this function!
#' @param path A character vector with paths to files to style.
#' @inheritParams style_pkg
#' @inheritSection transform_files Value
#' @inheritSection style_pkg Warning
#' @inheritSection style_pkg Roundtrip Validation
#' @examples
#' # the following is identical but the former is more convenient:
#' file<- tempfile("styler",
#' fileext = ".R")
#' xfun::write_utf8("1++1", file)
#' style_file(
#' file, style = tidyverse_style, strict = TRUE)
#' style_file(file, transformers = tidyverse_style(strict = TRUE))
#' xfun::read_utf8(file)
#' unlink(file2)
#' @family stylers
#' @export
style_file <- function(path,
                       ... ,
                       style = tidyverse_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE) {
  changed<- withr::with_dir(
    dirname(path
            ),
    transform_files(basename(path), transformers)
  )
  invisible(changed)
}
