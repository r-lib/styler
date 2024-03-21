#' Hi
#'
#' x
#' @examples
#' \dontrun{
#' style_pkg(style = tidyverse_style, strict = TRUE)
#' style_pkg(
#'   scope = "line_breaks",
#'   math_token_spacing = specfy_math_token_spacing(zero = "'+'")
#' )
#' }
#' @export
style_pkg <- function(pkg = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      filetype = "R",
                      exclude_files = "R/RcppExports.R",
                      include_roxygen_examples = TRUE) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  changed <- withr::with_dir(pkg_root, prettify_pkg(
    transformers, filetype, exclude_files, include_roxygen_examples
  ))
  invisible(changed)
}
