#' @api
NULL

#' Convert from Rd to Markdown in roxygen2 comments
#'
#' Performs various substitutions in all `.R` files in a package.
#' Also attempts to enable Markdown support in `roxygen2` by adding a field to
#' `DESCRIPTION`.
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package
#' @return List of changed files, invisibly
#'
#' @export
roxygen2md <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, roxygen2md_local())
}

roxygen2md_local <- function() {
  files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  transformers <- c(
    convert_local_links,
    convert_alien_links,
    convert_code,
    NULL)

  add_roxygen_field()
  transform_files(files, transformers)
}

add_roxygen_field <- function() {
  roxygen_field <- desc::desc_get("Roxygen")
  roxygen_field_new <- "list(markdown = TRUE)"
  if (!identical(unname(roxygen_field), roxygen_field_new)) {
    if (is.na(roxygen_field)) {
      desc::desc_set("Roxygen" = roxygen_field_new)
    } else {
      message("Please update the Roxygen field in DESCRIPTION to include ", roxygen_field_new)
    }
  }
  invisible()
}

convert_local_links <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      "\\code{\\link{",
      capture(one_or_more(none_of("}"))),
      "}",
      maybe("()"),
      "}"
    ),
    "[\\1()]")
}

convert_alien_links <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      "\\code{\\link[",
      capture(one_or_more(none_of("]"))),
      "]{",
      capture(one_or_more(none_of("}"))),
      "}",
      maybe("()"),
      "}"
    ),
    "[\\1::\\2()]")
}

convert_code <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      "\\code{",
      capture(one_or_more(none_of("{}"))),
      "}"
    ),
    "`\\1`")
}
