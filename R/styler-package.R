#' Non-invasive pretty printing of R code
#'
#' styler allows you to format `.R`, `.Rmd`, `.Rmarkdown` and/or
#' `.qmd`, `.Rnw` files, R packages, or entire R source trees
#' according to a style guide.
#' The following functions can be used for styling:
#' * [style_text()] to style a character vector.
#' * [style_file()] to style a single file.
#' * [style_dir()] to style all files in a directory.
#' * [style_pkg()] to style the source files of an R package.
#' * [styler_addins] (RStudio Addins) to style either selected code or the
#' active file.
#' @examples
#' style_text("call( 1)")
#' style_text("1    + 1", strict = FALSE)
#' style_text("a%>%b", scope = "spaces")
#' style_text("a%>%b; a", scope = "line_breaks")
#' style_text("a%>%b; a", scope = "tokens")
"_PACKAGE"

## usethis namespace: start
#'
#' @importFrom magrittr "%>%"
#' @importFrom purrr compact partial flatten flatten_int flatten_chr
#' @importFrom purrr map map_lgl map_int map_chr map2 map2_chr map_at pmap pwalk
#' @importFrom rlang abort warn seq2 check_installed is_installed "%||%" set_names
#' @importFrom vctrs vec_rbind vec_slice vec_split
## usethis namespace: end
NULL


utils::globalVariables(c(
  ".",
  "pd", "pd_nested", "pd_flat", "flattened_pd",
  "line1", "line2", "col1", "col2", "parent",
  "terminal", "text", "short",
  "spaces", "lag_spaces",
  "newlines", "lag_newlines",
  "pos_id",
  NULL
))
