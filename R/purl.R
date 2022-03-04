#' Run [knitr::purl()], setting the chunk option `purl` to `TRUE` if it's not
#' already set to a literal value.
#'
#' This function is only exported for use in hook scripts, but it's not intended
#' to be called by the end-user directly.
#' @param path The path to the file you want to [knitr::purl()].
#' @family hook script helpers
#' @keywords internal
#' @export
robust_purl <- function(path) {
  ext <- fs::path_ext(path)
  path_rmd <- tempfile(fileext = paste0(".", ext))
  file.copy(path, path_rmd)
  lines <- readLines(path_rmd)
  has_purl <- grepl("purl.*=.*(TRUE|FALSE|T|F)", lines)
  has_eval <- grepl("eval.*=.*(TRUE|FALSE|T|F)", lines)
  should_not_override <- has_purl | has_eval

  if (tolower(ext) == "rmd") {
    lines[!should_not_override] <- gsub(
      "^```\\{ *[Rr] *.*\\}.*", "```{r purl = TRUE}", lines[!should_not_override]
    )
  } else if (tolower(ext) == "rnw") {
    lines[!should_not_override] <- gsub(
      "^<<>>=.*", "<<purl=TRUE>>=", lines[!should_not_override]
    )
  }
  writeLines(lines, path_rmd)
  path_ <- knitr::purl(
    input = path_rmd,
    output = tempfile(fileext = ".R"),
    quiet = TRUE,
    documentation = FALSE
  )
  path_
}
