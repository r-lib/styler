is_plain_r_file <- function(path) {
  grepl("\\.R$", path, ignore.case = TRUE)
}

is_rmd_file <- function(path) {
  grepl("\\.Rmd$", path, ignore.case = TRUE)
}

is_rnw_file <- function(path) {
  grepl("\\.Rnw$", path, ignore.case = TRUE)
}

is_unsaved_file <- function(path) {
  path == ""
}

#' Map the file type to a corresponding regular expression
#'
#' @param filetype The file type to map to a regex.
#' @examples
#' styler:::map_filetype_to_pattern(c(".rMd", "R"))
#' @keywords internal
map_filetype_to_pattern <- function(filetype) {
  paste0("(", paste(set_and_assert_arg_filetype(filetype), collapse = "|"), ")$")
}
