is_plain_r_file <- function(path) {
  grepl("\\.R$", path, ignore.case = TRUE)
}

is_rprofile_file <- function(path) {
  grepl(".rprofile", path, ignore.case = TRUE)
}
is_rmd_file <- function(path) {
  grepl("\\.(Rmd|Rmarkdown)$", path, ignore.case = TRUE)
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

#' `dir()`, but without dot-prefix and different defaults
#'
#' When using `dir()`, you can set `full.names = FALSE`, but then you can only
#' pass a character vector of length one as `path` to not loose the information
#' about where the files are. This function solves that case. It's needed when
#' one wants to standardize paths to use set operations on them, i.e. when the
#' user supplied input does not have a dot prefix. See 'Examples'.
#'
#' For different defaults, see `dir_without_._one`.
#' @param path A path.
#' @param ... Passed to [base::dir()].
#' @seealso set_and_assert_arg_paths
#' @keywords internal
#' @examples
#' setdiff("./file.R", "file.R") # you want to standardize first.
dir_without_. <- function(path, recursive = TRUE, ...) {
  purrr::map(path, dir_without_._one, recursive = recursive, ...) %>%
    unlist()
}

#' `dir()`, but with full names, ignored case, and included hidden files and
#' recursive.
#' @keywords internal
dir_without_._one <- function(path, recursive, ...) {
  relative <- dir(
    path = path,
    full.names = FALSE,
    ignore.case = TRUE,
    recursive = recursive,
    all.files = TRUE,
    ...
  )
  if (path == ".") {
    return(relative)
  }
  file.path(path, relative)
}
