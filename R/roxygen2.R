extract_diff_one_line <- function(one_line) {
  if (one_line$old_lineno < 0 || one_line$new_lineno < 0) {
    one_line$content
  } else {
    NULL
  }
}

extract_diff_lines <- function(lines) {
  purrr::map(lines, extract_diff_one_line)
}

extract_diff_hunks <- function(hunks) {
  purrr::map(hunks, ~ extract_diff_lines(.x$lines))
}

extract_diff_files <- function(files) {
  purrr::map(files, ~ extract_diff_hunks(.x$hunks))
}

#' Extract old and new lines from `git diff --cached`
#'
#' This is useful to detect within a hook script if the core function
#' from a hook such as [roxygen2::roxygenize()] must run at all or not.
#' @param root The root of project.
#' @keywords internal
extract_diff_root <- function(root = here::here()) {
  assert_is_git_repo(root)
  repo <- git2r::repository(root)
  if (length(git2r::reflog(repo)) == 0) {
    # nothing committed yet
    all_files <- git2r::status()$staged

    purrr::map(all_files[grepl("^R/.*\\.[Rr]$", all_files)], readLines) %>%
      unlist() %>%
      unname()
  } else {
    diff <- git2r::diff(repo, index = TRUE)
    extract_diff_files(diff$files) %>%
      unlist()
  }
}

#' Check if we should run roxygen.
#'
#' This is the case if a new or replaced/removed line contains a roxygen2
#' comment in a file that is staged. This is only exported because it's used
#' in a hook script: It's not intended to be called by users of {precommit}
#' directly.
#' @return
#' A logical vector of length 1.
#' @inheritParams extract_diff_root
#' @examples
#' \dontrun{
#' diff_requires_run_roxygenize()
#' }
#' @export
diff_requires_run_roxygenize <- function(root = here::here()) {
  if (!rlang::is_installed("git2r")) {
    rlang::abort("You need to install the R package git2r to run this hook.")
  }
  changed_lines_content <- extract_diff_root(root)
  any(grepl("^#'", changed_lines_content))
}
