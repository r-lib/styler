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
#' This is useful to detect within a hook script if the core function from a
#' hook such as [roxygen2::roxygenize()] must run at all or not.
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
#' comment in a file that is staged.
#' This function is only exported for use in hook scripts, but it's not intended
#' to be called by the end-user directly.
#' @return
#' A logical vector of length 1.
#' @inheritParams extract_diff_root
#' @family hook script helpers
#' @examples
#' \dontrun{
#' diff_requires_run_roxygenize()
#' }
#' @export
diff_requires_run_roxygenize <- function(root = here::here()) {
  if (rlang::with_handlers(withr::with_namespace("git2r", FALSE), error = function(...) TRUE)) {
    generic <- paste0(
      " Please add the package as a dependency to ",
      "`.pre-commit-config.yaml` -> `id: roxygenize` -> ",
      "`additional_dependencies` and try again. The package must be ",
      "specified so `renv::install()` understands it, e.g. like this:\n\n",
      "    -   id: roxygenize",
      "
        additional_dependencies:
        - git2r\n\n"
    )
    msg <- paste0(
      "The R package {git2r} must be available to benefit from caching of this hook.",
      generic
    )
    rlang::warn(msg)
    return(TRUE)
  }
  changed_lines_content <- extract_diff_root(root)
  is_roxygen <- grepl("^#'", changed_lines_content)
  if (any(is_roxygen)) {
    return(TRUE)
  } else {
    # check if formals were changed
    # we invalidate the cache on formal change, even if it is not sure they are
    # documented with roxygen. This is easy, cheap and safe. Might give false
    # positive (invalidates in cases where it's not necessary).
    without_comments <- gsub("#.*", "", changed_lines_content)
    any(grep("function(", without_comments, fixed = TRUE))
  }
}

#' Assert if all dependencies are installed
#'
#' This function is only exported for use in hook scripts, but it's not intended
#' to be called by the end-user directly.
#' @family hook script helpers
#' @export
roxygen_assert_additional_dependencies <- function() {
  out <- rlang::with_handlers(
    # roxygen2 will load: https://github.com/r-lib/roxygen2/issues/771
    pkgload::load_all(quiet = TRUE),
    error = function(e) {
      e
    }
  )
  if (inherits(out, "packageNotFoundError") || ("message" %in% names(out) && grepl("Dependency package(\\(s\\))? .* not available", out$message))) {
    # case used in package but not installed
    rlang::abort(paste0(
      "The roxygenize hook requires all* dependencies of your package to be listed in ",
      "the file `.pre-commit-config.yaml` under `id: roxygenize` -> ",
      "`additional_dependencies:`, like this:\n\n",
      "    -   id: roxygenize",
      "
        additional_dependencies:
        -    tidyr
        -    dplyr\n\n",
      "Call ",
      "`precommit::snippet_generate('additional-deps-roxygenize')` ",
      "(requires the dev version of {precommit}, install with with ",
      "`remotes::install_github('lorenzwalthert/precommit')` and paste the ",
      "output into the file `.pre-commit-config.yaml`. This requires precommit",
      " > 0.1.3 and assumes you declared all dependencies in `DESCRIPTION`.",
      "\n\nContext: https://github.com/lorenzwalthert/precommit/issues/243",
      "\n\nThe initial error (from `pkgload::load_all()`) was: ",
      conditionMessage(out), ".\n\n===================================\n",
      "*Some packages are already installed in the renv to run the hook, so ",
      "these technically don't have to be listed as additional dependencies, ",
      "but we recommend listing all for simplicity and consistency."
    ))
  }
}

#' Roxygen depending on cache state
#'
#' This function is only exported for use in hook scripts, but it's not intended
#' to be called by the end-user directly.
#' @inheritParams R.cache::saveCache
#' @family hook script helpers
#' @export
#' @importFrom R.cache saveCache
# fails if accessed with R.cache::saveCache()!
roxygenize_with_cache <- function(key, dirs) {
  if (diff_requires_run_roxygenize()) {
    out <- rlang::with_handlers(
      roxygen2::roxygenise(),
      error = function(e) e
    )
    if (
      inherits(out, "packageNotFoundError") ||
        ("message" %in% names(out) && grepl("Dependency package(\\(s\\))? .* not available", out$message))
    ) {
      rlang::abort(paste0(
        conditionMessage(out),
        " Please add the package as a dependency to ",
        "`.pre-commit-config.yaml` -> `id: roxygenize` -> ",
        "`additional_dependencies` and try again. The package must be ",
        "specified so `renv::install()` understands it, e.g. like this:\n\n",
        "    -   id: roxygenize",
        "
        additional_dependencies:
        - r-lib/pkgapi\n\n"
      ))
    } else if (inherits(out, "error")) {
      rlang::abort(conditionMessage(out))
    } else if (inherits(out, "warning")) {
      rlang::warn(conditionMessage(out))
    }
    saveCache(object = Sys.time(), key = key, dirs = dirs)
  }
}
