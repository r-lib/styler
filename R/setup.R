#' Set up pre-commit
#'
#' Get started.
#' @inheritParams fallback_doc
#' @inheritParams use_precommit_config
#' @inheritSection use_precommit_config Copying an existing config file
#' @section When to call this function?:
#'
#' * You want to add pre-commit support to a git repo which does not have a
#'   `.pre-commit-config.yaml`. This involves adding
#'   a pre-commit config file and making sure git will call the hooks before
#'   the next commit.
#' * You cloned a repo that has a `.pre-commit-config.yaml` already. You need
#'   to make sure git calls the hooks before the next commit.
#'
#' @section What it does the funciton do?:
#' * installs pre-commit in your current directory.
#' * sets up a template `.pre-commit-config.yaml`.
#' * autoupdates the template to make sure you get the latest versions of the
#'   hooks.
#' * Open the config file if RStudio is running.
#' @family helpers
#' @export
use_precommit <- function(path_cp_config_from = getOption("precommit.path_cp_config_from"),
                          force = FALSE,
                          open = rstudioapi::isAvailable(),
                          path_root = here::here()) {
  assert_is_installed()
  assert_is_git_repo(path_root)
  path_cp_config_from <- set_path_cp_config_from(path_cp_config_from)
  install_repo(path_root)
  use_precommit_config(
    path_cp_config_from, force, path_root,
    open = FALSE, verbose = FALSE
  )
  autoupdate(path_root)
  if (open) {
    open_config(path_root)
  }
}

assert_is_git_repo <- function(path_root) {
  if (is.null(git2r::discover_repository(path_root))) {
    rlang::abort(paste0(
      "The directory ", path_root, " is not a git repo. Please navigate to ",
      path_root, " and init git in ",
      "this directory with `$ git init` from the command line or ",
      "`> usethis::use_git()` from the R prompt."
    ))
  }
}

assert_is_installed <- function() {
  if (!is_installed()) {
    rlang::abort(paste0(
      "pre-commit is not installed on your system (or we can't find it).\n\n",
      "If you have it installed and you know where it is, please set the R option ",
      "`precommit.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R. ",
      "If you think this is a standard location, please open an issue on GitHub ",
      "so we can auto-detect this location in the future and spare new users some",
      "set-up troubles.\n\n",
      "If you don't know where the executable is stored, go back to the log output ",
      "that resulted from the installation of pre-commit for hints. If you found ",
      "it and you think it's a standard location, please open an issue on GitHub ",
      "so we can auto-detect this location in the future and spare unexpereienced",
      "users some trouble.\n\n",
      "In case you are totally lost with these messages, you can most likely ",
      "solve the problems with just using the conda installation method, see ",
      "https://lorenzwalthert.github.io/precommit/ for how to do this."
    ))
  }
}

#' Install pre-commit on your system with conda
#' @keywords internal
install_precommit_impl <- function() {
  if (!"r-reticulate" %in% reticulate::conda_list()$name) {
    reticulate::conda_create("r-reticulate")
  }
  reticulate::conda_install(packages = "pre-commit")
}

#' Auto-update your hooks
#'
#' Runs `pre-commit autoupdate`.
#' @inheritParams fallback_doc
#' @export
autoupdate <- function(path_root = here::here()) {
  withr::with_dir(path_root, {
    assert_correct_upstream_repo_url()
    out <- system2(path_precommit_exec(), "autoupdate")
    if (out == 0) {
      usethis::ui_done(paste0(
        "Ran `pre-commit autoupdate` to get the latest version of the hooks."
      ))
    } else {
      rlang::abort("Running precommit autoupdate failed.")
    }
  })
}

assert_correct_upstream_repo_url <- function() {
  if (upstream_repo_url_is_outdated()) {
    usethis::ui_info(c(
      "The repo https://github.com/lorenzwalthert/pre-commit-hooks ",
      "has moved to https://github.com/lorenzwalthert/precommit. ",
      "Please fix the URL in .pre-commit-config.yaml, ",
      "most confortably with `precommit::open_config()`."
    ))
  }
}

upstream_repo_url_is_outdated <- function() {
  purrr::map_chr(yaml::read_yaml(".pre-commit-config.yaml")$repos, ~ .x$repo) %>%
    grepl("https://github.com/lorenzwalthert/pre-commit-hooks", ., fixed = TRUE) %>%
    any()
}
