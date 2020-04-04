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
#' * sets up pre-commit in your current directory with `$ pre-commit install`.
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

#' Auto-update your hooks
#'
#' Runs `pre-commit autoupdate`.
#' @inheritParams fallback_doc
#' @export
autoupdate <- function(path_root = here::here()) {
  withr::with_dir(path_root, {
    assert_correct_upstream_repo_url()
    out <- call_and_capture(path_precommit_exec(), "autoupdate")
    if (out$exit_status == 0) {
      usethis::ui_done(paste0(
        "Ran `pre-commit autoupdate` to get the latest version of the hooks."
      ))
    } else {
      rlang::abort("Running precommit autoupdate failed.")
    }
  })
}


upstream_repo_url_is_outdated <- function() {
  purrr::map_chr(yaml::read_yaml(".pre-commit-config.yaml")$repos, ~ .x$repo) %>%
    grepl("https://github.com/lorenzwalthert/pre-commit-hooks", ., fixed = TRUE) %>%
    any()
}
