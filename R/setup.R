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
#' @section What it does the function do?:
#' * sets up pre-commit in your current directory with `$ pre-commit install`.
#' * sets up a template `.pre-commit-config.yaml`.
#' * autoupdates the template to make sure you get the latest versions of the
#'   hooks.
#' * Open the config file if RStudio is running.
#' @family helpers
#' @export
use_precommit <- function(config_source = getOption("precommit.config_source"),
                          force = FALSE,
                          open = rstudioapi::isAvailable(),
                          root = here::here()) {
  assert_is_installed()
  assert_is_git_repo(root)
  config_source <- set_config_source(config_source)
  install_repo(root)
  use_precommit_config(
    config_source, force, root,
    open = FALSE, verbose = FALSE
  )
  autoupdate(root)
  if (open) {
    open_config(root)
  }
}

#' Auto-update your hooks
#'
#' Runs `pre-commit autoupdate`.
#' @inheritParams fallback_doc
#' @export
autoupdate <- function(root = here::here()) {
  withr::with_dir(root, {
    assert_correct_upstream_repo_url()
    out <- call_precommit("autoupdate")
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
