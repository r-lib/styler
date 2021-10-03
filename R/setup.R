#' Get started with pre-commit
#'
#' This function sets up pre-commit for your git repo.
#' @param install_hooks Whether to install environments for all available hooks.
#'   If `FALSE`, environments are installed with first commit.
#' @param legacy_hooks How to treat hooks already in the repo which are not
#'   managed by pre-commit. "forbid", the default, will cause  `use_precommit()`
#'   to fail if there are such hooks. "allow" will run these along with
#'   pre-commit. "remove" will delete them.
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
#' @section What does the function do?:
#' * Sets up a template `.pre-commit-config.yaml`.
#' * Autoupdates the template to make sure you get the latest versions of the
#'   hooks.
#' * Installs the pre-commit script along with the hook environments with
#'   `$ pre-commit install --install-hooks`.
#' * Opens the config file if RStudio is running.
#' @family helpers
#' @return
#' `NULL` (invisibly). The function is called for its side effects.
#' @examples
#' \dontrun{
#' use_precommit()
#' }
#' @export
use_precommit <- function(config_source = getOption("precommit.config_source"),
                          force = FALSE,
                          legacy_hooks = "forbid",
                          open = rstudioapi::isAvailable(),
                          install_hooks = TRUE,
                          root = here::here()) {
  rlang::arg_match(legacy_hooks, c("forbid", "allow", "remove"))
  assert_is_installed()
  assert_is_git_repo(root)
  config_source <- set_config_source(config_source, root = root)
  use_precommit_config(
    config_source, force, root,
    open = FALSE, verbose = FALSE
  )
  autoupdate(root)
  install_repo(root, install_hooks, legacy_hooks)
  if (open) {
    open_config(root)
  }
  invisible(NULL)
}

#' Auto-update your hooks
#'
#' Runs [`pre-commit autoupdate`](https://pre-commit.com/#pre-commit-autoupdate).
#' @return
#' The exit status from `pre-commit autoupdate` (invisibly).
#' @inheritParams fallback_doc
#' @examples
#' \dontrun{
#' autoupdate()
#' }
#' @export
autoupdate <- function(root = here::here()) {
  withr::with_dir(root, {
    assert_correct_upstream_repo_url()
    out <- call_precommit("autoupdate")
    if (out$exit_status == 0) {
      cli::cli_alert_success(paste0(
        "Ran {.fun pre-commit autoupdate } to get the latest version of the hooks."
      ))
    } else {
      communicate_captured_call(
        out,
        preamble = "Running precommit autoupdate failed."
      )
    }
    invisible(out$exit_status)
  })
}


upstream_repo_url_is_outdated <- function() {
  purrr::map_chr(yaml::read_yaml(".pre-commit-config.yaml")$repos, ~ .x$repo) %>%
    grepl("https://github.com/lorenzwalthert/pre-commit-hooks", ., fixed = TRUE) %>%
    any()
}

#' Generate code snippets
#'
#' Utility function to generate code snippets:
#'
#' @details
#' Currently supported:
#'
#' * additional-deps-roxygenize: Code to paste into
#'   `.pre-commit-config.yaml` for the additional dependencies required by
#'   roxygen2.
#' @param snippet Name of the snippet.
#' @inheritParams fallback_doc
#' @export
snippet_generate <- function(snippet = "", root = here::here()) {
  rlang::arg_match(snippet, c("additional-deps-roxygenize"))
  if (snippet == "additional-deps-roxygenize") {
    rlang::inform(
      "Generating snippet using installed versions of all dependencies.\n"
    )
    deps <- desc::desc_get_deps()
    deps <- deps[order(deps$package), ]
    paste0(
      "        -    ", deps$package, "@",
      purrr::map_chr(deps$package, ~ as.character(packageVersion(.x))), "\n",
      collapse = ""
    ) %>%
      sort() %>%
      cat(sep = "")
    remote_deps <- rlang::with_handlers(
      desc::desc_get_field("Remotes"),
      error = function(e) character()
    )
    if (length(remote_deps) > 0) {
      rlang::warn(paste0(
        "It seems you have remote dependencies in your `DESCRIPTION`. You ",
        "need to edit the above list manually to match the syntax `renv::install()` ",
        "understands, i.e. if you have in your `DESCRIPTION`", "

Imports:
    tidyr
Remotes:
    tidyverse/tidyr@2fd80d5

You need in your `.pre-commit-config.yaml`

        additional_dependencies:
        -    tidyverse/tidyr@2fd80d5
      "
      ))
    }
  }
}
