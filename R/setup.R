#' Get started with pre-commit
#'
#' This function sets up pre-commit for your git repo.
#' @param install_hooks Whether to install environments for all available hooks.
#'   If `FALSE`, environments are installed with first commit.
#' @inheritParams use_ci
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
                          ci = getOption("precommit.ci", "native"),
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
    use_ci(ci)
  } else {
    if (ci == "gha") {
      use_ci(ci, force = force, root = root)
    }
  }

  invisible(NULL)
}


#' Use continuous integration with pre-commit
#'
#' Sets up continuous integration, or prompts the user to do it manually.
#'
#' @param ci Specifies which continuous integration service to use. See
#'   `vignette("ci", package = "precommit")` for details. Defaults to
#'   `getOption("precommit.ci", "native")`, which is set to
#'   `"native"` on package loading (if unset). `"native"` sets up
#'   [pre-commit.ci](https://pre-commit.ci). Alternatively, `"gha"` can be used
#'   to set up [GitHub Actions](https://github.com/features/actions). Set value
#'   to `NULL` if you don't want to use a continuous integration.
#' @param force Whether or not to overwrite an existing ci config file (only
#'   relevant for `ci = "gha"`).
#' @inheritParams fallback_doc
#' @export
use_ci <- function(ci = getOption("precommit.ci", "native"),
                   force = FALSE, root = here::here()) {
  if (is.na(ci)) {
    return()
  } else if (ci == "gha") {
    dest <- fs::path(root, ".github/workflows/pre-commit.yaml")
    fs::dir_create(fs::path_dir(dest))
    fs::file_copy(
      system.file("pre-commit-gha.yaml", package = "precommit"),
      dest,
      overwrite = force
    )
    cli::cli_alert_success(paste0(
      "Added GitHub Action template to ",
      "{.code .github/workflows/pre-commit.yaml}. Pre-commit hooks will ",
      "run on pull requests. If workflow fails, please file an issue in ",
      "{.code https://github.com/lorenzwalthert/precommit}."
    ))
  } else if (ci == "native") {
    cli::cli_ul("Sign in with GitHub to authenticate {.url https://pre-commit.ci}.")
    Sys.sleep(2)
    utils::browseURL("https://pre-commit.ci")
  } else {
    rlang::abort(
      'Argument `ci` must be one of `"native"` (default), `"gha"` or `NULL`.'
    )
  }
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
#' @param open Whether or not to open the .pre-commit-config.yaml. The default
#' is `TRUE` when working in  RStudio. Otherwise, we recommend manually opening
#' the file.
#' @inheritParams fallback_doc
#' @export
snippet_generate <- function(snippet = "",
                             open = rstudioapi::isAvailable(),
                             root = here::here()) {
  rlang::arg_match(snippet, c("additional-deps-roxygenize"))
  if (snippet == "additional-deps-roxygenize") {
    rlang::inform(paste(
      "Generating snippet using CRAN versions. If you need another source,",
      "specify with syntax that `renv::install()` understands (see examples in",
      "help file).",
      "\n"
    ))
    deps <- desc::desc_get_deps()
    non_r_deps <- deps[!(deps$type == "Depends" & deps$package == "R"), ]
    non_r_deps <- non_r_deps[order(non_r_deps$package), ]
    snippet_generate_impl_additional_deps_roxygenize(non_r_deps$package) %>%
      cat(sep = "")
    cat("\n")
    cli::cli_ul("Copy the above into `.pre-commit-config.yaml`.")
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
  if (open) {
    precommit::open_config(root)
  }
}

snippet_generate_impl_additional_deps_roxygenize <- function(packages, with_version = FALSE) {
  out <- paste0(
    "        -    ", packages, if (with_version) "@",
    if (with_version) purrr::map_chr(packages, ~ as.character(packageVersion(.x))), "\n",
    collapse = ""
  ) %>%
    sort()
  paste0("    -   id: roxygenize
        # roxygen requires loading pkg -> add dependencies from DESCRIPTION
        additional_dependencies:\n", out)
}
