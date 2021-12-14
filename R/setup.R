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
#' @param open Whether or not to open `.pre-commit-config.yaml` after
#'   it's been placed in your repo as well as
#'   [pre-commit.ci](https://pre-commit.ci) (if `ci = "native"`). The default is
#'   `TRUE` when working in RStudio.
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
  }
  use_ci(ci, force = force, open = open, root = root)
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
#' @param open Whether or not to open [pre-commit.ci](https://pre-commit.ci)
#'   (if `ci = "native"`). The default is `TRUE` when working in RStudio.
#' @inheritParams fallback_doc
#' @export
use_ci <- function(ci = getOption("precommit.ci", "native"),
                   force = FALSE,
                   open = rstudioapi::isAvailable(),
                   root = here::here()) {
  if (!fs::file_exists(fs::path(root, ".pre-commit-config.yaml"))) {
    rlang::abort(paste0(
      "Your repo has no `.pre-commit-config.yaml` file, please initialize ",
      "{precommit} with `precommit::use_precommit()`."
    ))
  }
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
    cli::cli_ul(paste0(
      "Sign in with GitHub to authenticate {.url https://pre-commit.ci} and ",
      "then come back to complete the set-up process."
    ))
    Sys.sleep(2)
    if (open) {
      utils::browseURL("https://pre-commit.ci")
    }
  } else {
    rlang::abort(
      'Argument `ci` must be one of `"native"` (default), `"gha"` or `NULL`.'
    )
  }
  config <- readLines(fs::path(root, ".pre-commit-config.yaml"))
  if (length(grep("^ *- *id *: *roxygenize", config)) > 0) {
    cli::cli_ul(paste0(
      "It seems like you are using the roxygenize hook. This requires further ",
      "edits in your {.code .pre-commit-config.yaml}, please run ",
      "{.code precommit::snippet_generate('additional-deps-roxygenize')} to ",
      "proceed."
    ))
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
    if (unname(read.dcf("DESCRIPTION")[, "Package"]) == "precommit") {
      message(paste0(
        "`autoupdate()` ran in {precommit} package directory, skipping ",
        "`ensure_renv_precommit_compat()`"
      ))
    } else {
      ensure_renv_precommit_compat(root = root)
    }
    invisible(out$exit_status)
  })
}

ensure_renv_precommit_compat <- function(root = here::here()) {
  withr::local_dir(root)
  path_config <- ".pre-commit-config.yaml"
  config_lines <- readLines(path_config, encoding = "UTF-8")
  has_renv <- fs::file_exists("renv.lock")
  if (!has_renv) {
    return()
  }

  rev <- rev_read(path_config)
  rlang::with_handlers(
    {
      rev <- rev_as_pkg_version(rev)
      maximal_rev <- package_version("0.1.3.9014")
      if (rev > maximal_rev) {
        rlang::warn(paste0(
          "It seems like you want to use {renv} and {precommit} in the same ",
          "repo. This is not well supported for users of RStudio and ",
          "`precommit > 0.1.3.9014` at the moment (details: ",
          "https://github.com/lorenzwalthert/precommit/issues/342). ",
          "Autoupdate aborted and `rev:` in `.pre-commit-config.yaml` set to ",
          "a version compatible with {renv}."
        ))
        config_lines <- gsub(
          paste0("^ *rev *: *", "v", as.character(rev)),
          "    rev: v0.1.3.9014",
          config_lines
        )
        withr::local_options(encoding = "native.enc")
        writeLines(enc2utf8(config_lines), path_config, useBytes = TRUE)
      }
    },
    error = function(e) NULL
  )
}


upstream_repo_url_is_outdated <- function() {
  rev_read(".pre-commit-config.yaml") %>%
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
#'   the roxygenize hook.
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
    hard_dependencies <- deps[(deps$type %in% c("Depends", "Imports")), "package"]
    hard_dependencies_vec <- hard_dependencies %>%
      setdiff("R")
    if (length(hard_dependencies_vec) < 1) {
      cli::cli_alert_success(paste0(
        "According to {.code DESCRIPTION}`, there are no hard dependencies of ",
        "your package. You are set."
      ))
      return()
    }
    hard_dependencies %>%
      snippet_generate_impl_additional_deps_roxygenize() %>%
      cat(sep = "")
    cat("\n")
    cli::cli_ul(paste0(
      "Replace the `id: roxygenize` key in `.pre-commit-config.yaml` with the ",
      "above code."
    ))
    cli::cli_alert_info(paste0(
      "Note that CI services like {.url pre-commit.ci} have build-time ",
      "restrictions and installing the above dependencies may exceed those, ",
      "resulting in a timeout. See ",
      '{.code vignette("ci", package = "precommit")} for details and solutions.'
    ))
    remote_deps <- rlang::with_handlers(
      desc::desc_get_field("Remotes"),
      error = function(e) character()
    )
    if (length(remote_deps) > 0) {
      rlang::warn(paste0(
        "It seems you have remote dependencies in your `DESCRIPTION`. You ",
        "need to edit the above list manually to match the syntax `renv::install()` ",
        "understands, i.e. if you have in your `DESCRIPTION`

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
    "        -    ", packages, "\n",
    collapse = ""
  ) %>%
    sort()
  paste0("    -   id: roxygenize
        # roxygen requires loading pkg -> add dependencies from DESCRIPTION
        additional_dependencies:\n", out)
}
