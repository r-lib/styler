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
  use_precommit_config(path_cp_config_from, force, path_root, open = FALSE)
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
      "pre-commit is not installed on your system (or we can't find it). ",
      "If you have it installed, please set the R option ",
      "`precommit.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R.",
      "If not, install it with ",
      "`precommit::install_precommit()` or an installation ",
      "method in the official installation guide ",
      "(https://pre-commit.com/#install). The latter requires you to set",
      "the R option `precommit.executable` as well after the installation."
    ))
  }
}




set_path_cp_config_from <- function(path_cp_config_from) {
  if (is.null(path_cp_config_from)) {
    # workaround for R CMD CHECK warning about hidden top-level directories.
    name_origin <- "pre-commit-config.yaml"
    path_cp_config_from <- system.file(name_origin, package = "precommit")
  }
  if (!fs::file_exists(path_cp_config_from)) {
    rlang::abort(paste0(
      "File ", path_cp_config_from, " does not exist. Please use the ",
      "argument `path_cp_config_from` to provide a path to an existing ",
      "`.pre-commit.yaml` or `NULL` to use the template config."
    ))
  }
  file_type <- as.character(fs::file_info(path_cp_config_from)$type)
  if (!(file_type %in% c("file", "symlink"))) {
    rlang::abort(paste0(
      "File ", path_cp_config_from, " must be a file or a symlink, not a ",
      file_type, ". Please change the argument `path_cp_config_from` ",
      "accordingly."
    ))
  }
  path_cp_config_from
}

#' Initiate a pre-commit config file
#'
#' @param path_cp_config_from Path to a `.pre-commit-config.yaml`. This config file
#'   will be hard-copied into `path_root`. If `NULL`, we use
#'   a default config from the path returned by
#'   `system.file("pre-commit-config.yaml", package = "precommit")`. See
#'   section 'Copying an existing config file'.
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after
#'   it's been placed in your repo. The default is `TRUE` when working in
#'   RStudio. Otherwise, we recommend manually inspecting the file.
#' @section Copying an existing config file:
#' You can use an existing `.pre-commit-config.yaml` file when intiializing
#' pre-commit with [use_precommit()] using the argument `path_cp_config_from` to copy
#' an existing config file into your repo. For convenience, this argument
#' defaults to the R option `precommit.path_cp_config_from`, so you may want to
#' set this option in your `.Rprofile` for convenience.
#' Note that this is **not** equivalent
#' to the `--config` option in the CLI command `precommit install` and similar,
#' which do *not* copy a config file into a project root (and allow to put it
#' under version control), but rather link it in some more or less transparent
#' way.
#' @inheritParams fallback_doc
#' @export
use_precommit_config <- function(path_cp_config_from = getOption("precommit.path_cp_config_from"),
                                 force,
                                 open = rstudioapi::isAvailable(),
                                 path_root = here::here()) {
  path_cp_config_from <- set_path_cp_config_from(path_cp_config_from)
  escaped_name_target <- "^\\.pre-commit-config\\.yaml$"
  name_target <- ".pre-commit-config.yaml"
  if (!fs::file_exists(fs::path(path_root, name_target)) | force) {
    fs::file_copy(
      path_cp_config_from,
      fs::path(path_root, name_target),
      overwrite = TRUE
    )
    usethis::ui_done("Copied .pre-commit-config.yaml to {path_root}")
  } else {
    usethis::ui_warn(paste0(
      "There is already a pre-commit configuration file in ",
      path_root,
      ". Use `force = TRUE` to replace .pre-commit-config.yaml"
    ))
  }

  if (is_package(".")) {
    usethis::write_union(".Rbuildignore", escaped_name_target)
  }
  usethis::ui_todo(c(
    "Edit .precommit-hooks.yaml to (de)activate the hooks you want to use. ",
    "All available hooks: ",
    "https://pre-commit.com/hooks.html",
    "R specific hooks:",
    "https://github.com/lorenzwalthert/precommit."
  ))
}

#' Install pre-commit on your system with conda
#' @keywords internal
install_precommit_impl <- function() {
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
    out <- system2(path_pre_commit_exec(), "autoupdate")
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
