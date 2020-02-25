#' Set up pre-commit
#'
#' Get started.
#' @section When to call this function?:
#'
#' * You want to add pre-commit support to a git repo. This involves adding
#'   a pre-commit config file and making sure git will call the hooks before
#'   the next commit.
#' * You use a repo that has such a config file but for pre-commit to become
#'   active, you need to make sure git knows that it should call pre-commit.
#'
#' @section What it does the funciton do?:
#' * installs pre-commit in your current directory.
#' * sets up a template `.pre-commit-config.yaml`.
#' * autoupdates the template to make sure you get the latest versions of the
#'   hooks.
#' * Open the config file if RStudio is running.
#'
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after
#'   it's been placed in your repo.
#' @inheritParams fallback_doc
#' @family helpers
#' @export
use_precommit <- function(force = FALSE,
                          open = TRUE,
                          path_root = here::here()) {
  withr::with_dir(path_root, {
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
    install_repo()
    use_precommit_config(force, path_root)
    autoupdate(path_root)
    if (open) {
      open_config(path_root)
    }
  })
}

use_precommit_config <- function(force, path_root = here::here()) {
  name_origin <- "pre-commit-config.yaml"
  escaped_name_target <- "^\\.pre-commit-config\\.yaml$"
  name_target <- ".pre-commit-config.yaml"
  # workaround for RCMD CHECK warning about hidden top-level directories.
  if (!fs::file_exists(fs::path(name_target)) | force) {
    fs::file_copy(
      system.file(name_origin, package = "precommit"),
      fs::path(".", name_target),
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
