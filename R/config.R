#' Initiate a pre-commit config file
#'
#' @param path_cp_config_from Path or URL to a `.pre-commit-config.yaml`. This
#'   config file will be hard-copied into `path_root`. If `NULL`, we check if
#'   `path_root` is a package or project directory using
#'   [rprojroot::find_package_root_file()], and resort to an appropriate default
#'   config. See section 'Copying an existing config file'.
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after
#'   it's been placed in your repo. The default is `TRUE` when working in
#'   RStudio. Otherwise, we recommend manually inspecting the file.
#' @param verbose Whether or not to communicate what's happening.
#' @section Copying an existing config file:
#' You can use an existing `.pre-commit-config.yaml` file when initializing
#' pre-commit with [use_precommit()] using the argument `path_cp_config_from` to
#' copy an existing config file into your repo. This argument defaults to the R
#' option `precommit.path_cp_config_from`, so you may want to set this option in
#' your `.Rprofile` for convenience. Note that this is **not** equivalent
#' to the `--config` option in the CLI command `pre-commit install` and similar,
#' which do *not* copy a config file into a project root (and allow to put it
#' under version control), but rather link it in some more or less transparent
#' way.
#' @inheritParams fallback_doc
#' @export
use_precommit_config <- function(path_cp_config_from = getOption("precommit.path_cp_config_from"),
                                 force,
                                 open = rstudioapi::isAvailable(),
                                 path_root = here::here(),
                                 verbose = FALSE) {
  path_cp_config_from <- set_path_cp_config_from(
    path_cp_config_from,
    path_root = path_root,
    verbose = verbose
  )
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
    usethis::ui_info(paste0(
      "There is already a pre-commit configuration file in ",
      path_root,
      ". Use `force = TRUE` to replace .pre-commit-config.yaml."
    ))
  }

  if (is_package(path_root)) {
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

#' Set the location to a config file
#'
#' If a remote location is specified, the file is downloaded to a temporary
#' location and the path to this location is returned. If `NULL`, we'll resort
#' to a default config. We'll perform some checks on the existence of the file
#' too.
#' @keywords internal
set_path_cp_config_from <- function(path_cp_config_from,
                                    path_root,
                                    verbose = TRUE) {
  if (is_url(path_cp_config_from)) {
    if (verbose) {
      usethis::ui_info("Downloading remote config from {path_cp_config_from}.")
    }
    tmp <- tempfile()

    target <- fs::path_ext_set(tmp, fs::path_ext(path_cp_config_from))
    utils::download.file(path_cp_config_from, target, quiet = TRUE)
    rlang::with_handlers(
      yaml::read_yaml(target, fileEncoding = "UTF-8"),
      error = function(e) {
        rlang::abort(paste0(
          "Provided config file is not a valid yaml file. ",
          "Please provide a valid yaml file. The error was:", conditionMessage(e)
        ))
      }
    )
    return(target)
  }
  if (is.null(path_cp_config_from)) {
    # workaround for R CMD CHECK warning about hidden top-level directories.
    name_origin <- ifelse(is_package(path_root),
      "pre-commit-config-pkg.yaml",
      "pre-commit-config-proj.yaml"
    )
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
  if (verbose) {
    usethis::ui_info("Using local conifig from {path_cp_config_from}.")
  }
  path_cp_config_from
}


example_remote_config <- function() {
  "https://raw.githubusercontent.com/lorenzwalthert/precommit/master/inst/pre-commit-config.yaml"
}
