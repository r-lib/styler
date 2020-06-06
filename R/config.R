#' Initiate a pre-commit config file
#'
#' @param config_source Path or URL to a `.pre-commit-config.yaml`. This
#'   config file will be hard-copied into `root`. If `NULL`, we check if
#'   `root` is a package or project directory using
#'   [rprojroot::find_package_root_file()], and resort to an appropriate default
#'   config. See section 'Copying an existing config file'.
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after
#'   it's been placed in your repo. The default is `TRUE` when working in
#'   RStudio. Otherwise, we recommend manually inspecting the file.
#' @param verbose Whether or not to communicate what's happening.
#' @section Copying an existing config file:
#' You can use an existing `.pre-commit-config.yaml` file when initializing
#' pre-commit with [use_precommit()] using the argument `config_source` to
#' copy an existing config file into your repo. This argument defaults to the R
#' option `precommit.config_source`, so you may want to set this option in
#' your `.Rprofile` for convenience. Note that this is **not** equivalent
#' to the `--config` option in the CLI command `pre-commit install` and similar,
#' which do *not* copy a config file into a project root (and allow to put it
#' under version control), but rather link it in some more or less transparent
#' way.
#' @inheritParams fallback_doc
#' @return
#' Character vector of length one with the path to the config file used.
#' @examples
#' \dontrun{
#' use_precommit_config()
#' }
#' @export
use_precommit_config <- function(config_source = getOption("precommit.config_source"),
                                 force = FALSE,
                                 open = rstudioapi::isAvailable(),
                                 verbose = FALSE,
                                 root = here::here()) {
  config_source <- set_config_source(
    config_source,
    root = root,
    verbose = verbose
  )
  escaped_name_target <- "^\\.pre-commit-config\\.yaml$"
  name_target <- ".pre-commit-config.yaml"
  if (!fs::file_exists(fs::path(root, name_target)) | force) {
    fs::file_copy(
      config_source,
      fs::path(root, name_target),
      overwrite = TRUE
    )
    usethis::ui_done("Copied .pre-commit-config.yaml to {root}")
  } else {
    usethis::ui_info(paste0(
      "There is already a pre-commit configuration file in ",
      root,
      ". Use `force = TRUE` to replace .pre-commit-config.yaml."
    ))
  }

  if (is_package(root)) {
    usethis::write_union(".Rbuildignore", escaped_name_target)
  }
  usethis::ui_todo(c(
    "Edit .precommit-hooks.yaml to (de)activate the hooks you want to use. ",
    "All available hooks: ",
    "https://pre-commit.com/hooks.html",
    "R specific hooks:",
    "https://github.com/lorenzwalthert/precommit."
  ))
  config_source
}

#' Set the location to a config file
#'
#' If a remote location is specified, the file is downloaded to a temporary
#' location and the path to this location is returned. If `NULL`, we'll resort
#' to a default config. We'll perform some checks on the existence of the file
#' too.
#' @keywords internal
set_config_source <- function(config_source,
                              root,
                              verbose = TRUE) {
  if (is_url(config_source)) {
    if (verbose) {
      usethis::ui_info("Downloading remote config from {config_source}.")
    }
    tmp <- tempfile()

    target <- fs::path_ext_set(tmp, fs::path_ext(config_source))
    utils::download.file(config_source, target, quiet = TRUE)
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
  if (is.null(config_source)) {
    # workaround for R CMD CHECK warning about hidden top-level directories.
    name_origin <- ifelse(is_package(root),
      "pre-commit-config-pkg.yaml",
      "pre-commit-config-proj.yaml"
    )
    config_source <- system.file(name_origin, package = "precommit")
  }
  if (!fs::file_exists(config_source)) {
    rlang::abort(paste0(
      "File ", config_source, " does not exist. Please use the ",
      "argument `config_source` to provide a path to an existing ",
      "`.pre-commit.yaml` or `NULL` to use the template config."
    ))
  }
  file_type <- as.character(fs::file_info(config_source)$type)
  if (!(file_type %in% c("file", "symlink"))) {
    rlang::abort(paste0(
      "File ", config_source, " must be a file or a symlink, not a ",
      file_type, ". Please change the argument `config_source` ",
      "accordingly."
    ))
  }
  if (verbose) {
    usethis::ui_info("Using local conifig from {config_source}.")
  }
  config_source
}


example_remote_config <- function() {
  "https://raw.githubusercontent.com/lorenzwalthert/precommit/master/inst/pre-commit-config-proj.yaml"
}
