#' Set up a pre-com
#' Copy a default pre-commit-config.yaml in the root of your project
#' @param hooks Ignored.
#' @param path_root The path to the root directory of your project.
#' @export
use_precommit <- function(hooks = NULL,
                                      path_root = here::here(),
                                      force = FALSE) {
  name_config <- ".pre-commit-config.yaml"
  if (!fs::file_exists(fs::path(path_root, name_config)) | force) {
    fs::file_copy(
      system.file(name_config, package = "precommithooks"),
      path_root, 
      overwrite = TRUE
    )
  } else {
    rlang::abort(paste0(
      "There is already a pre-commit configuration file in ",
      path_root,
      ". Use `force = TRUE` to replace .pre-commit-config.yaml"
    ))
  }
  usethis::ui_done("Copied .pre-commit-config.yaml {path_root}")
  if (is_package(path_root)) {
    usethis::write_union(fs::path(path_root, ".Rbuildignore"), name_config)
    usethis::ui_done("Added .pre-commit-config.yaml to your .Rbuildignore.")
  }
  usethis::ui_todo("Run `pre-commit install` in your terminal to get started.")
  usethis::ui_todo(paste0(
    "Run `pre-commit autoupdate` in your terminal to get the latest",
    "version of the hooks."
  ))
}


is_package <- function(base_path = here::here()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}
#' Open pre-commit related files
#'
#' @details
#' * `open_config()`: opens the pre-commit config file.
#' * `open_wordlist()`: opens the the WORDLIST file for the check-spelling hook.
#' @export
open_config <- function() {
  rstudioapi::navigateToFile(".pre-commit-config.yaml")
}

#' @export
open_wordlist <- function() {
  rstudioapi::navigateToFile("inst/WORDLIST")
}
