#' Set up a pre-comit config file for your git repo
#' 
#' Copies a default pre-commit-config.yaml in the root of your project and open 
#' it. Note that this function **does not install pre-commit** on your system, 
#' this needs to be done once in advance of calling this function. Follow the 
#' instructions [here](https://pre-commit.com/#install) and make sure the 
#' executable is on your path.
#' @param path_root The path to the root directory of your project.
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after 
#'   it's been placed in your repo.
#' @family helpers
#' @export
use_precommit <- function(path_root = here::here(),
                          force = FALSE, 
                          open = TRUE) {
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
  usethis::ui_todo(c(
    "Edit .precommit-hooks.yaml to (de)activate the hooks you want to use. ",
    "All available hooks: ", 
    "https://pre-commit.com/hooks.html", 
    "R specific hooks:",
    "https://github.com/lorenzwalthert/pre-commit-hooks."
  ))
  
  if (open && rstudioapi::isAvailable()) open_config()
}


#' Open pre-commit related files
#'
#' @details
#' * `open_config()`: opens the pre-commit config file.
#' * `open_wordlist()`: opens the the WORDLIST file for the check-spelling hook
#'   in inst/WORDLIST.
#' @family helpers
#' @export
open_config <- function() {
  rstudioapi::navigateToFile(".pre-commit-config.yaml")
}

#' @export
#' @rdname open_config
open_wordlist <- function() {
  rstudioapi::navigateToFile("inst/WORDLIST")
}



is_package <- function(base_path = here::here()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
                  error = function(e) NULL
  )
  !is.null(res)
}