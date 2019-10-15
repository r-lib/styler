#' Set up pre-commit
#'
#' @details
#' * installs pre-commit in your current directory with.
#' * sets up a template `.pre-commit-config.yaml`.
#' * autoupdates the template to make sure you get the latest versions of the
#'   hooks.
#' * Open the config file if RStudio is running.
#'
#' @param path_root The path to the root directory of your project.
#' @param force Whether to replace an existing config file.
#' @param open Whether or not to open the .pre-commit-config.yaml after
#'   it's been placed in your repo.
#' @family helpers
#' @export
use_precommit <- function(path_root = here::here(),
                          force = FALSE,
                          open = TRUE) {
  withr::with_dir(path_root, {
    if (!is_installed()) {
      rlang::abort(paste0(
        "pre-commit is not installed on your system. Please install it with ",
        "`precommit::install_precommit()` or an installation ",
        "method in the official installation guide ",
        "(https://pre-commit.com/#install)."
      ))
    }
    install_repo()
    use_precommit_config(force)
    autoupdate()
    if (open) {
      open_config()
    }
  })
}

install_system <- function() {
  if (!is_installed()) {
    usethis::ui_info("Installing pre-commit with conda into r-reticulate env.")
    install_precommit_impl()
    usethis::ui_done("Sucessfully installed pre-commit on your system.")
    usethis::ui_todo("To use it with this project, run `precommit::use_precommit()`")
    options(precommit.executable = derive_path_precommit_exec())
  }
}

#' Install pre-commit on your system.
#'
#' This installs pre-commit in the default conda environment of reticulate. It
#' will be available to use accross different git repositories.
#' @export
install_precommit <- function() {
  install_system()
}

install_repo <- function() {
  system2(find_pre_commit_exec(), "install")
  usethis::ui_done("Sucessfully installed pre-commit for this repo.")
}

use_precommit_config <- function(force) {
  name_origin <- "pre-commit-config.yaml"
  escaped_name_target <- "^\\.pre-commit-config\\.yaml$"
  name_target <- ".pre-commit-config.yaml"
  # workaround for RCMD CHECK warning about hidden top-level directories.
  path_root <- getwd()
  if (!fs::file_exists(fs::path(name_target)) | force) {
    fs::file_copy(
      system.file(name_origin, package = "precommit"),
      fs::path(".", name_target),
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
  if (is_package(".")) {
    usethis::write_union(".Rbuildignore", escaped_name_target)
    usethis::ui_done("Added .pre-commit-config.yaml to your .Rbuildignore.")
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

is_installed <- function() {
  fs::file_exists(find_pre_commit_exec(check_if_exists = FALSE))
}

#' Auto-update your hooks
#'
#' Runs `pre-commit autoupdate`.
#' @export
autoupdate <- function() {
  system2(find_pre_commit_exec(), "autoupdate")
  usethis::ui_done(paste0(
    "Ran `pre-commit autoupdate` to get the latest version of the hooks."
  ))
}

#' Locate the pre-comit executable
#'
#' @param check_if_exists Whether or not to make sure the returned path also
#'  exists.
find_pre_commit_exec <- function(check_if_exists = TRUE) {
  final <- getOption("precommit.executable") %>%
    as.character()
  if (!check_if_exists) {
    return(final)
  }
  if (!fs::file_exists(final)) {
    rlang::abort(paste0(
      "pre-commit executable does not exist at ",
      final,
      ". Please locate your pre-commit ",
      "executable and set the R option `precommit.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R."
    ))
  }
  final
}

derive_path_precommit_exec <- function() {
  tryCatch(
    {
      ls <- reticulate::conda_list()
      derived <- fs::path(fs::path_dir(ls[ls == "r-reticulate", ]$python[1]), "pre-commit")
      unname(ifelse(fs::file_exists(derived), derived, ""))
    },
    error = function(e) ""
  )
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
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(".pre-commit-config.yaml")
  } else {
    rlang::abort("Can't open if you don't have RStudio running.")
  }
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
