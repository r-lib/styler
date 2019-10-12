#' Set up pre-commit
#' 
#' 
#' @details 
#' * installs pre-commit on your system with [install_precommit()].
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
#' @param skip_install Skip the installation with conda. We'll assume you have 
#'   installed the executable otherwise, preferrably as described in the 
#'   [official installation guide](https://pre-commit.com/#install). This might 
#'   make sense if you don't want to use conda. In that case, please set the 
#'   R option `precommithooks.executable` in your `.Rprofile` to make sure 
#'   this package finds the executable and you can perform other operations such 
#'   as [autoupdate()].
#' @family helpers
#' @export
use_precommit <- function(path_root = here::here(),
                          force = FALSE,
                          open = TRUE, 
                          skip_install = FALSE) {
  withr::with_dir(path_root, {
    install_system(skip_install, force)
    install_repo()
    use_precommit_config(force)
    autoupdate()
    if (open) {
      open_config(open)
    }
  })
}

install_system <- function(skip_install, force) {
  if (!skip_install) {
    if (!is_installed() || force) {
      usethis::ui_info("Installing pre-commit with conda into r-reticulate env.")
      install_precommit()
      usethis::ui_done("Sucessfully installed pre-commit on your system.")
      options(precommithooks.executable = derive_path_precommit_exec())
    }
  } else {
    usethis::ui_info("Assuming you have installed pre-commit yourself")
  }
}

install_repo <- function() {
  system2(find_pre_commit_exec(), "install")
  usethis::ui_done("Sucessfully installed pre-commit for this repo.")
}

use_precommit_config <- function(force) {
  name_config <- ".pre-commit-config.yaml"
  path_root <- getwd()
  if (!fs::file_exists(fs::path(name_config)) | force) {
    fs::file_copy(
      system.file(name_config, package = "precommithooks"),
      ".",
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
    usethis::write_union(".Rbuildignore", name_config)
    usethis::ui_done("Added .pre-commit-config.yaml to your .Rbuildignore.")
  }
  usethis::ui_todo(c(
    "Edit .precommit-hooks.yaml to (de)activate the hooks you want to use. ",
    "All available hooks: ",
    "https://pre-commit.com/hooks.html",
    "R specific hooks:",
    "https://github.com/lorenzwalthert/pre-commit-hooks."
  ))
  
}

#' Install pre-commit on your system with conda
#' @keywords internal
install_precommit <- function() {
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
#' @param check_if_exist Whether or not to make sure the returned path also 
#'  exists.
find_pre_commit_exec <- function(check_if_exists = TRUE) {
  final <- getOption("precommithooks.executable") %>%
    as.character()
  if (!check_if_exists) {
    return(final)
  }
  if (!fs::file_exists(final)) {
    rlang::abort(paste0(
      "pre-commit executable does not exist at ", 
      final,
      ". Please locate your pre-commit ",
      "executable and set the R option `precommithooks.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R."
    ))
  }
  final
}

derive_path_precommit_exec <- function(check_existance = FALSE) {
  ls <- reticulate::conda_list()
  derived <- fs::path(fs::path_dir(ls[ls == "r-reticulate", ]$python), "pre-commit")
  derived
  if (check_existance) {
    ifelse(fs::file_exists(derived), derived, "")
  } else {
    derived
  }
  
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
