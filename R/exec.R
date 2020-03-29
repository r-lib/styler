#' Locate the pre-commit executable
#'
#' @param check_if_exists Whether or not to make sure the returned path also
#'  exists.
#' @export
path_precommit_exec <- function(check_if_exists = TRUE) {
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

#' @rdname path_precommit_exec
#' @export
path_pre_commit_exec <- function(check_if_exists = TRUE) {
  .Deprecated("path_precommit_exec", old = "path_pre_commit_exec")
  path_precommit_exec(check_if_exists = check_if_exists)
}

#' Derive the path to the pre-commit executable
#'
#' First check if there is an executable on the `$PATH`, then check if the
#' conda installation was used. Returns `""` if no executable is found. If not,
#' search other possible locations that are OS dependent.
#' @keywords internal
path_derive_precommit_exec <- function() {
  path <- path_derive_precommit_exec_path()
  if (path == "") {
    path <- path_derive_precommit_exec_conda()
  }
  if (path == "") {
    os <- tolower(Sys.info()[["sysname"]])
    if (os == "darwin") {
      path <- path_derive_precommit_exec_macOS()
    } else if (os == "windows") {
      path <- path_derive_precommit_exec_win()
    } else if (os == "linux") {
      path <- path_derive_precommit_exec_linux()
    }
  }
  path
}

path_derive_precommit_exec_linux <- function() {
  path_derive_precommit_exec_impl(
    "~/.local/bin" # pip: https://unix.stackexchange.com/questions/240037/why-did-pip-install-a-package-into-local-bin
  )
}

path_derive_precommit_exec_win <- function() {
  path_derive_precommit_exec_impl(fs::path_home("AppData/Roaming/Python/Scripts"))
}

path_derive_precommit_exec_macOS <- function() {
  c(
    fs::path(fs::dir_ls(path_if_exist("~/Library/Python/")), "bin"), # pip
    "/usr/local/bin" # homebrew
  ) %>%
    path_derive_precommit_exec_impl()
}


path_derive_precommit_exec_impl <- function(candidate) {
  assumed <- fs::path(candidate, "pre-commit")
  existant <- assumed[fs::file_exists(assumed)]
  if (length(existant) > 0) {
    existant[1]
  } else {
    ""
  }
}

#' Derive the pre-commit executable from the path
#'
#' Tries to derive the `pre-commit` executable from the `$PATH`.
#' Returns `""` if no executable is found.
#' @keywords internal
path_derive_precommit_exec_path <- function() {
  unname(Sys.which("pre-commit")[1])
}

path_derive_precommit_exec_conda <- function() {
  tryCatch(
    {
      ls <- reticulate::conda_list()

      path_reticulate <- fs::path_dir(ls[ls$name == "r-reticulate", "python"][1])
      derived <- fs::path(
        path_reticulate,
        ifelse(is_windows(), "Scripts", ""),
        ifelse(is_windows(), "pre-commit.exe", "pre-commit")
      )
      unname(ifelse(fs::file_exists(derived), derived, ""))
    },
    error = function(e) ""
  )
}
