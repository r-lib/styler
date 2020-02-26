#' Locate the pre-comit executable
#'
#' @param check_if_exists Whether or not to make sure the returned path also
#'  exists.
#' @export
path_pre_commit_exec <- function(check_if_exists = TRUE) {
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
      # path <- path_derive_precommit_exec_mac()
    } else if (os == "windows") {
      # path <- path_derive_precommit_exec_windows()
    } else if (os == "linux") {
      path <- path_derive_precommit_exec_linux()
    }
  }
  path
}

# https://unix.stackexchange.com/questions/240037/why-did-pip-install-a-package-into-local-bin
path_derive_precommit_exec_linux <- function() {
  assumed <- fs::path("~/.local/bin", "pre-commit")
  if (fs::file_exists(assumed)) {
    assumed
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
