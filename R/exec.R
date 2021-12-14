#' Locate the pre-commit executable
#'
#' [path_precommit_exec()] simply reads the R option `precommit.executable`,
#' [path_pre_commit_exec()] is the old spelling and deprecated.
#'
#' @param check_if_exists Whether or not to make sure the returned path also
#'  exists.
#' @return
#'  A character vector of length one with the path to the pre-commit executable.
#' @seealso
#' [path_derive_precommit_exec()] for the heuristic to derive it from scratch.
#' @examples
#' \dontrun{
#' path_precommit_exec()
#' }
#' @export
path_precommit_exec <- function(check_if_exists = TRUE) {
  final <- getOption("precommit.executable") %>%
    as.character()
  if (!check_if_exists) {
    return(final)
  }
  if (!file_exists(final)) {
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
#' @examples
#' \dontrun{
#' path_pre_commit_exec()
#' }
#' @export
path_pre_commit_exec <- function(check_if_exists = TRUE) {
  .Deprecated("path_precommit_exec", old = "path_pre_commit_exec")
  path_precommit_exec(check_if_exists = check_if_exists)
}

#' Derive the path to the pre-commit executable
#'
#' Returns "" if search was not successful, the path otherwise.
#' @section Heuristic:
#' - First check if there is an executable on the `$PATH` using
#'   [path_derive_precommit_exec_path()]
#' - Search os dependent for other possible locations for common installation
#'   methods.
#' - If not, check if we can find one in a conda environment with
#'   [path_derive_precommit_exec_conda()]. Do this last as it's the slowest.
#' @keywords internal
path_derive_precommit_exec <- function() {
  path <- path_derive_precommit_exec_path()
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "darwin") {
    path <- c(path, path_derive_precommit_exec_macOS())
  } else if (os == "windows") {
    path <- c(path, path_derive_precommit_exec_win())
  } else if (os == "linux") {
    path <- c(path, path_derive_precommit_exec_linux())
  }
  path <- unique(path[path != ""])
  if (length(path) == 0) {
    path_derive_precommit_exec_conda()
  } else if (length(path) == 1) {
    path
  } else {
    path_warn_multiple_execs(path)
    path[1]
  }
}

#' Find an executable
#'
#' Evaluates if the pre-commit executable exists in one or more candidate
#' locations. If so, return one, else return the empty string
#' @param candidate A directory to check for the pre-commit executable. The
#'   directory may also not exist.
#' @keywords internal
path_derive_precommit_exec_impl <- function(candidate) {
  existant <- path_candidate_to_actual(candidate)
  if (length(existant) >= 1) {
    existant
  } else {
    ""
  }
}

path_warn_multiple_execs <- function(paths) {
  rlang::warn(paste0(
    "We detected multiple pre-commit executables. This is likely ",
    "going to get you into trouble in the future, e.g. when you want to ",
    "upgrade, as you easily loose track of different versions. We strongly ",
    "suggest to only keep one pre-commit executable and uninstall (or delete) ",
    "the other ones. Here are the locations where we detected executables:\n\n",
    "- ", paste0(paths, collapse = "\n- "), "\n\n",
    "Note that in all local repos where you used pre-commit, the executable ",
    "path is hardcoded (in .git/hooks/pre-commit). Deleting a referenced ",
    "executable will break pre-commit for that local repo. A simple ",
    "re-initialization with `precommit::use_precommit()` or ",
    "`$ pre-commit install` will fix this for pre-commit hooks.\n\nIf you ",
    "have hook types other than pre-commit such as pre-push (you can check ",
    "which files are in .git/hooks/), you need to name the type and use the ",
    "command line, i.e. `$ pre-commit install -t pre-push`. "
  ))
}

path_candidate_to_actual <- function(candidate) {
  assumed <- fs::path(candidate, precommit_executable_file())
  assumed[file_exists(assumed)]
}

path_derive_precommit_exec_linux <- function() {
  path_derive_precommit_exec_impl(
    path_if_exist(fs::path_home(".local/bin")) # 18.04 and 16.04 with pip3.
  )
}

path_derive_precommit_exec_win <- function() {
  path_derive_precommit_exec_impl(c(
    path_derive_precommit_exec_win_python3plus_base(), # Python3+
    fs::path_home("AppData/Roaming/Python/Scripts") # default python
  ))
}

#' Where are executables on Windows for Python 3 and higher?
#'
#' Heuristic to determine the directory where the pre-commit executable on
#' Windows lives for Python versions 3 and above.
#' @keywords internal
path_derive_precommit_exec_win_python3plus_base <- function() {
  # exclude default Python
  path_derive_precommit_exec_win_python3plus_candidates() %>%
    sort(decreasing = TRUE) %>%
    fs::path("Scripts")
}

# Only reason to capsule this: mock test.
path_derive_precommit_exec_win_python3plus_candidates <- function() {
  fs::dir_ls(path_if_exist(fs::path_home("AppData/Roaming/Python/")), regexp = "Python[0-9]+$")
}


path_derive_precommit_exec_macOS <- function() {
  path_derive_precommit_exec_macOS_candidates() %>%
    path_derive_precommit_exec_impl()
}

path_derive_precommit_exec_macOS_candidates <- function() {
  candidate_system <- fs::dir_ls(
    path_if_exist("/Library/Frameworks/Python.framework/Versions/"),
    # avoid alias to /Library/Frameworks/Python.framework/Versions/Current/
    regexp = "[0-9]$"
  )

  candidate_user <- fs::dir_ls(path_if_exist("~/Library/Python/"))
  c(
    fs::path(sort(candidate_user, decreasing = TRUE), "bin"), # pip
    fs::path(sort(candidate_system, decreasing = TRUE), "bin"),
    "/usr/local/bin", # homebrew
    "/opt/homebrew/bin" # homebrew Apple Silicon (M1 chip)
  )
}

#' Derive the pre-commit executable from the path
#'
#' Tries to derive the `pre-commit` executable from the `$PATH`.
#' Returns `""` if no executable is found.
#' @keywords internal
path_derive_precommit_exec_path <- function() {
  unname(Sys.which(precommit_executable_file()))
}

#' Derive the path to the conda pre-commit executable
#'
#' Only checks the conda env `r-precommit`.
#' If we can't find the executable, the empty string is returned.
#' @keywords internal
path_derive_precommit_exec_conda <- function() {
  path <- path_derive_precommit_exec_conda_impl("r-precommit")
  if (path == "") {
    path <- path_derive_precommit_exec_conda_impl("r-reticulate")
    if (path != "") {
      rlang::warn(paste0(
        "The R packae {precommit} now requires the executable to live ",
        "in the conda environment r-precommit, not r-reticulate anymore ",
        "where it is currently installed. ",
        "Please run `precommit::install_precommit(force = TRUE)` to re-install with conda ",
        "or choose another installation method as described in the README. To save ",
        "space on disk, you probably want to delete the pre-commit executable at ",
        path, " and the package sources in the ",
        "conda environment r-reticulate with ",
        "`reticulate::conda_remove('r-reticulate', 'pre-commit')`."
      ))
    }
  }
  path
}

path_derive_precommit_exec_conda_impl <- function(conda_env) {
  tryCatch(
    {
      ls <- reticulate::conda_list()
      path_reticulate <- fs::path_dir(ls[ls$name == conda_env, "python"][1])
      derived <- fs::path(
        path_reticulate,
        ifelse(is_windows(), "Scripts", ""),
        precommit_executable_file()
      )
      unname(ifelse(file_exists(derived), derived, ""))
    },
    error = function(e) ""
  )
}

#' The name of the executable file
#'
#' This is platform dependent.
#' @keywords internal
precommit_executable_file <- function() {
  ifelse(is_windows(), "pre-commit.exe", "pre-commit")
}
