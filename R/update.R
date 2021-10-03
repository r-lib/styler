#' Update the pre-commit executable
#'
#' Updates the conda installation of the upstream framework pre-commit. This
#' does not update the R package `{precommit}` and only works if you choose
#' conda as your installation method. If you have problems updating, we suggest
#' deleting the conda environment `r-precommit` (if you are sure nothing but
#' pre-commit depend on it) and do a fresh installation with
#' [install_precommit()].
#' @return
#' The exit status of the conda update command (invisible).
#' @family executable managers
#' @export
update_precommit <- function() {
  old <- version_precommit()
  assert_reticulate_is_installed()
  if (!is_conda_installation()) {
    rlang::abort(paste0(
      "You can only update your pre-commit executable via the R API if you ",
      "chose the installation method via conda ",
      "(see https://lorenzwalthert.github.io/precommit/#installation). It ",
      "does not seem you installed via conda, because the path to the ",
      "executable in use is ", path_precommit_exec(), ". Please use the ",
      "update utilities of the installation method you chose. Alternatively, ",
      "you can uninstall with the utility of your installation method / ",
      "delete the executable and ",
      "run `precommit::install_precommit()` to switch to the conda ",
      "installation method."
    ))
  }
  exit_status <- update_impl()
  new <- version_precommit()
  if (new == old) {
    cli::cli_alert_info("Nothing to update, your version {old} is the latest available.")
  } else {
    cli::cli_alert_success("Successfully updated pre-commit from version {old} to {new}.")
  }
  invisible(exit_status)
}

#' Retrieve the version of the pre-commit executable used
#'
#' Retrieves the version of the pre-commit executable used.
#' @family executable managers
#' @export
version_precommit <- function() {
  out <- call_precommit("--version")
  if (out$exit_status == 0) {
    out <- trimws(gsub("pre-commit", "", out$stdout[1]))
  } else {
    communicate_captured_call(
      out,
      preamble = "Running `pre-commit --version` failed."
    )
  }
  out
}
