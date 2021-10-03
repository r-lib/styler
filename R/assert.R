assert_is_git_repo <- function(root) {
  if (is.null(git2r::discover_repository(root))) {
    rlang::abort(paste0(
      "The directory ", root, " is not a git repo. Please navigate to ",
      root, " and init git in ",
      "this directory with `$ git init` from the command line or ",
      "`> usethis::use_git()` from the R prompt."
    ))
  }
}

assert_is_installed <- function() {
  if (!is_installed()) {
    rlang::abort(paste0(
      "pre-commit is not installed on your system (or we can't find it).\n\n",
      "If you have it installed and you know where it is, please set the R option ",
      "`precommit.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R. ",
      "If you think this is a standard location, please open an issue on GitHub ",
      "so we can auto-detect this location in the future and spare new users some ",
      "set-up troubles.\n\n",
      "If you don't know where the executable is stored, go back to the log output ",
      "that resulted from the installation of pre-commit for hints. If you found ",
      "it and you think it's a standard location, please open an issue on GitHub ",
      "so we can auto-detect this location in the future and spare unexpereienced ",
      "users some trouble.\n\n",
      "In case you are totally lost with these messages, you can most likely ",
      "solve the problems with just using the conda installation method, see ",
      "https://lorenzwalthert.github.io/precommit/ for how to do this."
    ))
  }
}

assert_correct_upstream_repo_url <- function() {
  if (upstream_repo_url_is_outdated()) {
    cli::cli_alert_info(c(
      "The repo https://github.com/lorenzwalthert/pre-commit-hooks ",
      "has moved to ", hooks_repo, ". ",
      "Please fix the URL in .pre-commit-config.yaml, ",
      "most confortably with `precommit::open_config()`."
    ))
  }
}
