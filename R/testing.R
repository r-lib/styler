#' Run a test
#'
#' Tests for the executables used as pre-commit hooks via `entrypoint` in
#' `.pre-commit-config.yaml`. Set's the env variable `R_PRECOMMIT_HOOK_ENV` to
#' when running.
#' @details
#' Two potential outcomes of a hooks are pass or fail. This is reflected on the
#' level of the executable: Fail means the executable fails or the file is
#' changed. Pass means the executable succeeds and the file is unchanged.
#' We check if the executable passes as follows:
#'
#' * If we expect success (by setting `error_msg` to `NULL`), we make sure
#'   nothing was written to sterr and the file content does not change.
#'
#' * If we expect failure, it can be due to changed file or due to failed
#'   executable. To check for failed executable, we set `error_msg` to
#'   the message we expect. To check changed file content, we set `error_msg` to
#'   `NA`.
#'
#' @param hook_name The name of the hook in `inst/hooks/exported/`, without
#'   file extension.
#' @param file_name The file to test in `tests/in` (without extension). Can be
#'   a named vector of length one where the name is the target location relative
#'   to the temporary location and the value is the source of the file.
#' @param suffix The suffix of `file_name`.
#' @param file_transformer A function that takes the file names as input and is
#'   ran right before the hook script is invoked, returning the path to the
#'   files, potentially modified (if renamed). This can be useful if you need to
#'   make in-place modifications to the file, e.g. to test hooks that operate on
#'   `.Rprofile`. You can't have different names for different tests on that
#'   file because it must be called `.Rprofile` all the time. And R CMD check
#'   seems to remove hidden files, so we must also rename it. The transformation
#'   is also applied to a temp copy of the reference file before a comparison is
#'   made.
#' @inheritParams run_test_impl
#' @keywords internal
run_test <- function(hook_name,
                     file_name = hook_name,
                     suffix = ".R",
                     error_msg = NULL,
                     msg = NULL,
                     cmd_args = NULL,
                     artifacts = NULL,
                     file_transformer = function(files) files,
                     env = character()) {
  withr::local_envvar(list(R_PRECOMMIT_HOOK_ENV = "1"))
  path_executable <- fs::dir_ls(system.file(
    fs::path("hooks", "exported"),
    package = "precommit"
  ), regexp = paste0("/", hook_name))
  if (length(path_executable) != 1) {
    rlang::abort("Failed to derive hook path")
  }
  path_candidate <- paste0(testthat::test_path("in", file_name), suffix) %>%
    ensure_named(names(file_name), fs::path_file)
  run_test_impl(
    path_executable, path_candidate,
    error_msg = error_msg,
    msg = msg,
    cmd_args = cmd_args,
    artifacts = ensure_named(artifacts),
    file_transformer = file_transformer,
    env = env
  )
}

#' Implement a test run
#'
#' @param path_executable The path to the executable bash script.
#' @param path_candidate The path to a file that should be modified by the
#'   executable.
#' @param artifacts Path with artifact files to copy to the temp directory root where
#'   the test is run. If you don't target the root, this can be a named vector
#'   of length one where the name is the target location relative to the
#'   temporary location and the value is the source of the file.
#' @param error_msg An expected error message. If no error is expected, this
#'   can be `NULL`. In that case, the `comparator` is applied.
#' @param msg The expected stdout message. If `NULL`, this check is omitted.
#' @param cmd_args More arguments passed to the file. Pre-commit handles it as
#'   described [here](https://pre-commit.com/#arguments-pattern-in-hooks).
#' @param env The environment variables to set with [base::system2()].
#' @keywords internal
run_test_impl <- function(path_executable,
                          path_candidate,
                          error_msg,
                          msg,
                          cmd_args,
                          artifacts,
                          file_transformer,
                          env) {
  expect_success <- is.null(error_msg)
  tempdir <- fs::dir_create(fs::file_temp())
  copy_artifacts(artifacts, tempdir)
  # if name set use this, otherwise put in root
  path_candidate_temp <- fs::path(tempdir, names(path_candidate))
  fs::dir_create(fs::path_dir(path_candidate_temp))
  fs::file_copy(path_candidate, path_candidate_temp, overwrite = TRUE)
  path_candidate_temp <- withr::with_dir(
    tempdir,
    file_transformer(path_candidate_temp)
  )
  path_stderr <- tempfile()
  path_stdout <- tempfile()
  exit_status <- hook_state_create(
    tempdir,
    path_candidate_temp,
    path_executable,
    cmd_args,
    path_stdout,
    path_stderr,
    env
  )
  hook_state_assert(
    path_candidate,
    tempdir,
    path_candidate_temp,
    file_transformer,
    path_stdout,
    path_stderr,
    expect_success,
    error_msg,
    msg,
    exit_status
  )
}


#' Create a hook state
#'
#' Runs the hook script to create a hook state, i.e. exit code, transformed
#' files and emitted messages of the hook run.
#' @keywords internal
hook_state_create <- function(tempdir,
                              path_candidate_temp,
                              path_executable,
                              cmd_args,
                              path_stdout,
                              path_stderr,
                              env) {
  withr::local_dir(tempdir)
  files <- fs::path_rel(path_candidate_temp, tempdir)
  # https://r.789695.n4.nabble.com/Error-message-Rscript-should-not-be-used-without-a-path-td4748071.html
  system2(paste0(Sys.getenv("R_HOME"), "/bin/Rscript"),
    args = c(path_executable, cmd_args, files),
    stderr = path_stderr, stdout = path_stdout, env = env
  )
}

#' Check if the hook produced what you want
#'
#' Match the resulting state after the hook run with the expected state
#' @keywords internal
hook_state_assert <- function(path_candidate,
                              tempdir,
                              path_candidate_temp,
                              file_transformer,
                              path_stdout,
                              path_stderr,
                              expect_success,
                              error_msg,
                              msg,
                              exit_status) {
  purrr::map2(path_candidate, path_candidate_temp,
    hook_state_assert_one,
    tempdir = tempdir,
    file_transformer = file_transformer,
    path_stdout = path_stdout,
    path_stderr = path_stderr,
    expect_success = expect_success,
    error_msg = error_msg,
    msg = msg,
    exit_status = exit_status
  )
}

hook_state_assert_one <- function(path_candidate,
                                  tempdir,
                                  path_candidate_temp,
                                  file_transformer,
                                  path_stdout,
                                  path_stderr,
                                  expect_success,
                                  error_msg,
                                  msg,
                                  exit_status) {
  candidate <- readLines(path_candidate_temp)
  path_temp <- tempfile()
  fs::file_copy(path_candidate, path_temp)
  path_temp <- withr::with_dir(
    tempdir,
    file_transformer(path_temp)
  )
  reference <- readLines(path_temp)
  if (expect_success) {
    # file not changed + no stderr
    contents <- readLines(path_stderr)
    if (exit_status != 0) {
      testthat::fail("Expected: No error. Found:", contents)
    }
    testthat::expect_equivalent(candidate, reference)
    if (!is.null(msg)) {
      contents <- readLines(path_stdout)
      testthat::expect_match(
        paste(contents, collapse = "\n"), msg,
        fixed = TRUE
      )
    }
  } else if (!expect_success) {
    # either file changed or stderr
    if (is.na(error_msg)) {
      if (identical(candidate, reference)) {
        testthat::fail(paste0(
          path_candidate, " and ", path_candidate_temp,
          " are not supposed to be identical but they are"
        ))
      }
    } else {
      contents <- readLines(path_stderr)
      testthat::expect_match(
        paste(contents, collapse = "\n"), error_msg,
        fixed = TRUE
      )
      testthat::expect_false(exit_status == 0)
    }
  }
}

on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

#' The testing environment does not use a conda environment if the env variable
#' PRECOMMIT_INSTALLATION_METHOD is not 'conda'.
#' @keywords internal
not_conda <- function() {
  Sys.getenv("PRECOMMIT_INSTALLATION_METHOD") != "conda"
}

#' Testing utilities
#'
#' Similar to the `local_()` family from `{withr}`, this function creates a
#' temporary directory and optionally initiates git and pre-commit in it.
#' @inheritParams withr::local_tempdir
#' @param git Whether or not to init git in the local directory.
#' @param use_precommmit Whether or not to [use_precommit()].
#' @keywords internal
local_test_setup <- function(git = TRUE,
                             use_precommit = FALSE,
                             package = FALSE,
                             quiet = TRUE,
                             ...,
                             .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(dir, .local_envir = .local_envir)
  if (quiet) {
    withr::local_options("usethis.quiet" = TRUE, .local_envir = .local_envir)
  }
  if (git) {
    git2r::init()
    git2r::config(
      user.name = "testthat",
      user.email = "agent@testthat.com",
      core.autocrlf = "true"
    )
    withr::defer(fs::dir_delete(fs::path(dir, ".git")), envir = .local_envir)
  }
  if (use_precommit) {
    suppressMessages(use_precommit(..., root = dir))
  }
  if (package) {
    usethis::create_package(dir)
    withr::local_dir(dir)
    usethis::proj_set(dir)
    usethis::use_testthat()
  }

  dir
}

#' Generate a random package name that is not installed
#' @param n The number of times we should try
#' @keywords internal
generate_uninstalled_pkg_name <- function(n = 10) {
  additional_pkg <- paste0("package", digest::digest(Sys.time()))
  if (rlang::is_installed(additional_pkg)) {
    if (n > 0) {
      generate_uninstalled_pkg_name(n - 1)
    } else {
      rlang::abort("could not find a package name that was not yet installed")
    }
  } else {
    additional_pkg
  }
}

generate_uninstalled_pkg_call <- function(n = 10) {
  paste0(generate_uninstalled_pkg_name(n), "::x")
}


#' Copy some file to the test directory that must be present, but are not
#' passed to the hook as a file argument.
#' @param artifacts Artifacts to copy.
#' @param tempdir The temporary directory.
#' @keywords internal
copy_artifacts <- function(artifacts, tempdir) {
  if (!is.null(artifacts)) {
    paths_artifacts <- fs::path(tempdir, names(artifacts))
    new_dirs <- fs::path_dir(paths_artifacts)
    fs::dir_create(new_dirs)
    fs::file_copy(artifacts, paths_artifacts, overwrite = TRUE)
  }
}
