#' Make a call with [system2()] and capture the effects.
#' @param command The command to issue. A character string of length one.
#' @param args The command line arguments.
#' @param ... Arguments passed to [system2()].
#' @param wait Passed to [system2()].
#' @return
#' A list with:
#' * content of stderr
#' * content of stdout
#' * exit status
#' @keywords internal
call_and_capture <- function(command, args, ..., wait = TRUE) {
  if (length(command) != 1 | !inherits(command, "character")) {
    rlang::abort("The command must be a character string of length 1.")
  }
  stdout <- tempfile()
  writeLines("", stdout)
  stderr <- tempfile()
  writeLines("", stderr)
  if (!is.character(args)) {
    rlang::abort(paste0(
      "command line arguments must be a character vector, not of class `",
      class(args)[1], "`."
    ))
  }
  exit_status <- suppressWarnings(
    system2(command, args, ..., wait = wait, stdout = stdout, stderr = stderr)
  )
  # on Windows, there is no output when wait = FALSE
  if (is_windows() && !wait) {
    out <- list(
      stdout = "[cannot recover stdout for Windows when R option `precommit.block_install_hooks` is FALSE]",
      stderr = "[cannot recover stderr for Windows when R option `precommit.block_install_hooks` is FALSE]",
      exit_status = exit_status
    )
    return(out)
  }
  stderr <- readLines(stderr)
  if (isTRUE(any(grepl("error", stderr, ignore.case = TRUE)))) {
    # conda run has exit status 0 but stderr with ERROR, we need to set exit
    # code in that case.
    exit_status <- -999
  }

  if (exit_status != 0) {
    if (length(stderr) < 1) {
      stderr <- paste0(
        "Could not recover stderr. Run the following command to get the error",
        paste(...)
      )
    }
  }
  list(
    stdout = readLines(stdout),
    stderr = stderr,
    exit_status = exit_status
  )
}

#' Call pre-commit
#'
#' Either via `conda run` (because conda env needs to be activated in general to
#' ensure an executable to runs successfully) or, if the installation method was
#' not conda, as a plain bash command.
#' @param ... Arguments passed to the command line call `pre-commit`.
#' @param wait Passed to [base::system2()].
#' @keywords internal
call_precommit <- function(..., wait = TRUE) {
  if (is_conda_installation()) {
    call_and_capture(
      reticulate::conda_binary(),
      c("run", "-n", "r-precommit", path_precommit_exec(), ...),
      wait = wait
    )
  } else {
    call_and_capture(path_precommit_exec(), c(...), wait = wait)
  }
}

#' @param x The output of [call_and_capture()].
communicate_captured_call <- function(x, preamble = "") {
  if (x$exit_status != 0) {
    trans <- rlang::abort
  } else {
    trans <- rlang::warn
  }
  trans(paste0(preamble,
    "\nstderr: ",
    paste0(x$stderr, collapse = "\n"), "\n\nstdout: ",
    paste0(x$stdout, collapse = "\n"),
    collapse = "\n"
  ))
  x$exit_status
}
