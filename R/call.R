#' Make a call with [system2()] and capture the effects.
#' @param ... Arguments passed to [system2()].
#' @return
#' A list with:
#' * content of stderr
#' * content of stdout
#' * exit status
call_and_capture <- function(...) {
  stdout <- tempfile()
  writeLines("", stdout)
  stderr <- tempfile()
  writeLines("", stderr)
  exit_status <- suppressWarnings(
    system2(..., stdout = stdout, stderr = stderr)
  )
  stderr <- readLines(stderr)
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

install_repo <- function(path_root) {
  tmp1 <- tempfile()
  writeLines("", tmp1)
  tmp2 <- tempfile()
  writeLines("", tmp2)
  withr::with_dir(path_root, {
    out <- call_and_capture(path_precommit_exec(), "install")
    if (out$exit_status == 0) {
      usethis::ui_done("Sucessfully installed pre-commit for repo {fs::path_file(path_root)}.")
    } else {
      usethis::ui_oops("Failed to install pre-commit for repo {fs::path_file(path_root)}.")
      communicate_captured_call(out, preamble = "Problems during initialization:")
    }
  })
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
}
