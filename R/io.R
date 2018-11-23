#' Apply a function to the contents of a file
#'
#' Transforms a file with a function.
#' @param path A vector with file paths to transform.
#' @param fun A function that returns a character vector.
#' @param write_back Whether or not the results of the transformation should
#'   be written back to the file.
#' @importFrom magrittr set_names
#' @keywords internal
transform_utf8 <- function(path, fun, write_back = TRUE) {
  map_lgl(path, transform_utf8_one, fun = fun, write_back = write_back) %>%
    set_names(path)
}

transform_utf8_one <- function(path, fun, write_back = write_back) {
  old <- xfun::read_utf8(path)
  tryCatch({
    new <- fun(old)
    if (write_back) {
      xfun::write_utf8(new, path)
    }
    !identical(unclass(old), unclass(new))
  }, error = function(e) {
    warning("When processing ", path, ": ", conditionMessage(e), call. = FALSE)
    NA
  })
}
