#' Return info of last modified file
#' @param path_relative_cache The path of the cache relative to the cache root.
#' @param files Character vector with files to check.
#' @keywords internal
last_modified <- function(path_relative_cache, files = NULL) {
  all_files <- file.info(files)
  all_files[order(all_files$mtime, decreasing = TRUE)[1], ]
}

#' Check if a file is cached
#' @param x An object, often the last modification time for hooks like 
#'   `roxygenize`.
#' @inheritParams last_modified
#' @keywords internal
is_cached <- function(x, path_relative_cache) {
  !is.null(R.cache::findCache(
    key = x,
    path = path_relative_cache
  ))
}