#' Return info of last modified file
#' @param path_relative_cache The path of the cache relative to the cache root.
#' @param files Character vector with files to check.
#' @keywords internal
last_modified <- function(path_relative_cache, files = NULL) {
  all_files <- file.info(files)
  all_files[order(all_files$mtime, decreasing = TRUE)[1], ]
}