#' @export
roxygen2md <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, roxygen2md_local())
}

roxygen2md_local <- function() {
  files <- dir(pattern = "[.][rR]$", recursive = TRUE)
  convert_local_links(files)
  convert_alien_links(files)
  convert_code(files)

  if (is.na(desc::desc_get("RoxygenNote"))) {
    desc::desc_set("RoxygenNote" = "list(markdown = TRUE)")
  } else {
    message("Please update the RoxygenNote field in DESCRIPTION to include list(markdown = TRUE)")
  }
}

convert_local_links <- function(files) {
  gsub_in_files(files, "[\\\\]code[{][\\\\]link[{]([^}]+)[}][}]", "[\\1()]")
}

convert_alien_links <- function(files) {
  gsub_in_files(files, "[\\\\]code[{][\\\\]link[[]([^]]+)[]][{]([^}]+)[}][}]", "[\1::\2()]")
}

convert_code <- function(files) {
  gsub_in_files(files, "[\\\\]code[{]([^{}]+)[}]", "`\\1`")
}

gsub_in_files <- function(files, search, replace, ...) {
  lapply(files, gsub_in_file, search, replace)
}

gsub_in_file <- function(file, search, replace, ...) {
  text <- readLines(file)
  new_text <- gsub(search, replace, text, ...)
  writeLines(new_text, file)
}
