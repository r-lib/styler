#' @export
roxygen2md <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, roxygen2md_local())
}

roxygen2md_local <- function() {
  files <- dir(pattern = "[.][rR]$", recursive = TRUE)

  changed <- c(
    convert_local_links(files),
    convert_alien_links(files),
    convert_code(files),
    NULL
  )
  changed <- sort(unique(changed))

  if (length(changed) > 0) {
    message("Changed ", length(changed), " files: ", paste(changed, collapse = ", "), ". Please review the changes carefully!")
  } else {
    message("No files changed")
  }

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
  gsub_in_files(files, "[\\\\]code[{][\\\\]link[[]([^]]+)[]][{]([^}]+)[}][}]", "[\\1::\\2()]")
}

convert_code <- function(files) {
  gsub_in_files(files, "[\\\\]code[{]([^{}]+)[}]", "`\\1`")
}

gsub_in_files <- function(files, search, replace, ...) {
  changed <- BBmisc::vlapply(files, gsub_in_file, search, replace, ...)
  files[changed]
}

gsub_in_file <- function(file, search, replace, ...) {
  text <- readLines(file)
  new_text <- gsub(search, replace, text, ...)
  writeLines(new_text, file)
  any(text != new_text)
}
