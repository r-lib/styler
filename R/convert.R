#' Convert from Rd to Markdown in roxygen2 comments
#'
#' Performs various substitutions in all `.R` files in a package.
#' Also attempts to enable Markdown support in `roxygen2` by adding a field to
#' `DESCRIPTION`.
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package
#'
#' @export
roxygen2md <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  withr::with_dir(pkg_root, roxygen2md_local())
}

roxygen2md_local <- function() {
  files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)

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

  roxygen_field <- desc::desc_get("Roxygen")
  roxygen_field_new <- "list(markdown = TRUE)"
  if (!identical(unname(roxygen_field), roxygen_field_new)) {
    if (is.na(roxygen_field)) {
      desc::desc_set("Roxygen" = roxygen_field_new)
    } else {
      message("Please update the Roxygen field in DESCRIPTION to include ", roxygen_field_new)
    }
  }

  invisible()
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
