#' Stylers for RStudio Addins
#'
#' Helper functions for styling via RStudio Addins.
#'
#' @section Auto-Save Option:
#' By default, both of the RStudio Addins will apply styling to the (selected)
#' file contents without saving changes. Automatic saving can be enabled by
#' setting the environment variable `save_after_styling` to `TRUE`.
#'
#' Consider setting this in your `.Rprofile` file if you want to persist
#' this setting across multiple sessions. Untitled files will always need to be
#' saved manually after styling.
#'
#' @name styler_addins
#' @family stylers
#' @seealso [Sys.setenv()]
NULL

#' @describeIn styler_addins Styles the active file with [tidyverse_style()] and
#'   `strict = TRUE`.
#' @keywords internal
style_active_file <- function() {
  transformer <- make_transformer(tidyverse_style(),
    include_roxygen_examples = TRUE
  )
  context <- get_rstudio_context()
  if (is_rmd_file(context$path)) {
    out <- transform_rmd(context$contents, transformer)
  } else if (is_plain_r_file(context$path) | is_unsaved_file(context$path)) {
    out <- try_transform_as_r_file(context, transformer)
  } else {
    stop("Can only style .R and .Rmd files.", call. = FALSE)
  }
  rstudioapi::modifyRange(
    c(1, 1, length(context$contents) + 1, 1),
    paste0(append(out, ""), collapse = "\n"), id = context$id
  )
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
}

#' Style a file as if it was an .R file
#'
#' If not successful, the file is most
#' likely not a .R file, so saving the file and try styling again will work if
#' the file is an .Rmd file. Otherwise, we can throw an error that the file must
#' be a .R or .Rmd file.
#' @param context The context from `styler:::get_rstudio_context()`.
#' @param transformer A transformer function most conveniently constructed with
#'   [make_transformer()].
#' @keywords internal
try_transform_as_r_file <- function(context, transformer) {
  tryCatch(
    transformer(context$contents),
    error = function(e) {
      preamble_for_unsaved <- paste(
        "Styling of unsaved files is only supported for R files with valid code.",
        "Please save the file (as .R or .Rmd) and make sure that the R code in it",
        "can be parsed. Then, try to style again."
      )

      if (context$path == "") {
        stop(paste0(preamble_for_unsaved, "The error was \n", e), call. = FALSE)
      } else {
        stop(e)
      }
    }
  )
}

#' @describeIn styler_addins Styles the highlighted selection in a `.R` or
#'   `.Rmd` file.
#' @keywords internal
style_selection <- function() {
  context <- get_rstudio_context()
  text <- context$selection[[1]]$text
  if (all(nchar(text) == 0)) stop("No code selected")
  out <- style_text(text)
  rstudioapi::modifyRange(
    context$selection[[1]]$range, paste0(out, collapse = "\n"), id = context$id
  )
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}
