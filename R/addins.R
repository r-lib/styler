
#' Stylers for RStudio Addins
#'
#' Helper functions for styling via RStudio Addins.
#'
#' @section Auto-Save Option:
#' By default, both of the RStudio Addins will apply styling to the (selected)
#' file contents without saving changes. Automatic saving can be enabled by
#' setting the environment variable \code{save_after_styling} to \code{TRUE}.
#'
#' Consider setting this in your \code{.Rprofile} file if you want to persist
#' this setting across multiple sessions. Untitled files will always need to be
#' saved manually after styling.
#'
#' @name styler_addins
#' @family stylers
#' @seealso [Sys.setenv()]
NULL

#' @describeIn styler_addins Styles the active file
style_active_file <- function() {
  context <- get_rstudio_context()
  out <- style_text(context$contents)
  rstudioapi::modifyRange(
    c(1, 1, length(out) + 1, 1),
    paste0(out, collapse = "\n"), id = context$id
  )
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
}



#' @describeIn styler_addins Styles the highlighted region
#' @importFrom rlang seq2
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
