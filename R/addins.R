#' Style the active file
#'
#' Helper function for RStudio Addin.
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


#' Style the highlighted region
#'
#' Helper function for RStudio Addin. This function is complicated because of
#' one thing: You can highlight also just parts of lines.
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
