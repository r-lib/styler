#' Style the active file
#'
#' Helper function for RStudio Addin.
style_active_file <- function() {
  context <- find_active_context()
  style_file(context$path, style = tidyverse_style)
}


#' Style the highlighted region
#'
#' Helper function for RStudio Addin. This function is complicated because of
#' one thing: You can highlight also just parts of lines.
#' @importFrom rlang seq2
style_active_region <- function() {
  context <- get_rstudio_context()
  out <- style_text(context$selection[[1]]$text)
  rstudioapi::modifyRange(context$selection[[1]]$range, out, id = context$id)
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}
