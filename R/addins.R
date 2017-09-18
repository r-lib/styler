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
  text <- context$selection[[1]]$text
  if (all(nchar(text)) == 0) stop("No text selected")
  out <- style_text(text)
  pos <- rstudioapi::as.document_position(context$selection[[1]]$range$start)
  rng <- context$selection[[1]]$range
  rstudioapi::insertText(rng, "", id = context$id)
  rstudioapi::insertText(pos, paste0(out, collapse = "\n"), id = context$id)
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}
