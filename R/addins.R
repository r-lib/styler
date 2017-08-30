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
  context <- find_active_context()
  if (all(context$start == context$end)) stop("No region selected")
  all_text <- utf8::read_lines_enc(context$path)
  styled_expr <- style_region(all_text, context)
  neighbourhood <- extract_neighbourhood(all_text, context)

  styled_lines <- complete_styled_expr(context, styled_expr, neighbourhood)

  merged_text <- append(neighbourhood$body, styled_lines, context$start[1] - 1)
  utf8::write_lines_enc(merged_text, context$path)
}

#' Helper to extract and preprocess relevant attributes from
#' [rstudioapi::getActiveDocumentContext()].
#'
find_active_context <- function() {
  context <- get_rstudio_context()
  path <- context$path
  start <- context$selection[[1]]$range$start
  end <- context$selection[[1]]$range$end
  if (end[2] == 1 & !all(start == end)) {
    end[1] <- end[1] - 1
    end[2] <- 1000000L # because of range constraint in substr()
  }
  list(start = start, end = end, path = path)
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}

#' Style a region of text given context
#'
#' First extracts the relevant expression from `text` and returns after styling.
#' @param text Character vector that contains the highlighted region.
#' @param context The context from [find_active_context()].
style_region <- function(text, context) {
  ind_to_style <- seq2(context$start[1], context$end[1])
  lines_to_style <- text[ind_to_style]
  last <- length(lines_to_style)
  lines_to_style[1] <- substring(lines_to_style[1], context$start[2])
  start_if_on_same_line <- ifelse(context$start[1] == context$end[1], context$start[2], 0)
  lines_to_style[last] <- substring(
    lines_to_style[last],
    1,
    context$end[2] - start_if_on_same_line
  )
  styled_lines <- style_text(lines_to_style)
  styled_lines
}

#' Complete styled expression with unstyled fraction on start / end line
#'
#' Essentially adding the parts before the highlighted area on the line where
#' the higlighted area starts to the styled expression. Also, after the styled
#' expression, the remainder of that line that was not styled is added.
#' @param context The context from [find_active_context()].
#' @param styled_expr Character vector consisting of the styled expression.
#' @param neighbourhood Neighbourhood obtained from [extract_neighbourhood()].
complete_styled_expr <- function(context,
                                 styled_expr,
                                 neighbourhood) {
  if (context$start[1] == context$end[1]) {
    styled_expr[1] <-
      paste0(neighbourhood$start, styled_expr[1], neighbourhood$end)
  } else {
    last <- length(styled_expr)
    first_line <- paste0(neighbourhood$start, styled_expr[1])
    last_line <- paste0(styled_expr[last], neighbourhood$end)
    styled_expr[1] <- first_line
    styled_expr[last] <- last_line
  }
  styled_lines <- styled_expr
  styled_lines
}

#' Extract text around the highlighted text
#'
#' Extracts unselected text on highlighted lines and text on other lines
#' @param text Character vector that contains highlighted area.
#' @param context The context from [find_active_context()].
#' @return A character vector of length three.
#'   * First element corresponds to the text on the start line that was not
#'     selected.
#'   * Second element to the text on the end line that was not selected.
#'   * Third element is the lines that are not overlapping with the
#'     highlighted text.
extract_neighbourhood <- function(text, context) {
  ind_to_style <- seq2(context$start[1], context$end[1])
  lines_to_style <- text[ind_to_style]
  lines_not_to_style <- text[setdiff(seq_along(text), ind_to_style)]
  fract_of_line_start <- substring(lines_to_style[1], 1, context$start[2] - 1)
  last <- length(lines_to_style)
  fract_of_line_end <- substring(lines_to_style[last], context$end[2])
  list(
    start = fract_of_line_start,
    end = fract_of_line_end,
    body = lines_not_to_style
  )
}
