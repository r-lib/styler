#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [create_filler()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @name update_indention
NULL

#' @rdname update_indention
indent_round <- function(pd, indent_by) {
  opening <- which(pd$token == "'('")
  if (length(opening) > 0) {
    start <- opening + 1
    stop <- nrow(pd) - 1
  } else {
    start <- stop <- 0
  }
  pd <- pd %>%
    mutate(indent = indent + ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)) %>%
    select_(~indent, ~newlines, ~everything())
  pd
}


#' @rdname update_indention
indent_curly <- function(pd, indent_by) {
  opening <- which(pd$token == "'{'")
  if (length(opening) > 0) {
    start <- opening + 1
    stop <- nrow(pd) - 1
  } else {
    start <- stop <- 0
  }
  pd <- pd %>%
    mutate(indent = indent + ifelse(seq_len(nrow(pd)) %in% start:stop, indent_by, 0)) %>%
    select_(~indent, ~newlines, ~everything())
  pd
}

#' Strip EOL spaces
#'
#' Remove end-of-line spaces.
#' @param pd_flat A flat parse table.
#' @return A nested parse table.
strip_eol_spaces <- function(pd_flat) {
  pd_flat %>%
    mutate(spaces = spaces * (newlines == 0))
}
