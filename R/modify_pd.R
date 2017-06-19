#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [create_filler()] or
#'   [create_filler_nested()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @name update_indention
NULL

#' @rdname update_indention
indent_round <- function(pd, indent_by = 2) {
  opening <- which(pd$token == "'('")
  if (length(opening) > 0) {
    start <- opening + 1
    stop <- nrow(pd) - 1
  } else {
    start <- stop <- 0
  }
  pd <- pd %>%
    mutate(indent = indent + ifelse(seq_len(nrow(pd)) %in% start:stop,
                                    indent_by,
                                    0)) %>%
    select_(~indent, ~newlines, ~everything())
  pd
}


#' @rdname update_indention
indent_op <- function(pd, indent_by = 2, op = op_token) {
  add <- lag(pd$token %in% op_token, n = 1, default = 0)
  if (any(add > 0)) {
    pd <- pd %>%
      mutate(spaces_before = spaces_before + indent_by * add)
  }
  select_(pd, ~indent, ~newlines, ~everything())
}



#' Update indention information of nested parse data
#'
#' These functions apply the update functions of the same name but without
#'   suffix nested to each level of nesting of the nested parse table.
#' @param pd A nested parse table that is already enhanced with
#'   line break and space information via [create_filler_nested].
#' @name update_indention_nested
NULL

#' @rdname update_indention_nested
indent_round_nested <- function(pd) {
  if (is.null(pd)) return(pd)
  pd <- pd %>%
    indent_round(indent_by = 2) %>%
    mutate(child = map(child, indent_round_nested))
  pd
}


#' Strip EOL spaces
#'
#' Remove end-of-line spaces.
#' @param pd_nested A nested parse table.
#' @return A nested parse table.
strip_eol_spaces <- function(pd_flat) {
  pd_flat %>%
    mutate(spaces = spaces * (newlines == 0))
}


#' Strip EOL spaces
#'
#' Remove end-of-line spaces.
#' @param pd_nested A nested parse table.
#' @return A nested parse table.
strip_eol_spaces_nested <- function(pd_nested) {
  if (is.null(pd_nested)) return()
  pd_nested <- pd_nested %>%
    mutate(spaces = spaces * (newlines == 0),
           child  = map(child, strip_eol_spaces_nested))
  pd_nested
}
