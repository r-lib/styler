#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [create_filler] or
#'   [create_filler_nested].
#' @param indent_by How many spaces should be added after the token of interest.
#' @name update_indention
NULL

#' @rdname update_indention
indent_round <- function(pd, indent_by) {
  start <- which(pd$token == "'('") + 1
  stop <- which(pd$token == "')'") - 1
  if (length(start) == 0 && length(stop) == 0) {
    pd$indent <- 0
  } else {
    pd$indent <- ifelse(1:nrow(pd) %in% start[1]:stop[1], indent_by, 0) *
      lag(pd$newlines, default = 0)
  }
  # general, should maybe not go here.
  pd$spaces <- pd$spaces * (pd$newlines == 0)

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
  pd <- indent_round(pd, indent_by = 2)
  pd$child <- map(pd$child, indent_round_nested)
  pd
}
