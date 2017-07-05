#' Update indention information of parse data
#'
#' @param pd A nested or flat parse table that is already enhanced with
#'   line break and space information via [create_filler()].
#' @param indent_by How many spaces should be added after the token of interest.
#' @name update_indention
NULL

#' @rdname update_indention
indent_round <- function(pd, indent_by) {
  indention_needed <- needs_indention(pd, token = "'('")
  if (indention_needed) {
    opening <- which(pd$token == "'('")
    start <- opening + 1
    stop <- nrow(pd) - 1
    pd <- pd %>%
      mutate(indent = indent + ifelse(seq_len(nrow(pd)) %in% start:stop,
                                      indent_by, 0))
  }
  pd %>%
    set_unindention_child(token = "')'", unindent_by = indent_by) %>%
    select_(~indent, ~newlines, ~everything())

}

#' @rdname update_indention
indent_curly <- function(pd, indent_by) {
  indention_needed <- needs_indention(pd, token = "'{'")
  if (indention_needed) {
    opening <- which(pd$token == "'{'")
    start <- opening + 1
    stop <- nrow(pd) - 1
    pd <- pd %>%
      mutate(indent = indent + ifelse(seq_len(nrow(pd)) %in% start:stop,
                                      indent_by, 0))
  }
  pd <- pd %>%
    set_unindention_child(token = "'}'", unindent_by = indent_by) %>%
    select_(~indent, ~newlines, ~everything())
  pd
}


#' Check whether indention is needed
#'
#' @param pd A parse table.
#' @param token Which token the check should be based on.
#' @return returns `TRUE` if indention is needed, `FALSE` otherwise. Indention
#'   is needed:
#'     * if `token` occurs in `pd`.
#'     * if there is no child that starts on the same line as `token` and
#'       "opens" indention without closing it on this line.
#' @return `TRUE` if indention is needed, `FALSE` otherwise.
needs_indention <- function(pd, token = "'('") {
  opening <- which(pd$token %in% token)
  length(opening) > 0 && !child_indents(pd, opening, c("'('", "'{'"))
}

#' Check whether a child will indent
#'
#' @param pd A parse table.
#' @param opening The row number of the opening token in the parse table.
#' @return Returns `TRUE` if `pd` has at least one child that indents starting
#'   on the same line as the opening token, `FALSE` otherwise.
#' @param token On which token the indention check should be based on.
#' @importFrom purrr map_lgl
child_indents <- function(pd, opening, token) {
  if (is.null(pd$child)) return(FALSE)
  opening_line <- pd$line1[opening]
  pd <- pd %>%
    filter(!terminal, line1 == opening_line, line2 != opening_line)
  if (nrow(pd) == 0) return(FALSE)
  children_indent <- map_lgl(pd$child, pd_has_token, token)
  any(children_indent)
}

#' Check whether a parse table contains a token
#'
#' @param pd A parse table.
#' @param token The token for which it should be checked whether it is in the
#'   parse table.
#' @return `TRUE` if the token is in the parse table, `FALSE` otherwise.
pd_has_token <- function(pd, token) {
  has_indention_token <- token %in% pd$token
  any(has_indention_token)
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
