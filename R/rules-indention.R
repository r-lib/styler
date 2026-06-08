#' @describeIn update_indention Inserts indention based on round, square and
#'   curly brackets.
#' @keywords internal
indent_braces <- function(pd, indent_by) {
  indent_indices <- compute_indent_indices(
    pd,
    token_opening = c("'('", "'['", "'{'", "LBB"),
    token_closing = c("')'", "']'", "'}'")
  )
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  set_unindention_child(pd, token = "')'", unindent_by = indent_by)
}

#' Revert the indention of function declaration header
#'
#' Necessary for consistent indention of the function declaration header.
#' @param pd A parse table.
#' @inheritParams is_single_indent_function_declaration
#' @seealso set_unindention_child update_indention_reference_function_declaration
#' @keywords internal
unindent_function_declaration <- function(pd, indent_by = 2L) {
  if (is_function_declaration(pd)) {
    idx_closing_brace <- which(pd$token == "')'")
    fun_dec_head <- seq2(2L, idx_closing_brace)
    if (is_single_indent_function_declaration(pd, indent_by = indent_by)) {
      pd$indent[fun_dec_head] <- indent_by
      pd$indent[idx_closing_brace] <- 0L
    } else {
      pd$indent[fun_dec_head] <- 0L
    }
  }
  pd
}

#' Is the function declaration authored with standard single/double indentation?
#'
#' Assumes you already checked if it's a function declaration with
#' `is_function_declaration`. It returns `TRUE` if the first formal argument
#' starts on a new line AND either the closing parenthesis `)` also starts on a
#' new line or the first argument is indented by standard single/double
#' indentation (`<= 2 * indent_by` spaces). This robustly identifies standard
#' multi-line headers while correctly returning `FALSE` for aligned/hanging
#' indents (where arguments start on the first line or share the line with `)`).
#' @param pd A parse table.
#' @inheritParams tidyverse_style
#' @keywords internal
is_single_indent_function_declaration <- function(pd, indent_by = 2L) {
  idx_paren_open <- which(pd$token == "'('")[1L]
  idx_paren_close <- which(pd$token == "')'")[1L]
  if (is.na(idx_paren_open) || is.na(idx_paren_close)) return(FALSE)

  row_idx <- seq_len(nrow(pd))
  formals <- which(
    pd$token %in% c("SYMBOL_FORMALS", "SYMBOL_SUB") &
      row_idx > idx_paren_open &
      row_idx < idx_paren_close
  )
  if (length(formals) == 0L) return(FALSE)

  first_formal_idx <- formals[1L]
  if (any(pd$lag_newlines[seq2(idx_paren_open + 1L, first_formal_idx)] > 0L)) {
    return(pd$lag_newlines[idx_paren_close] > 0L || pd$spaces[first_formal_idx - 1L] <= 2L * indent_by)
  }

  formals_nl <- which(
    pd$token %in% c("SYMBOL_FORMALS", "SYMBOL_SUB") &
      pd$lag_newlines > 0L &
      row_idx > idx_paren_open &
      row_idx < idx_paren_close
  )
  if (length(formals_nl) == 0L) return(FALSE)

  first_nl_formal <- formals_nl[1L]
  pd$spaces[first_nl_formal - 1L] <= 2L * indent_by
}



#' @describeIn update_indention Indents *all* tokens after `token` - including
#'   the last token.
#' @keywords internal
indent_op <- function(pd,
                      indent_by,
                      token = c(
                        math_token,
                        logical_token,
                        special_token,
                        "PIPE",
                        "LEFT_ASSIGN",
                        "EQ_ASSIGN",
                        "'$'",
                        "'~'"
                      )) {
  indent_indices <- compute_indent_indices(pd, token)
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Updates indention for token EQ_SUB. Only differs
#'   from [indent_op()] in the sense that not all subsequent tokens in the parse
#'   table are necessarily indented, as `EQ_SUB` and `EQ_FORMALS` can occur
#'   multiple times in a parse table.
#'   occurs is not indented (see[compute_indent_indices()])
#' @keywords internal
indent_eq_sub <- function(pd,
                          indent_by,
                          token = c("EQ_SUB", "EQ_FORMALS")) {
  eq_sub <- pd$token %in% token
  if (!any(eq_sub)) {
    return(pd)
  }
  has_line_break <- pd$lag_newlines > 0L | pd$token == "COMMENT"
  indent_indices <- which(lag(eq_sub, default = FALSE) & has_line_break)
  if (any(pd$token[indent_indices] == "COMMENT")) {
    indent_indices <- purrr::map_int(indent_indices, function(idx) {
      if (pd$token[idx] == "COMMENT") {
        next_non_comment(pd, idx)
      } else {
        idx
      }
    })
  }
  pd$indent[indent_indices] <- pd$indent[indent_indices] + indent_by
  pd
}

#' @describeIn update_indention Is used to indent for / while / if / if-else
#'   statements that do not have curly parenthesis.
#' @keywords internal
indent_without_paren <- function(pd, indent_by = 2L) {
  pd %>%
    indent_without_paren_for_while_fun(indent_by) %>%
    indent_without_paren_if_else(indent_by)
}

#' Update the indention reference
#'
#' @param pd_nested A nested parse table.
#' @name update_indention_ref
#' @keywords internal
NULL

#' @describeIn update_indention_ref Updates the reference pos_id for all
#'   tokens in `pd_nested` if `pd_nested` contains a function declaration.
#'   Tokens inside a function declaration are are re-indented,
#'   that is, they are indented up to the level at which the token FUNCTION
#'   ends in terms of col2.
#' @examples
#' \dontrun{
#' a <- function(x,
#'               y) {
#'   x + y
#' }
#' }
#'
#' @keywords internal
update_indention_reference_function_declaration <- function(pd_nested) {
  if (is_function_declaration(pd_nested) && !is_single_indent_function_declaration(pd_nested)) {
    seq <- seq2(3L, nrow(pd_nested) - 2L)
    pd_nested$indention_ref_pos_id[seq] <- pd_nested$pos_id[2L]
  }
  pd_nested
}
