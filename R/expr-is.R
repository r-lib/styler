#' Check whether a parse table corresponds to a certain expression
#'
#' @param pd A parse table.
#' @name pd_is
NULL

#' @describeIn pd_is Checks whether `pd` contains an expression wrapped in
#'   curly brackets.
is_curly_expr <- function(pd) {
  if (is.null(pd)) return(FALSE)
  pd$token[1] == "'{'"
}

is_subset_expr <- function(pd) {
  if (is.null(pd) || nrow(pd) == 1) return(FALSE)
  pd$token[2] == "'['"
}

#' @describeIn pd_is Checks whether `pd` is a function call.
is_function_call <- function(pd) {
  if (is.null(pd)) return(FALSE)
  if (is.na(pd$token_before[2])) return(FALSE)
  pd$token_before[2] == "SYMBOL_FUNCTION_CALL"
}

#' @describeIn pd_is Checks whether `pd` is a function declaration.
is_function_dec <- function(pd) {
  if (is.null(pd)) return(FALSE)
  pd$token[1] == "FUNCTION"
}

is_comment <- function(pd) {
  if (is.null(pd)) return(FALSE)
  pd$token == "COMMENT"
}

#' Identify comments that are shebangs
#'
#' Shebangs should be preserved and no space should be inserted between
#' \# and !. A comment is a shebang if it is the first top level token
#' (identified with `pos_id`) and if it starts with `#!`.
#' @param pd A parse table.
#' @examples
#' style_text("#!/usr/bin/env Rscript")
is_shebang <- function(pd) {
  is_first_comment <- is_comment(pd) & (pd$pos_id == 1L)
  is_first_comment[is_first_comment] <- grepl(
    "^#!", pd$text[is_first_comment], perl = TRUE
  )
  is_first_comment
}

#' Identify spinning code chunk header
#'
#' See https://yihui.name/knitr/demo/stitch/#spin-comment-out-texts for details.
#' @examples
#' style_text(c(
#'   "# title",
#'   "some_code <- function() {}",
#'   "#+ chunk-label, opt1=value1",
#'   "call(3, 2, c(3:2))"
#'   ))
#' @param pd A parse table.
is_code_chunk_header <- function(pd) {
  is_comment <- is_comment(pd)
  is_comment[is_comment] <- grepl(
    "^#[\\+|\\-]", pd$text[is_comment], perl = TRUE
  )
  is_comment
}

contains_else_expr <- function(pd) {
  any(pd$token == "ELSE")
}

#' Check whether an else expression needs braces
#'
#' Checks whether an else expression in a nest needs braces. Note that for
#' if-else-if expressions, there is no need to add braces since the if in
#' else-if will be visited separately with the visitor. This applies to all
#' conditional statements with more than one alternative.
#' @param pd A parse table
contains_else_expr_that_needs_braces <- function(pd) {
  else_idx <- which(pd$token == "ELSE")
  if (length(else_idx) > 0) {
    non_comment_after_else <- next_non_comment(pd, else_idx)
    sub_expr <- pd$child[[non_comment_after_else]]
    # needs braces if NOT if_condition, NOT curly expr
    !is_cond_expr(sub_expr) && !is_curly_expr(sub_expr)
  } else {
    FALSE
  }
}

is_cond_expr <- function(pd) {
  pd$token[1] == "IF"
}
