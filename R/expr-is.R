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
    !is_cond_expr(sub_expr) &&  !is_curly_expr(sub_expr)
  } else {
    FALSE
  }
}


is_cond_expr <- function(pd) {
  pd$token[1] == "IF"
}
