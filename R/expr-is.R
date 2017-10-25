#' Check whether a parse table corresponds to a a certain expression
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
