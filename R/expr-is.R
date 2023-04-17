#' What is a parse table representing?
#'
#' Check whether a parse table corresponds to a certain expression.
#' @name pd_is
#'
#' @param pd A parse table.
#' @param tilde_pos Integer vector indicating row-indices that should be
#'   checked for tilde. See 'Details'.
#'
#' @family third-party style guide helpers
NULL

#' @describeIn pd_is Checks whether `pd` contains an expression wrapped in curly brackets.
#' @examples
#' code <- "if (TRUE) { 1 }"
#' pd <- compute_parse_data_nested(code)
#' is_curly_expr(pd)
#' child_of_child <- pd$child[[1]]$child[[5]]
#' is_curly_expr(child_of_child)
#'
#' @export
is_curly_expr <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token[1L] == "'{'"
}

#' @describeIn pd_is Checks whether `pd` contains a `for` loop.
#' @examples
#' code <- "for (i in 1:5) print(1:i)"
#' pd <- compute_parse_data_nested(code)
#' is_for_expr(pd)
#' is_for_expr(pd$child[[1]])
#'
#' @export
is_for_expr <- function(pd) {
  pd$token[1L] == "FOR"
}

#' @describeIn pd_is Checks whether `pd` contains is a conditional expression.
#' @examples
#' code <- "if (TRUE) x <- 1 else x <- 0"
#' pd <- compute_parse_data_nested(code)
#' is_conditional_expr(pd)
#' is_conditional_expr(pd$child[[1]])
#'
#' @export
is_conditional_expr <- function(pd) {
  pd$token[1L] == "IF"
}

#' @describeIn pd_is Checks whether `pd` contains a `while` loop.
#' @export
is_while_expr <- function(pd) {
  pd$token[1L] == "WHILE"
}


#' @describeIn pd_is Checks whether `pd` is a function call.
#' @examples
#' code <- "x <- list(1:3)"
#' pd <- compute_parse_data_nested(code)
#' is_function_call(pd)
#' child_of_child <- pd$child[[1]]$child[[3]]
#' is_function_call(child_of_child)
#'
#' @export
is_function_call <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  if (is.na(pd$token_before[2L])) {
    return(FALSE)
  }
  pd$token_before[2L] == "SYMBOL_FUNCTION_CALL"
}

#' @describeIn pd_is Checks whether `pd` is a function declaration.
#' @examples
#' code <- "foo <- function() NULL"
#' pd <- compute_parse_data_nested(code)
#' is_function_declaration(pd)
#' child_of_child <- pd$child[[1]]$child[[3]]
#' is_function_declaration(child_of_child)
#'
#' @export
is_function_declaration <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token[1L] == "FUNCTION"
}

#' @describeIn pd_is Checks for every token whether or not it is a comment.
#' @examples
#' code <- "x <- 1 # TODO: check value"
#' pd <- compute_parse_data_nested(code)
#' is_comment(pd)
#'
#' @export
is_comment <- function(pd) {
  if (is.null(pd)) {
    return(FALSE)
  }
  pd$token == "COMMENT"
}

#' @describeIn pd_is Checks whether `pd` contains a tilde.
#' @details
#' A tilde is on the top row in the parse table if it is an asymmetric tilde
#' expression (like `~column`), in the second row if it is a symmetric tilde
#' expression (like `a~b`).
#' @examples
#' code <- "lm(wt ~ mpg, mtcars)"
#' pd <- compute_parse_data_nested(code)
#' is_tilde_expr(pd$child[[1]]$child[[3]])
#' is_symmetric_tilde_expr(pd$child[[1]]$child[[3]])
#' is_asymmetric_tilde_expr(pd$child[[1]]$child[[3]])
#'
#' @export
is_tilde_expr <- function(pd, tilde_pos = c(1L, 2L)) {
  if (is.null(pd) || nrow(pd) == 1L) {
    return(FALSE)
  }
  any(pd$token[tilde_pos] == "'~'")
}

#' @describeIn pd_is If `pd` contains a tilde, checks whether it is asymmetrical.
#' @export
is_asymmetric_tilde_expr <- function(pd) {
  is_tilde_expr(pd, tilde_pos = 1L)
}

#' @describeIn pd_is If `pd` contains a tilde, checks whether it is symmetrical.
#' @export
is_symmetric_tilde_expr <- function(pd) {
  is_tilde_expr(pd, tilde_pos = 2L)
}

is_subset_expr <- function(pd) {
  if (is.null(pd) || nrow(pd) == 1L) {
    return(FALSE)
  }
  pd$token[2L] %in% subset_token_opening
}


#' Identify comments that are shebangs
#'
#' Shebangs should be preserved and no space should be inserted between
#' `#` and `!`. A comment is a shebang if it is the first top-level token
#' (identified with `pos_id`) and if it starts with `#!`.
#' @param pd A parse table.
#' @examples
#' style_text("#!/usr/bin/env Rscript")
#' @keywords internal
is_shebang <- function(pd) {
  is_first_comment <- pd$pos_id == 1L
  is_first_comment[is_first_comment] <- startsWith(pd$text[is_first_comment], "#!")
  is_first_comment
}

#' Identify spinning code chunk header or xaringan
#'
#' Wrongly identifies a comment without a preceding line break as a code chunk
#' header.
#' See https://yihui.name/knitr/demo/stitch/#spin-comment-out-texts for details.
#' @examples
#' style_text(c(
#'   "# title",
#'   "some_code <- function() {}",
#'   "#+ chunk-label, opt1=value1",
#'   "call(3, 2, c(3:2))",
#'   "#> 99"
#' ))
#' @param pd A parse table.
#' @keywords internal
is_code_chunk_header_or_xaringan_or_code_output <- function(pd) {
  grepl("^#[\\+|\\-|<<|>]", pd$text, perl = TRUE)
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
#' @keywords internal
contains_else_expr_that_needs_braces <- function(pd) {
  else_idx <- which(pd$token == "ELSE")
  if (length(else_idx) > 0L) {
    non_comment_after_else <- next_non_comment(pd, else_idx)
    sub_expr <- pd$child[[non_comment_after_else]]
    # needs braces if NOT if_condition, NOT curly expr
    !is_conditional_expr(sub_expr) && !is_curly_expr(sub_expr)
  } else {
    FALSE
  }
}
