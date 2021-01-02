#' Set line break before a curly brace
#'
#' Rule:
#' * Principle: Function arguments that consist of a braced expression always
#'   need to start on a new line
#' * Exception: [...] unless it's the last argument and all other
#'   arguments fit on the line of the function call
#' * Exception: [...] or they are named.
#' * Extension: Also, expressions following on braced expressions also cause a
#'   line trigger.
#' @keywords internal
#' @examples
#' \dontrun{
#' tryCatch(
#'   {
#'     f(8)
#'   },
#'   error = function(e) NULL
#' )
#' # last-argument case
#' testthat("braces braces are cool", {
#'   code(to = execute)
#' })
#' call2(
#'   x = 2,
#'   {
#'     code(to = execute)
#'   },
#'   c = { # this is the named case
#'     g(x = 7)
#'   }
#' )
#' tryGugus(
#'   {
#'     g5(k = na)
#'   },
#'   a + b # line break also here because
#'   # preceded by brace expression
#' )
#' }
set_line_break_before_curly_opening <- function(pd) {
  line_break_to_set_idx <- which(
    (pd$token_after == "'{'") & (pd$token != "COMMENT")
  )

  line_break_to_set_idx <- setdiff(line_break_to_set_idx, nrow(pd))
  if (length(line_break_to_set_idx) > 0) {
    is_not_curly_curly <- map_chr(
      line_break_to_set_idx + 1L,
      ~ next_terminal(pd[.x, ], vars = "token_after")$token_after
    ) != "'{'"
    last_expr_idx <- max(which(pd$token == "expr"))
    is_last_expr <- ifelse(pd$token[1] == "IF",
      # rule not applicable for IF
      TRUE, (line_break_to_set_idx + 1L) == last_expr_idx
    )
    eq_sub_before <- pd$token[line_break_to_set_idx] == "EQ_SUB"
    linebreak_before_curly <- ifelse(is_function_call(pd),
      any(pd$lag_newlines[seq2(1, line_break_to_set_idx[1])] > 0),
      FALSE
    )
    # no line break before last brace expression and named brace expression to
    should_be_on_same_line <- is_not_curly_curly &
      ((is_last_expr & !linebreak_before_curly) | eq_sub_before)
    is_not_curly_curly_idx <- line_break_to_set_idx[should_be_on_same_line]
    pd$lag_newlines[1 + is_not_curly_curly_idx] <- 0L


    # other cases: line breaks
    should_not_be_on_same_line <- is_not_curly_curly &
      ((!is_last_expr | linebreak_before_curly) & !eq_sub_before)
    should_not_be_on_same_line_idx <- line_break_to_set_idx[should_not_be_on_same_line]

    pd$lag_newlines[1 + should_not_be_on_same_line_idx] <- 1L

    # non-curly expressions after curly expressions must have line breaks
    if (length(should_not_be_on_same_line_idx) > 0) {
      comma_exprs_idx <- which(pd$token == "','")
      comma_exprs_idx <- setdiff(comma_exprs_idx, 1 + is_not_curly_curly_idx)
      non_comment_after_comma <- map_int(comma_exprs_idx,
        next_non_comment,
        pd = pd
      )
      non_comment_after_expr <-
        non_comment_after_comma[non_comment_after_comma > should_not_be_on_same_line_idx[1]]
      pd$lag_newlines[non_comment_after_comma] <- 1L
    }
  }
  pd
}


set_line_break_around_comma <- function(pd, strict) {
  comma_with_line_break_that_can_be_removed_before <-
    (pd$token == "','") &
      (pd$lag_newlines > 0) &
      (pd$token_before != "COMMENT") &
      (lag(pd$token) != "'['")

  pd$lag_newlines[comma_with_line_break_that_can_be_removed_before] <- 0L
  pd$lag_newlines[lag(comma_with_line_break_that_can_be_removed_before)] <- 1L
  pd
}

style_line_break_around_curly <- function(strict, pd) {
  if (is_curly_expr(pd) && nrow(pd) > 2) {
    closing_before <- pd$token == "'}'"
    opening_before <- (pd$token == "'{'") & (pd$token_after != "COMMENT")
    to_break <- lag(opening_before, default = FALSE) | closing_before
    len_to_break <- sum(to_break)
    pd$lag_newlines[to_break] <- ifelse(rep(strict, len_to_break),
      1L,
      pmax(1L, pd$lag_newlines[to_break])
    )
  }
  pd
}

#' Styling around `\{\{`
#'
#' With \{rlang\} version 0.4, a new syntactic sugar is introduced, the
#' curly-curly operator. It interprets this code in a special way:
#' `call(\{\{ x \}\})`. See this
#' [blog post](https://www.tidyverse.org/articles/2019/06/rlang-0-4-0/)
#' on the topic. Here, the curly-curly sugar is understood as two opening
#' curly braces, followed by an expression followed by two closing curly braces,
#' e.g. `\{\{1\}\}`. `\{\{1\} + 1\}` does not contain the curly-curly syntactic
#' sugar according to the above definition. On the other hand `\{\{ x + y \}\}`
#' is recognized by styler as containing it (and is parsable code)
#' but will most likely give an error at runtime because the way the syntactic
#' sugar is defined in rlang is to use a single token within curly-curly. In
#' addition, because rlang parses `\{\{` in a special way (just as `!!`), the
#' expression `\{\{ x \}\}` will give a runtime error when used outside of a
#' context that is capable of handling it, e.g. on the top level (that is, not
#' within function call like `rlang_fun(\{\{ x \}\})`) or within a base R
#' function such as [c()]. However, these differences are assumed to be
#' irrelevant for styling curly-curly, as much as they were for styling `!!`.
#' curly-curly affects styling of line break and spaces, namely:
#'
#' * No line break after first or second `\{`, before third and fourth `\{`.
#' * No space after first and third `\{`, one space after second and before third
#'   `\}`.
#' * No line breaks within curly-curly, e.g. `\{\{ x \}\}` can only contain line
#'   breaks after the last brace or before the first brace. But these are not
#'   dependent on curly-curly specifically.
#' @param pd A parse table.
#' @keywords internal
#' @seealso style_text_without_curly_curly
set_line_break_around_curly_curly <- function(pd) {
  if (is_curly_expr(pd)) {
    # none after {
    opening_before <- (pd$token == "'{'") &
      (pd$token_before == "'{'" | pd$token_after == "'{'")

    # none before }
    closing_before <- (pd$token == "'}'") &
      (pd$token_after == "'}'" | pd$token_before == "'}'")
    if (any(opening_before) && any(closing_before)) {
      pd$lag_newlines[lag(opening_before, default = FALSE)] <- 0L
      pd$lag_newlines[closing_before] <- 0L
    }
  }
  pd
}

# if ) follows on }, don't break line
remove_line_break_before_round_closing_after_curly <- function(pd) {
  round_after_curly <- pd$token == "')'" & (pd$token_before == "'}'")
  pd$lag_newlines[round_after_curly] <- 0L
  pd
}

remove_line_breaks_in_fun_dec <- function(pd) {
  if (is_function_dec(pd)) {
    round_after <- pd$token == "')'" & pd$token_before != "COMMENT"
    pd$lag_newlines[pd$lag_newlines > 1L] <- 1L
    pd$lag_newlines[round_after] <- 0L
  }
  pd
}


#' @importFrom rlang seq2
add_line_break_after_pipe <- function(pd) {
  is_pipe <- pd$token == c("SPECIAL-PIPE")
  pd$lag_newlines[lag(is_pipe) & pd$lag_newlines > 1] <- 1L

  if (sum(is_pipe & pd$token_after != "COMMENT") > 1 &&
    !(next_terminal(pd, vars = "token_before")$token_before %in% c("'('", "EQ_SUB", "','"))) {
    pd$lag_newlines[lag(is_pipe)] <- 1L
  }
  pd
}


#' Set line break for multi-line function calls
#' @param pd A parse table.
#' @param except_token_after A character vector with tokens after "'('" that do
#'   not cause a line break after "'('".
#' @param except_text_before A character vector with text before "'('" that do
#'   not cause a line break after "'('".
#' @param except_token_before A character vector with text before "')'" that do
#'   not cause a line break before "')'".
#' @name set_line_break_if_call_is_multi_line
#' @importFrom rlang seq2
#' @keywords internal
NULL

#' @describeIn set_line_break_if_call_is_multi_line Sets line break after
#'   opening parenthesis.
#' @keywords internal
set_line_break_after_opening_if_call_is_multi_line <-
  function(pd,
           except_token_after = NULL,
           except_text_before = NULL) {
    if (!is_function_call(pd) && !is_subset_expr(pd)) {
      return(pd)
    }
    npd <- nrow(pd)
    seq_x <- seq2(3L, npd - 1L)
    is_multi_line <- any(
      (pd$lag_newlines[seq_x] > 0) |
        (pd$token[seq_x] == "COMMENT")
    )
    if (!is_multi_line) {
      return(pd)
    }
    break_pos <- find_line_break_position_in_multiline_call(pd)

    exception_pos <- c(
      which(pd$token %in% except_token_after),
      ifelse(pd$child[[1]]$text[1] %in% except_text_before, break_pos, NA)
    )
    pd$lag_newlines[setdiff(break_pos, exception_pos)] <- 1L
    pd
  }


#' Find index of the token before which the line should be broken
#'
#' Given a multi-line function call parse table, this function finds the
#' position of the first named argument and breaks returns the index of it.
#' If there is no named argument, the line is broken right after the opening
#' parenthesis.
#' @inheritParams set_line_break_if_call_is_multi_line
#' @keywords internal
find_line_break_position_in_multiline_call <- function(pd) {
  candidate <- (which(pd$token == "EQ_SUB") - 1L)[1]
  ifelse(is.na(candidate), 3L, candidate)
}


#' @describeIn set_line_break_if_call_is_multi_line Sets line break before
#'   closing parenthesis.
#' @keywords internal
set_line_break_before_closing_call <- function(pd, except_token_before) {
  if (!is_function_call(pd) && !is_subset_expr(pd)) {
    return(pd)
  }
  npd <- nrow(pd)
  is_multi_line <- any(pd$lag_newlines[seq2(3L, npd - 1L)] > 0)
  if (!is_multi_line) {
    exception <- which(pd$token_before %in% except_token_before)
    pd$lag_newlines[setdiff(npd, exception)] <- 0L
    return(pd)
  }
  pd$lag_newlines[npd] <- 1L
  pd
}


#' @rdname set_line_break_if_call_is_multi_line
#' @keywords internal
remove_line_break_in_fun_call <- function(pd, strict) {
  if (is_function_call(pd)) {
    # no blank lines within function calls
    if (strict) {
      pd$lag_newlines[lag(pd$token == "','") & pd$lag_newlines > 1 & pd$token != "COMMENT"] <- 1L
    }
    if (nrow(pd) == 3) {
      pd$lag_newlines[3] <- 0L
    }
  }
  pd
}


set_linebreak_after_ggplot2_plus <- function(pd) {
  # if expression is unary, first token is +. Exclude this case.
  is_plus_raw <- c(FALSE, pd$token[-1] == "'+'")
  if (any(is_plus_raw)) {
    first_plus <- which(is_plus_raw)[1]
    next_non_comment <- next_non_comment(pd, first_plus)
    is_plus_or_comment_after_plus_before_fun_call <-
      lag(is_plus_raw, next_non_comment - first_plus - 1, default = FALSE) &
        (pd$token_after == "SYMBOL_FUNCTION_CALL" | pd$token_after == "SYMBOL_PACKAGE")
    if (any(is_plus_or_comment_after_plus_before_fun_call)) {
      gg_call <- pd$child[[previous_non_comment(pd, first_plus)]]$child[[1]]
      if (!is.null(gg_call) && isTRUE(gg_call$text[gg_call$token == "SYMBOL_FUNCTION_CALL"] == "ggplot")) {
        plus_without_comment_after <- setdiff(
          which(is_plus_raw),
          which(lead(pd$token == "COMMENT"))
        )

        pd$lag_newlines[plus_without_comment_after + 1] <- 1L
      }
    }
  }
  pd
}
