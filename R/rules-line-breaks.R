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
#' @examplesIf FALSE
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
#'   x = 2, {
#'     code(to = execute)
#'   },
#'   c = {
#'     # this is the named case
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
#'
#' # brace expressions go on new line if part of a pipe, in function call...
#' c(
#'   data %>%
#'     filter(bar) %>%
#'     {
#'       cor(.$col1, .$col2, use = "complete.obs")
#'     }
#' )
#' # ... or outside
#' data %>%
#'   filter(bar) %>%
#'   {
#'     cor(.$col1, .$col2, use = "complete.obs")
#'   }
set_line_break_before_curly_opening <- function(pd) {
  line_break_to_set_idx <- which(
    (pd$token_after == "'{'") & !(pd$token %in% c("COMMENT", "EQ_FORMALS"))
  )

  line_break_to_set_idx <- setdiff(line_break_to_set_idx, nrow(pd))
  if (length(line_break_to_set_idx) > 0L) {
    is_not_curly_curly <- map_chr(
      line_break_to_set_idx + 1L,
      ~ next_terminal(vec_slice(pd, .x), vars = "token_after")$token_after
    ) != "'{'"
    last_expr_idx <- max(which(pd$token == "expr"))
    is_last_expr <- if (any(c("IF", "WHILE") == pd$token[1L])) {
      # rule not applicable for if and while
      TRUE
    } else {
      (line_break_to_set_idx + 1L) == last_expr_idx
    }
    no_line_break_before_curly_idx <- any(pd$token[line_break_to_set_idx] == "EQ_SUB")
    linebreak_before_curly <- ifelse(is_function_call(pd),
      # if in function call and has pipe, it is not recognized as function call
      # and goes to else case
      any(pd$lag_newlines[seq2(1L, line_break_to_set_idx[1L])] > 0L),
      # if not a function call, only break line if it is a pipe followed by {}
      pd$token[line_break_to_set_idx] %in% c("SPECIAL-PIPE", "PIPE")
    )
    # no line break before last brace expression and named brace expression to
    should_be_on_same_line <- is_not_curly_curly &
      (
        (is_last_expr & !linebreak_before_curly) |
          no_line_break_before_curly_idx
      )
    is_not_curly_curly_idx <- line_break_to_set_idx[should_be_on_same_line]
    pd$lag_newlines[1L + is_not_curly_curly_idx] <- 0L


    # other cases: line breaks
    should_not_be_on_same_line <- is_not_curly_curly &
      (
        (!is_last_expr | linebreak_before_curly) &
          !no_line_break_before_curly_idx
      )
    should_not_be_on_same_line_idx <- line_break_to_set_idx[
      should_not_be_on_same_line
    ]
    if (is_function_declaration(pd)) {
      should_not_be_on_same_line_idx <- setdiff(
        1L + should_not_be_on_same_line_idx, nrow(pd)
      )
    } else {
      should_not_be_on_same_line_idx <- 1L + should_not_be_on_same_line_idx
    }
    pd$lag_newlines[should_not_be_on_same_line_idx] <- 1L

    # non-curly expressions after curly expressions must have line breaks
    if (length(should_not_be_on_same_line_idx) > 0L) {
      comma_exprs_idx <- which(pd$token == "','")
      comma_exprs_idx <- setdiff(comma_exprs_idx, 1L + is_not_curly_curly_idx)
      non_comment_after_comma <- map_int(comma_exprs_idx,
        next_non_comment,
        pd = pd
      )
      pd$lag_newlines[non_comment_after_comma] <- 1L
    }
  }
  pd
}


set_line_break_around_comma_and_or <- function(pd, strict) {
  ops <- c("','", "AND", "OR", "AND2", "OR2")
  comma_with_line_break_that_can_be_removed_before <-
    (pd$token %in% ops) &
      (pd$lag_newlines > 0L) &
      (pd$token_before != "COMMENT") &
      !(lag(pd$token) %in% subset_token_opening)

  pd$lag_newlines[comma_with_line_break_that_can_be_removed_before] <- 0L
  pd$lag_newlines[lag(comma_with_line_break_that_can_be_removed_before)] <- 1L

  comma_with_line_break_that_can_be_moved_two_tokens_left <- which(
    (pd$token == "EQ_SUB") &
      (pd$lag_newlines > 0L) &
      (pd$token_before != "COMMENT") &
      !(lag(pd$token) %in% subset_token_opening)
  )

  pd$lag_newlines[comma_with_line_break_that_can_be_moved_two_tokens_left] <- 0L
  token_before <- map_int(
    comma_with_line_break_that_can_be_moved_two_tokens_left,
    previous_non_comment,
    pd = pd
  )
  pd$lag_newlines[token_before] <- 1L
  pd
}

style_line_break_around_curly <- function(strict, pd) {
  if (is_curly_expr(pd) && nrow(pd) > 2L) {
    closing_before <- pd$token == "'}'"
    opening_before <- (pd$token == "'{'")
    to_break <- lag(opening_before, default = FALSE) | closing_before
    len_to_break <- sum(to_break)
    pd$lag_newlines[to_break] <- ifelse(
      pd$token[to_break] == "COMMENT",
      pmin(1L, pd$lag_newlines[to_break]),
      if (strict) 1L else pmax(1L, pd$lag_newlines[to_break])
    )
  } else {
    is_else <- pd$token == "ELSE"
    if (any(pd$token_before[is_else] == "'}'")) {
      pd$lag_newlines[is_else] <- 0L
      pd$spaces[c(is_else, FALSE)[-1L]] <- 1L
    }
    is_if_after_else <- pd$token == "ELSE" & pd$token_after == "IF"
    pd$lag_newlines[lag(is_if_after_else)] <- 0L
  }
  pd
}

#' Styling around `\{\{`
#'
#' With \{rlang\} version 0.4, a new syntactic sugar is introduced, the
#' curly-curly operator. It interprets this code in a special way:
#' `call(\{\{ x \}\})`. See this
#' [blog post](https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/)
#' on the topic. Here, the curly-curly sugar is understood as two opening
#' curly braces, followed by an expression followed by two closing curly braces,
#' e.g. `\{\{1\}\}`. `\{\{1\} + 1\}` does not contain the curly-curly syntactic
#' sugar according to the above definition. On the other hand `\{\{ x + y \}\}`
#' is recognized by styler as containing it (and is parsable code)
#' but will most likely give an error at runtime because the way the syntactic
#' sugar is defined in rlang is to use a single token within curly-curly. In
#' addition, because rlang parses `\{\{` in a special way (just as `!!`), the
#' expression `\{\{ x \}\}` will give a runtime error when used outside of a
#' context that is capable of handling it, e.g. on the top-level (that is, not
#' within function call like `rlang_fun(\{\{ x \}\})`) or within a base R
#' function such as [c()]. However, these differences are assumed to be
#' irrelevant for styling curly-curly, as much as they were for styling `!!`.
#' curly-curly affects styling of line break and spaces, namely:
#'
#' * No line break after first or second `\{`, before third and fourth `\{`.
#' * No space after first and third `\{`, one space after second and before
#'   third `\}`.
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
      pos_opening_idx <- lag(opening_before, default = FALSE) & pd$token != "COMMENT"
      pd$lag_newlines[pos_opening_idx] <- 0L
      if (any(pos_opening_idx)) {
        # if line is broken with opening `{`, also break it with closing
        pd$lag_newlines[closing_before & pd$token_after != "COMMENT"] <- 0L
      }
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
  if (is_function_declaration(pd)) {
    is_double_indention <- is_double_indent_function_declaration(pd)
    round_after <- (
      pd$token == "')'" | pd$token_before == "'('"
    ) &
      pd$token_before != "COMMENT"
    pd$lag_newlines[pd$lag_newlines > 1L] <- 1L
    pd$lag_newlines[round_after] <- 0L
    if (is_double_indention) {
      pd$lag_newlines[lag(pd$token == "'('")] <- 1L
    }
  }
  pd
}

#'
add_line_break_after_pipe <- function(pd) {
  is_pipe <- pd$token %in% c("SPECIAL-PIPE", "PIPE")
  pd$lag_newlines[lag(is_pipe) & pd$lag_newlines > 1L] <- 1L

  if (sum(is_pipe & pd$token_after != "COMMENT") > 1L &&
    !(next_terminal(pd, vars = "token_before")$token_before %in% c("'('", "EQ_SUB", "','"))) {
    pd$lag_newlines[lag(is_pipe) & pd$token != "COMMENT"] <- 1L
  }
  pd
}

set_line_break_after_assignment <- function(pd) {
  is_assignment <- lag(pd$token, default = FALSE) %in% c("LEFT_ASSIGN", "EQ_ASSIGN")
  if (any(is_assignment)) {
    pd$lag_newlines[is_assignment] <- pmin(1L, pd$lag_newlines[is_assignment])
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
#' @param force_text_before A character vector with text before "'('" that
#'   forces a line break after every argument in the call.
#' @name set_line_break_if_call_is_multi_line
#'
#' @keywords internal
NULL

#' Sets line break after opening parenthesis
#'
#' @details
#' In general, every call that is multi-line has a line break after the opening
#' parenthesis. Exceptions:
#'
#' * The token right after the parenthesis is a comment, then, the line should
#'   be broken after the comment only. Governed by `except_token_after`.
#' * The name of the function called is `ifelse()` or similar, where we can
#'   allow the condition on the same line as the function name, and we don't
#'   impose rules on the line breaks for the subsequent arguments. Governed
#'   by `except_text_before`.
#' * Some calls like `switch()` statements are always forced to become multi-
#'   line. Governed by `force_text_before`.
#'
#' @keywords internal
set_line_break_after_opening_if_call_is_multi_line <- function(pd,
                                                               except_token_after = NULL,
                                                               except_text_before = NULL,
                                                               force_text_before = NULL) {
  if (!is_function_call(pd) && !is_subset_expr(pd)) {
    return(pd)
  }
  has_force_text_before <- last(pd$child[[1L]]$text) %in% force_text_before
  if (has_force_text_before) {
    break_pos <- c(
      which(lag(pd$token %in% c("','", "COMMENT"))),
      nrow(pd) # always break before last because this is multi-line
    )
  } else {
    if (!any(pd$lag_newlines[seq2(3L, nrow(pd))] > 0L)) {
      return(pd)
    }
    break_pos <- find_line_break_position_in_multiline_call(pd)
    idx_nested <- next_non_comment(pd, 2L)
    if (pd_is_multi_line(pd$child[[idx_nested]]) && sum(pd$lag_newlines) > 0L) {
      break_pos <- c(break_pos, idx_nested)
    }
  }
  exception_pos <- c(
    which(pd$token %in% except_token_after),
    ifelse(last(pd$child[[1L]]$text) %in% except_text_before, break_pos, NA)
  )
  pd$lag_newlines[setdiff(break_pos, exception_pos)] <- 1L
  if (has_force_text_before) {
    first_arg <- which(pd$token == "expr")[2L]
    if (lag(pd$token)[first_arg] != "COMMENT") {
      pd$lag_newlines[first_arg] <- 0L
    }
  }
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
  candidate <- (which(pd$token == "EQ_SUB") - 1L)[1L]
  if (is.na(candidate)) {
    3L
  } else {
    candidate
  }
}


#' @describeIn set_line_break_if_call_is_multi_line Sets line break before
#'   closing parenthesis.
#' @keywords internal
set_line_break_before_closing_call <- function(pd, except_token_before) {
  if (!is_function_call(pd) && !is_subset_expr(pd)) {
    return(pd)
  }
  npd <- nrow(pd)
  is_multi_line <- any(pd$lag_newlines[seq2(3L, npd - 1L)] > 0L)
  if (is_multi_line == 0L) {
    exception <- which(pd$token_before %in% except_token_before)
    pd$lag_newlines[setdiff(npd, exception)] <- 0L
    return(pd)
  }
  idx_non_comment <- previous_non_comment(pd, npd)
  if (pd$token[idx_non_comment] == "']'") {
    pd$lag_newlines[idx_non_comment] <- 1L
  } else {
    pd$lag_newlines[npd] <- 1L
  }
  pd
}


#' @rdname set_line_break_if_call_is_multi_line
#' @keywords internal
remove_line_break_in_fun_call <- function(pd, strict) {
  if (is_function_call(pd)) {
    # no blank lines within function calls
    if (strict) {
      pd$lag_newlines[
        lag(pd$token == "','") & pd$lag_newlines > 1L & pd$token != "COMMENT"
      ] <- 1L
    }
    if (nrow(pd) == 3L) {
      pd$lag_newlines[3L] <- 0L
    }
  }
  pd
}


set_line_break_after_ggplot2_plus <- function(pd) {
  # if expression is unary, first token is +. Exclude this case.
  is_plus_raw <- c(FALSE, pd$token[-1L] == "'+'")
  if (any(is_plus_raw)) {
    first_plus <- which(is_plus_raw)[1L]
    next_non_comment <- next_non_comment(pd, first_plus)
    is_plus_or_comment_after_plus_before_fun_call <-
      lag(is_plus_raw, next_non_comment - first_plus - 1L, default = FALSE) &
        (pd$token_after == "SYMBOL_FUNCTION_CALL" | pd$token_after == "SYMBOL_PACKAGE")
    if (any(is_plus_or_comment_after_plus_before_fun_call, na.rm = TRUE)) {
      gg_call <- pd$child[[previous_non_comment(pd, first_plus)]]$child[[1L]]
      if (!is.null(gg_call) && isTRUE(gg_call$text[gg_call$token == "SYMBOL_FUNCTION_CALL"] == "ggplot")) {
        plus_without_comment_after <- setdiff(
          which(is_plus_raw),
          which(lead(pd$token == "COMMENT"))
        )

        pd$lag_newlines[plus_without_comment_after + 1L] <- 1L
      }
    }
  }
  pd
}
