#' @include token-define.R
#' @keywords internal
add_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  idx_after <- op_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_after] <- pmax(pd_flat$spaces[idx_after], 1L)
  pd_flat
}

#' @include token-define.R
#' @keywords internal
set_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  if (!any(op_after)) {
    return(pd_flat)
  }
  op_before <- lead(op_after, default = FALSE)
  pd_flat$spaces[op_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat$spaces[op_after & (pd_flat$newlines == 0L)] <- 1L
  pd_flat
}

#' Style spacing around math tokens
#' @inheritParams style_space_around_token
#' @param one Character vector with tokens that should be surrounded by at
#'   least one space (depending on `strict = TRUE` in the styling functions
#'   [style_text()] and friends). See 'Examples'.
#' @param zero Character vector of tokens that should be surrounded with zero
#'   spaces.
#' @keywords internal
style_space_around_math_token <- function(strict, zero, one, pd_flat) {
  # We remove spaces for zero (e.g., around ^ in the tidyverse style guide)
  # even for strict = FALSE to be consistent with the : operator
  pd_flat %>%
    style_space_around_token(
      strict = TRUE, tokens = zero, level_before = 0L, level_after = 0L
    ) %>%
    style_space_around_token(
      strict = strict, tokens = one, level_before = 1L, level_after = 1L
    )
}

#' Set spacing of token to a certain level
#'
#' Set the spacing of all `tokens` in `pd_flat` to `level` if `strict = TRUE` or
#' to at least to `level` if `strict = FALSE`.
#' @param pd_flat A nest or a flat parse table.
#' @param strict Whether the rules should be applied strictly or not.
#' @param tokens Character vector with tokens that should be styled.
#' @param level_before,level_after Scalar indicating the amount of spaces that
#'   should be inserted around the `tokens` on the left and right position
#'   respectively.
#' @keywords internal
style_space_around_token <- function(pd_flat,
                                     strict,
                                     tokens,
                                     level_before,
                                     level_after = level_before) {
  op_after <- pd_flat$token %in% tokens
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L)
  idx_after <- op_after & (pd_flat$newlines == 0L)
  if (strict) {
    pd_flat$spaces[idx_before] <- level_before
    pd_flat$spaces[idx_after] <- level_after
  } else {
    pd_flat$spaces[idx_before] <-
      pmax(pd_flat$spaces[idx_before], level_before)
    pd_flat$spaces[idx_after] <-
      pmax(pd_flat$spaces[idx_after], level_after)
  }
  pd_flat
}

style_space_around_tilde <- function(pd_flat, strict) {
  if (is_symmetric_tilde_expr(pd_flat)) {
    pd_flat <- style_space_around_token(pd_flat,
      strict, "'~'",
      level_before = 1, level_after = 1
    )
  } else if (is_asymmetric_tilde_expr(pd_flat)) {
    pd_flat <- style_space_around_token(pd_flat,
      strict = TRUE, "'~'", level_before = 1,
      level_after = ifelse(nrow(pd_flat$child[[2]]) > 1, 1, 0)
    )
  }
  pd_flat
}

remove_space_after_unary_pm_nested <- function(pd) {
  if (any(pd$token[1] %in% c("'+'", "'-'"))) {
    pd$spaces[1] <- 0L
  }

  pd
}

#' Replace single quotes with double quotes
#'
#' We do not use `deparse()` as in previous implementations but `paste0()` since
#' the former approach escapes the reverse backslash in the line break character
#' `\\n` whereas the solution with `paste0()` does not.
#' @examples
#' style_text("'here
#' is a string
#' '")
#' @importFrom purrr map map_chr
#' @param pd_flat A flat parse table.
#' @keywords internal
fix_quotes <- function(pd_flat) {
  str_const <- pd_flat$token == "STR_CONST"
  str_const_change <- grepl("^'([^\"]*)'$", pd_flat$text[str_const])
  pd_flat$text[str_const][str_const_change] <-
    map(pd_flat$text[str_const][str_const_change], parse_text) %>%
    map_chr(~ paste0("\"", .x, "\""))
  pd_flat
}

remove_space_before_opening_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "'('"
  if (!any(paren_after)) {
    return(pd_flat)
  }
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_after_opening_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "'('"
  if (!any(paren_after)) {
    return(pd_flat)
  }
  pd_flat$spaces[paren_after & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_before_closing_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "')'"
  if (!any(paren_after)) {
    return(pd_flat)
  }
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

add_space_after_for_if_while <- function(pd_flat) {
  comma_after <- pd_flat$token %in% c("FOR", "IF", "WHILE")
  if (!any(comma_after)) {
    return(pd_flat)
  }
  idx <- comma_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- pmax(pd_flat$spaces[idx], 1L)
  pd_flat
}

add_space_before_brace <- function(pd_flat) {
  op_after <- pd_flat$token %in% "'{'"
  if (!any(op_after)) {
    return(pd_flat)
  }
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L) & pd_flat$token != "'('"
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  pd_flat
}

add_space_after_comma <- function(pd_flat) {
  comma_after <- (pd_flat$token == "','") & (pd_flat$newlines == 0L)
  pd_flat$spaces[comma_after] <- pmax(pd_flat$spaces[comma_after], 1L)
  pd_flat
}

set_space_after_comma <- function(pd_flat) {
  comma_after <- (pd_flat$token == "','") & (pd_flat$newlines == 0L)
  pd_flat$spaces[comma_after] <- 1L
  pd_flat
}

remove_space_before_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  if (!any(comma_after)) {
    return(pd_flat)
  }
  comma_before <- lead(comma_after, default = FALSE)
  idx <- comma_before & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- 0L
  pd_flat
}


#' Set space between levels of nesting
#'
#' With the nested approach, certain rules do not have an effect anymore because
#' of the nature of the nested structure. Setting spacing before curly
#' brackets in for / if / while statements and function declarations will be
#' such a case since a curly bracket is always at the first position in a parse
#' table, so spacing cannot be set after the previous token.
#' @param pd_flat A flat parse table.
#' @keywords internal
set_space_between_levels <- function(pd_flat) {
  if (pd_flat$token[1] %in% c("FUNCTION", "IF", "WHILE")) {
    index <- pd_flat$token == "')'" & pd_flat$newlines == 0L
    pd_flat$spaces[index] <- 1L
  } else if (pd_flat$token[1] == "FOR") {
    index <- pd_flat$token == "forcond" & pd_flat$newlines == 0
    pd_flat$spaces[index] <- 1L
  }
  pd_flat
}

#' Start comments with a space
#'
#' Forces comments to start with a space, that is, after the regular expression
#' "^#+'*", at least one space must follow if the comment is *non-empty*, i.e
#' there is not just spaces within the comment. Multiple spaces may be legit for
#' indention in some situations.
#' @param pd A parse table.
#' @param force_one Whether or not to force one space or allow multiple spaces
#'   after the regex "^#+'*".
#' @importFrom purrr map_chr
#' @keywords internal
start_comments_with_space <- function(pd, force_one = FALSE) {
  comment_pos <- is_comment(pd) & !is_shebang(pd) & !is_code_chunk_header(pd)
  if (!any(comment_pos)) {
    return(pd)
  }

  comments <- rematch2::re_match(
    pd$text[comment_pos],
    "^(?<prefix>#+['\\*]*)(?<space_after_prefix> *)(?<text>.*)$"
  )
  comments$space_after_prefix <- nchar(
    comments$space_after_prefix,
    type = "width"
  )
  comments$space_after_prefix <- set_spaces(
    spaces_after_prefix = comments$space_after_prefix,
    force_one
  )

  pd$text[comment_pos] <-
    paste0(
      comments$prefix,
      map_chr(comments$space_after_prefix, rep_char, char = " "),
      comments$text
    ) %>%
    trimws("right")
  pd$short[comment_pos] <- substr(pd$text[comment_pos], 1, 5)
  pd
}


set_space_before_comments <- function(pd_flat) {
  comment_after <- (pd_flat$token == "COMMENT") & (pd_flat$lag_newlines == 0L)
  if (!any(comment_after)) {
    return(pd_flat)
  }
  comment_before <- lead(comment_after, default = FALSE)
  pd_flat$spaces[comment_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat
}

add_space_before_comments <- function(pd_flat) {
  comment_after <- (pd_flat$token == "COMMENT") & (pd_flat$lag_newlines == 0L)
  if (!any(comment_after)) {
    return(pd_flat)
  }
  comment_before <- lead(comment_after, default = FALSE)
  pd_flat$spaces[comment_before & (pd_flat$newlines == 0L)] <-
    pmax(pd_flat$spaces[comment_before], 1L)
  pd_flat
}


remove_space_after_excl <- function(pd_flat) {
  excl <- (pd_flat$token == "'!'") &
    (pd_flat$token_after != "'!'") &
    (pd_flat$newlines == 0L)
  pd_flat$spaces[excl] <- 0L
  pd_flat
}

set_space_after_bang_bang <- function(pd_flat) {
  last_bang <- (pd_flat$token == "'!'") &
    (pd_flat$token_after != "'!'") &
    (pd_flat$newlines == 0L) &
    (pd_flat$token_before == "'!'")

  pd_flat$spaces[last_bang] <- 0L
  pd_flat
}

remove_space_before_dollar <- function(pd_flat) {
  dollar_after <- (pd_flat$token == "'$'") & (pd_flat$lag_newlines == 0L)
  dollar_before <- lead(dollar_after, default = FALSE)
  pd_flat$spaces[dollar_before] <- 0L
  pd_flat
}

remove_space_after_fun_dec <- function(pd_flat) {
  fun_after <- (pd_flat$token == "FUNCTION") & (pd_flat$lag_newlines == 0L)
  pd_flat$spaces[fun_after] <- 0L
  pd_flat
}

remove_space_around_colons <- function(pd_flat) {
  one_two_or_three_col_after <-
    pd_flat$token %in% c("':'", "NS_GET_INT", "NS_GET")

  one_two_or_three_col_before <-
    lead(one_two_or_three_col_after, default = FALSE)

  col_around <-
    one_two_or_three_col_before | one_two_or_three_col_after

  pd_flat$spaces[col_around & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

#' Set space between EQ_SUB and "','"
#' @param pd A parse table.
#' @keywords internal
set_space_between_eq_sub_and_comma <- function(pd) {
  op_before <- which(pd$token == "EQ_SUB" & lead(pd$token == "','"))
  pd$spaces[op_before] <- 1L
  pd
}
