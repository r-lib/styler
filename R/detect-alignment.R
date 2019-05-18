#' Check if tokens are aligned
#'
#' A line is called aligned if the following conditions hold:
#'
#' * argument name and = have the same line1 (i.e. the start of a token) for
#'   multiple lines for the first column.
#' * spacing around comma is correct (none before, at least one after).
#' * and at least one space around =.
#' @importFrom purrr map compact reduce map_lgl
#' @importFrom magrittr not
token_is_on_alligned_line <- function(pd_flat, op_before) {

  line_idx <- 1 + cumsum(pd_flat$lag_newlines)
  pd_flat$.lag_spaces <- lag(pd_flat$spaces)
  pd_by_line <- split(pd_flat, line_idx)

  line1_first_arg_name_and_eq_assign <- pd_by_line %>%
    map(line1_arg_name_and_eq_sub)
  eq_sub_line1 <- line1_first_arg_name_and_eq_assign %>%
    compact()

  if (length(eq_sub_line1) < 2) {
    # need at least two lines with eq_sub
    return(rep(FALSE, length = nrow(pd_flat)))
  }

  arg_name_and_eq_sub_are_aligned <- line1_first_arg_name_and_eq_assign %>%
    map_lgl(~identical(.x, eq_sub_line1[[1]]))

  if (sum(arg_name_and_eq_sub_are_aligned) < 2) {
    # first is obviously aligned with itself, so it needs one more.
    return(rep(FALSE, length = nrow(pd_flat)))
  }
  # Not sure if it's quicker to simply construct text from the nested pd and
  # then nchar(.text) to validate instead of doing complicated checking.
  # instead of also check

  has_correct_spacing_around_comma <- pd_by_line %>%
    map_lgl(has_correct_spacing_around_comma)

  has_correct_spacing_around_eq_sub <- pd_by_line %>%
    map_lgl(has_correct_spacing_around_eq_sub)

  pd_flat$.lag_spaces <- NULL

  pd_by_line_aligned <- arg_name_and_eq_sub_are_aligned &
    has_correct_spacing_around_comma &
    has_correct_spacing_around_eq_sub
  map2(pd_by_line, pd_by_line_aligned, ~ rep(.y, length = nrow(.x))) %>%
    unlist()
}

#' At least one space after comma, none before, for all but the last comma on
#' the line
#' @param pd_sub The subset of a parse table corresponding to one line.
#' @importFrom rlang seq2
#' @keywords internal
has_correct_spacing_around_comma <- function(pd_sub) {
  comma_tokens <- which(pd_sub$token == "','")
  if (length(comma_tokens) == 0) return(TRUE)
  relevant_comma_token <- comma_tokens[seq2(1, length(comma_tokens) - 1L)]
  correct_spaces_before <- pd_sub$.lag_spaces[relevant_comma_token] == 0
  correct_spaces_after <- pd_sub$spaces[relevant_comma_token] > 0
  all(correct_spaces_before) && all(correct_spaces_after)
}

#' At least one space around `EQ_SUB`
#' @keywords internal
#' @importFrom rlang seq2
has_correct_spacing_around_eq_sub <- function(pd_sub) {
  eq_sub_token <- which(pd_sub$token == "EQ_SUB")
  if (length(eq_sub_token) == 0) return(TRUE)
  relevant_eq_sub_token <- eq_sub_token[seq2(1, length(eq_sub_token) - 1L)]
  correct_spaces_before <- pd_sub$.lag_spaces[relevant_eq_sub_token] >= 1
  correct_spaces_after <- pd_sub$spaces[relevant_eq_sub_token] >= 1
  all(correct_spaces_before) && all(correct_spaces_after)
}

#' Computes the `line1` attribute for the first two tokens of the of a sub
#' parse table if it contains an `EQ_SUB`.
line1_arg_name_and_eq_sub <- function(pd_sub) {
  if (nrow(pd_sub) >= 2 && pd_sub$token[2] == "EQ_SUB") {
    cumsum(lag(nchar(pd_sub$text[1:2]), default = 0)) +
      cumsum(pd_sub$.lag_spaces[1:2])
  } else {
    NULL
  }
}
