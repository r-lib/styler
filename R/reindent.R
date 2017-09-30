
#' Update the indention reference
#'
#' @param pd_nested A nested parse table.
#' @name update_indention_ref
NULL


#' @describeIn update_indention_ref Updates the reference id for all
#'   tokens in `pd_nested` if `pd_nested` contains a function call. Tokens that
#'   start on the same line as the opening parenthesis, are not themselves
#'   function calls or expressions wrapped in curly brackets are re-indented,
#'   that is, they are indented up to the level at which the call ends in
#'   terms of col2. We need to take the last from the first child because calls
#'   like package::function() can have three elements.
#' @examples
#' \dontrun{
#' # not re-indented
#' call(call(
#'   xyz
#' ))
#' # re-indented
#' call(call(1,
#'           2))
#' }
#' @importFrom purrr map_lgl
#' @importFrom rlang seq2
update_indention_ref_fun_call <- function(pd_nested) {
  current_is_call <- pd_nested$token_before[2] %in% c("SYMBOL_FUNCTION_CALL")
  non_comment <- which(pd_nested$token != "COMMENT")
  first_non_comment_after_call <- non_comment[non_comment > 2][1]
  if ((current_is_call) &&
      pd_nested$lag_newlines[first_non_comment_after_call] == 0) {
    candidates <- seq2(3, nrow(pd_nested) - 1)

    child_is_call <- map_lgl(pd_nested$child, is_function_call)
    child_is_curly_expr <- map_lgl(pd_nested$child, is_curly_expr)
    child_is_on_same_line <- cumsum(pd_nested$lag_newlines) == 0
    call_on_same_line <- child_is_call & child_is_on_same_line
    to_indent <- setdiff(candidates, which(call_on_same_line | child_is_curly_expr))

    pd_nested$indention_ref_id[to_indent] <- last(pd_nested$child[[1]]$id)
  }
  pd_nested
}

#' @describeIn update_indention_ref Updates the reference id for all
#'   tokens in `pd_nested` if `pd_nested` contains a function declaration.
#'   Tokens inside a function declaration are are re-indented,
#'   that is, they are indented up to the level at which the token FUNCTION
#'   ends in terms of col2.
#' @examples
#' \dontrun{
#' a <- function(x,
#'               y) {
#' x + y
#' }
#' }
#' @importFrom rlang seq2
update_indention_ref_fun_dec <- function(pd_nested) {
  if (pd_nested$token[1] == "FUNCTION") {
    seq <- seq2(3, nrow(pd_nested) - 1)
    pd_nested$indention_ref_id[seq] <- pd_nested$id[1]
  }
  pd_nested
}

#' Apply reference indention to tokens
#'
#' Applies the reference indention created with functions
#' [update_indention_ref()] to the flattened parse table. The indention
#' is applied to all token that inherit from a reference token sequentially,
#' i.e. by looping over the target tokens.
#' @inheritParams apply_ref_indention_one
apply_ref_indention <- function(flattened_pd) {
  target_tokens <- which(flattened_pd$id %in% flattened_pd$indention_ref_id)
  flattened_pd <- reduce(
    target_tokens,
    apply_ref_indention_one,
    .init = flattened_pd
  )
  flattened_pd
}

#' Applying reference indention of a target token
#'
#' Applies the indention level of `target_token` to all tokens that have
#' `target_token` as a reference. This includes adding spaces to the first
#' tokens on a line and updating the column `col1` and `col2` for all tokens
#' on that line so they are kept updated.
#' @param flattened_pd A flattened parse table
#' @param target_token The index of the token from which the indention level
#'   should be applied to other tokens.
apply_ref_indention_one <- function(flattened_pd, target_token) {
  token_points_to_ref <-
    flattened_pd$indention_ref_id == flattened_pd$id[target_token]
  first_token_on_line <- flattened_pd$lag_newlines > 0L
  token_to_update <- which(token_points_to_ref & first_token_on_line)

  # udate spaces
  copied_spaces <- flattened_pd$col2[target_token] + 1
  old_spaces <- flattened_pd$lag_spaces[token_to_update[1]]
  shift <- copied_spaces - old_spaces
  flattened_pd$lag_spaces[token_to_update] <-
    flattened_pd$lag_spaces[token_to_update] + shift

  # update col1 / col2
  cols_to_update <- flattened_pd$line1 %in% flattened_pd$line1[token_to_update]
  flattened_pd$col1[cols_to_update] <- flattened_pd$col1[cols_to_update] + shift
  flattened_pd$col2[cols_to_update] <- flattened_pd$col2[cols_to_update] + shift
  flattened_pd

}




#' Set indention of tokens that match regex
#'
#' Force the level of indention of tokens whose text matches a regular
#' expression pattern to be a certain amount of spaces. The rule
#' is only active for the first tokens on a line.
#' @param flattened_pd A flattened parse table.
#' @param pattern A character  with regular expressions to match against the token
#'   in `flattened_pd`.
#' @param target_indention The desired level of indention of the tokens that
#'   match `pattern`.
#' @param comments_only Boolean indicating whether only comments should be
#'   checked or all tokens.
#' @return A flattened parse table with indention set to `target_indention` for
#'   the tokens that match `regex.`
#' @importFrom purrr map flatten_int
set_regex_indention <- function(flattened_pd,
                                  pattern,
                                  target_indention = 0,
                                  comments_only = TRUE) {
  if (comments_only) {
    cond <- which(
      (flattened_pd$token == "COMMENT") & (flattened_pd$lag_newlines > 0)
    )
    if (length(cond) < 1) return(flattened_pd)
    to_check <- flattened_pd[cond,]
    not_to_check <- flattened_pd[-cond,]
  } else {
    to_check <- flattened_pd
    not_to_check <- NULL
  }

  indices_to_force <-
    map(pattern, grep, to_check$text) %>%
    flatten_int()

  to_check$lag_spaces[indices_to_force] <- target_indention
  bind_rows(to_check, not_to_check) %>%
    arrange(pos_id)
}

#' Return regex patterns for re-indention
#'
#' @name regex_for_reindention
NULL

#' @export
#' @describeIn regex_for_reindention Returns `NULL`, i.e. no pattern will
#'   match against this.
regex_none <- function() {
  NULL
}
