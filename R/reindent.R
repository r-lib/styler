#' Apply reference indention to tokens
#'
#' Applies the reference indention created with functions
#' [update_indention_ref()] to the flattened parse table. The indention
#' is applied to all token that inherit from a reference token sequentially,
#' i.e. by looping over the target tokens.
#' @inheritParams apply_ref_indention_one
#' @keywords internal
apply_ref_indention <- function(flattened_pd) {
  target_tokens <- which(
    flattened_pd$pos_id %in% flattened_pd$indention_ref_pos_id
  )
  flattened_pd <- Reduce(
    apply_ref_indention_one,
    target_tokens,
    init = flattened_pd
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
#' @keywords internal
apply_ref_indention_one <- function(flattened_pd, target_token) {
  token_to_update <- find_tokens_to_update(flattened_pd, target_token)
  # update spaces
  copied_spaces <- flattened_pd$col2[target_token]
  shift <- copied_spaces
  flattened_pd$lag_spaces[token_to_update] <-
    flattened_pd$lag_spaces[token_to_update] + shift

  # update col1 / col2
  cols_to_update <- flattened_pd$line1 %in% flattened_pd$line1[token_to_update]
  flattened_pd$col1[cols_to_update] <- flattened_pd$col1[cols_to_update] + shift
  flattened_pd$col2[cols_to_update] <- flattened_pd$col2[cols_to_update] + shift
  flattened_pd
}

#' Find the tokens to update when applying a reference indention
#'
#' Given a target token and a flattened parse table, the token for which the
#' spacing information needs to be updated are computed. Since indention is
#' already embedded in the column `lag_spaces`, only tokens at the beginning of
#' a line are of concern.
#' @param flattened_pd A flattened parse table.
#' @inheritParams apply_ref_indention_one
#' @seealso apply_ref_indention_one()
#' @examples
#' style_text("function(a =
#' b,
#' dd
#' ) {}", scope = "indention")
#' style_text("function(a,
#' b,
#' dd
#' ) {}", scope = "indention")
#' @keywords internal
find_tokens_to_update <- function(flattened_pd, target_token) {
  token_points_to_ref <-
    flattened_pd$indention_ref_pos_id == flattened_pd$pos_id[target_token]
  first_token_on_line <- flattened_pd$lag_newlines > 0L
  which(token_points_to_ref & first_token_on_line)
}


#' Set indention of tokens that match regex
#'
#' Force the level of indention of tokens whose text matches a regular
#' expression pattern to be a certain amount of spaces. The rule
#' is only active for the first tokens on a line.
#' @param flattened_pd A flattened parse table.
#' @param pattern A character  with regular expressions to match against the
#'   token in `flattened_pd`.
#' @param target_indention The desired level of indention of the tokens that
#'   match `pattern`.
#' @param comments_only Boolean indicating whether only comments should be
#'   checked or all tokens.
#' @return A flattened parse table with indention set to `target_indention` for
#'   the tokens that match `regex.`
#' @keywords internal
set_regex_indention <- function(flattened_pd,
                                pattern,
                                target_indention = 0L,
                                comments_only = TRUE) {
  if (comments_only) {
    cond <- which(
      (flattened_pd$token == "COMMENT") & (flattened_pd$lag_newlines > 0L)
    )
    if (length(cond) < 1L) {
      return(flattened_pd)
    }
    to_check <- flattened_pd[cond, ]
    not_to_check <- flattened_pd[-cond, ]
  } else {
    to_check <- flattened_pd
    not_to_check <- NULL
  }

  indices_to_force <-
    map(pattern, grep, to_check$text) %>%
    flatten_int()

  to_check$lag_spaces[indices_to_force] <- target_indention
  bind_rows(to_check, not_to_check) %>%
    arrange_pos_id()
}
