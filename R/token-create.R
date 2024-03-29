#' Create a terminal token
#'
#' Creates a terminal token represented as (a row of) a parse table.
#' @param tokens Character vector with tokens to create.
#' @param texts Character vector with texts of the token to create.
#' @param lag_newlines Character vector with lag_newlines corresponding to the
#'   tokens.
#' @param spaces Character vector with spaces corresponding to the tokens.
#' @param pos_ids Character vector with positional id corresponding to the
#'   tokens.
#' @param token_before Character vector corresponding to the columns
#'   `token_before`.
#' @param token_after Character vector corresponding to the columns
#'   `token_after`.
#' @param indention_ref_pos_ids Character vector with indention ref ids
#'   corresponding to the tokens.
#' @param indents Vector with indents corresponding to the tokens.
#' @param terminal Boolean vector indicating whether a token is a terminal or
#'   not.
#' @param child The children of the tokens.
#' @param stylerignore Boolean to indicate if the line should be ignored by
#'   styler. Must take value from token before, can't have a default.
#' @param block The block (of caching) to which the token belongs. An integer.
#' @param is_cached Whether the token is cached already.
#' @family token creators
#' @keywords internal
create_tokens <- function(tokens,
                          texts,
                          lag_newlines = 0L,
                          spaces = 0L,
                          pos_ids,
                          token_before = NA,
                          token_after = NA,
                          indention_ref_pos_ids = NA,
                          indents,
                          terminal = TRUE,
                          child = NULL,
                          stylerignore,
                          block = NA,
                          is_cached = FALSE) {
  len_text <- length(texts)
  new_styler_df(
    list(
      token = tokens,
      text = texts,
      short = substr(texts, 1L, 5L),
      lag_newlines = lag_newlines,
      newlines = lead(lag_newlines),
      pos_id = pos_ids,
      token_before = token_before,
      token_after = token_after,
      terminal = rep(terminal, len_text),
      internal = rep(FALSE, len_text),
      spaces = spaces,
      multi_line = rep(0L, len_text),
      indention_ref_pos_id = indention_ref_pos_ids,
      indent = indents,
      child = rep(list(child), len_text),
      stylerignore = stylerignore,
      block = block,
      is_cached = is_cached
    )
  )
}

#' Create valid pos_ids if possible
#'
#' @param pd A parse table.
#' @param pos The position where the new id should be inserted.
#' @param by By how much the reference `pos_id` should be increased / decreased
#'   to create a new id.
#' @param after Boolean indicating whether it should be inserted after or before
#'   `pos`.
#' @param n Number of ids to generate.
#' @return
#' Returns a valid sequences of pos_ids or an error if it was not possible to
#' create one. The validation is done with [validate_new_pos_ids()]
#' @family token creators
#' @keywords internal
create_pos_ids <- function(pd, pos, by = 0.1, after = FALSE, n = 1L) {
  direction <- if (after) {
    1L
  } else {
    -1L
  }
  first <- find_start_pos_id(pd, pos, by, direction, after)
  new_ids <- seq(first,
    to = first + direction * (n - 1L) * by, by = by * direction
  )
  validate_new_pos_ids(new_ids, after)
  new_ids
}

#' Find legit starting value for a new positional id
#'
#' Looks at the current nest as well as into its children (if necessary) to make
#' sure the right id is returned. Otherwise, ordering of tokens might not be
#' preserved.
#' @param direction Derived from `after`. `1` if `after = TRUE`, `-1` otherwise.
#' @param candidates The `pos_ids` of the candidates that origin from other
#'   nests.
#' @inheritParams create_pos_ids
#' @keywords internal
find_start_pos_id <- function(pd,
                              pos,
                              by,
                              direction,
                              after,
                              candidates = NULL) {
  candidates <- append(candidates, pd$pos_id[pos])
  if (is.null(pd$child[[pos]])) {
    start_pos_idx <- if (after) {
      max(candidates)
    } else {
      min(candidates)
    }
    start_pos_idx <- start_pos_idx + (by * direction)
  } else {
    start_pos_idx <- find_start_pos_id(
      pd$child[[pos]],
      if (after) {
        nrow(pd$child[[pos]])
      } else {
        1L
      },
      by,
      direction,
      after,
      candidates
    )
  }

  start_pos_idx
}



#' Validate sequence of new position ids
#'
#' Ids created with `after = TRUE` can have `pos_id` values between x.0 and
#' x.5 and ids created with `after = FALSE` can have `pos_id` values between
#' 1+ x.0 and 1 + x.5 where x is the `pos_id` integer which was used as a
#' reference to create the new `pos_ids`.
#' @param new_ids A vector with new ids
#' @param after Whether the ids are created with `after = TRUE` (and hence
#' should be in the range x.0-x.45) or not.

#' @family token creators
#' @keywords internal
validate_new_pos_ids <- function(new_ids, after) {
  ref <- if (after) {
    floor(new_ids)
  } else {
    ceiling(new_ids)
  }

  if (any(abs(new_ids - ref) > 0.5)) abort("too many ids assigned.")
}

#' Wrap an expression in curly braces
#'
#' Adds curly braces to an expression (represented as a parse table) if there
#' are none.
#' @param pd A parse table.
#' @param stretch_out Whether or not to create a line break after the opening
#'   curly brace and before the closing curly brace.
#' @param space_after How many spaces should be inserted after the closing brace.
#' @keywords internal
wrap_expr_in_curly <- function(pd,
                               stretch_out = c(FALSE, FALSE),
                               space_after = 1L) {
  if (is_curly_expr(pd)) {
    return(pd)
  }
  if (stretch_out[1L]) {
    pd$lag_newlines[1L] <- 1L
  }

  opening <- create_tokens("'{'", "{",
    pos_ids = create_pos_ids(pd, 1L, after = FALSE),
    spaces = 1L - as.integer(stretch_out[1L]),
    stylerignore = pd$stylerignore[1L],
    indents = pd$indent[1L]
  )

  closing <- create_tokens(
    "'}'", "}",
    spaces = space_after, lag_newlines = as.integer(stretch_out[2L]),
    pos_ids = create_pos_ids(pd, nrow(pd), after = TRUE),
    stylerignore = pd$stylerignore[1L],
    indents = pd$indent[1L]
  )

  vec_rbind(opening, pd, closing) %>%
    set_multi_line()
}
