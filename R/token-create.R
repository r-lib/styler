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
#' @param indention_ref_ids Character vector with indention ref ids
#'   corresponding to the tokens.
#' @param indents Character vector with indents corresponding to the tokens.
#' @family token creators
create_tokens <- function(tokens,
                          texts,
                          lag_newlines = 0,
                          spaces = 0,
                          pos_ids,
                          parents,
                          token_before = NA,
                          token_after = NA,
                          indention_ref_ids = NA,
                          indents = 0) {
  len_text <- length(text)
  data_frame(
    token = tokens,
    text  = texts,
    short = substr(texts, 1, 5),
    lag_newlines = lag_newlines,
    newlines = lead(lag_newlines),
    pos_id = pos_ids,
    parent = parents,
    token_before = token_before,
    token_after = token_after,
    id = NA,
    terminal = rep(TRUE, len_text),
    internal = rep(FALSE, len_text),
    spaces = spaces,
    multi_line = rep(FALSE, len_text),
    indention_ref_id = indention_ref_ids,
    line1 = rep(NA, len_text),
    line2 = rep(NA, len_text),
    col1 = rep(NA, len_text),
    col2 = rep(NA, len_text),
    indent = indents,
    child = rep(list(NULL), len_text)
  )
}

#' Create a valid pos_id if possible
#'
#' @param pd A parse table.
#' @param pos The position where the new id should be inserted.
#' @param after Boolean indicating whether it should be inserted after or before
#'   `pos`.
#' @param by By how much the reference `pos_id` should be increased / decreased
#'   to create a new id.
#' @param n Number of ids to generate.
#' @return
#' Returns a valid pos_id or an error if it was not possible to create one. The
#' validation is done with [validate_new_pos_id()]
#' @family token creators
create_pos_id <- function(pd, pos, by = 0.1, after = FALSE, n = 1) {
  direction <- ifelse(after, 1L, -1L)
  first <- pd$pos_id[pos] + by * direction
  new_ids <- seq(first, to = first + direction * (n - 1) * by, by = by * direction)
  validate_new_pos_id(new_ids, after)
  new_ids
}

#' @describeIn create_pos_id Helper to create one id.
create_pos_id_one <- function(pd, pos, after = FALSE, by = 0.1) {
  new_id <- pd$pos_id[pos] + by * ifelse(after, 1L, -1L)
  validate_new_pos_id(new_id, after)
  new_id
}

#' Validate new position ids
#' @param new_ids A vector with new ids
#' @param after Whether the ids are created with `after = TRUE` (and hence
#' should be in the range x.0-x.45) or not.
#' @family token creators
validate_new_pos_id <- function(new_ids, after) {
  ref <- ifelse(after, floor(new_ids), ceiling(new_ids))
  if (any(abs(new_ids - ref) > 0.45)) stop("too many ids assigned")
}
#' Find the parent of a nest
#'
#' It is any id of the nest (nested parse table at one level
#' of nesting) since by definition, all tokens on a nest have the same id.
#' @param pd_nested A nested parse table.
find_parent <- function(pd_nested) {
  pd_nested$parent[1]
}

#' Create the parent id for a new token
#'
#' Just wraps [find_parent()], since it can also be used to obtain the parent id
#' that is needed to create a new token in this nest.
#' @inheritParams find_parent
create_parent_id <- function(pd_nested) {
  find_parent(pd_nested)
}

#' Wrap an expression in curly braces
#'
#' Adds curly braces to an expression (represented as a parse table) if there
#' are none.
#' @param pd A parse table.
wrap_expr_in_curly <- function(pd, stretch_out = FALSE) {
  if (is_curly_expr(pd)) return(pd)
  if (stretch_out) {
    pd$lag_newlines[1] <- 1L
  }
  opening <- create_tokens(
    "'{'", "{",
    pos_ids = create_pos_id(pd, 1, after = FALSE),
    parents = create_parent_id(pd)
  )

  closing <- create_tokens(
    "'}'", "}", spaces = 1, lag_newlines = as.integer(stretch_out),
    pos_id = create_pos_id(pd, nrow(pd), after = TRUE),
    parents = create_parent_id(pd)
  )
  bind_rows(opening, pd, closing)
}
