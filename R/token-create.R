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
#' @param parents Vector with `id` corresponding to the
#'   parent of the tokens we want to create.
#' @param token_before Character vector corresponding to the columns
#'   `token_before`.
#' @param token_after Character vector corresponding to the columns
#'   `token_after`.
#' @param indention_ref_ids Character vector with indention ref ids
#'   corresponding to the tokens.
#' @param indents Vector with indents corresponding to the tokens.
#' @param terminal Boolean vector indicating whether a token is a terminal or
#'   not.
#' @param child The children of the tokens.
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
                          indents = 0,
                          terminal = TRUE,
                          child = NULL) {
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
    terminal = rep(terminal, len_text),
    internal = rep(FALSE, len_text),
    spaces = spaces,
    multi_line = rep(FALSE, len_text),
    indention_ref_id = indention_ref_ids,
    indent = indents,
    child = rep(list(child), len_text)
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
create_pos_ids <- function(pd, pos, by = 0.1, after = FALSE, n = 1) {
  direction <- ifelse(after, 1L, -1L)
  first <- find_start_pos_id(pd, pos, by, direction, after)
  new_ids <- seq(first, to = first + direction * (n - 1) * by, by = by * direction)
  validate_new_pos_ids(new_ids, after)
  new_ids
}

#' Find legit starting value for a new positional id
#'
#' Looks at the current nest as well as into its children (if necessary) to make
#' sure the right id is returned. Otherise, ordering of tokens might not be
#' preserved.
#' @param direction Derived from `after`. `1` if `after = TRUE`, `-1` otherwise.
#' @inheritParams create_pos_ids
find_start_pos_id <- function(pd, pos, by, direction, after) {
  if (is.null(pd$child[[pos]])) {
    pd$pos_id[pos] + by * direction
  } else {

    find_start_pos_id(
      pd$child[[pos]], if_else(after, nrow(pd$child[[pos]]), 1L),
      by, direction, after
    )
  }
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
validate_new_pos_ids <- function(new_ids, after) {
  ref <- ifelse(after, floor(new_ids), ceiling(new_ids))
  if (any(abs(new_ids - ref) > 0.5)) stop("too many ids assigned")
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
#' are none. Because of the nature of the nested parse table, curly braces only appear
#' with a single expression between them, so `wrap_expr_in_curly()` actually
#' first wraps the expression to wrap in curly braces into a new expression and
#' then adds curly braces around this new expression.
#' @param pd A parse table.
#' @param stretch_out Whether or not to create a line break after the opening
#'   curly brace and before the closing curly brace.
wrap_expr_in_curly <- function(pd, stretch_out = FALSE) {
  if (is_curly_expr(pd)) return(pd)
  if (stretch_out) {
    pd$lag_newlines[1] <- 1L
  }
  expr <- create_tokens(
    "expr", "",
    pos_ids = create_pos_ids(pd, 1, after = FALSE),
    parents = create_parent_id(pd),
    child = pd,
    terminal = FALSE
  )
  opening <- create_tokens(
    "'{'", "{",
    pos_ids = create_pos_ids(expr, 1, after = FALSE),
    parents = NA
  )

  closing <- create_tokens(
    "'}'", "}", spaces = 1, lag_newlines = as.integer(stretch_out),
    pos_ids = create_pos_ids(pd, nrow(pd), after = TRUE),
    parents = NA
  )

  bind_rows(opening, expr, closing)
}
