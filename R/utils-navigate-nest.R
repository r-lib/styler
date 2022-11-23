#' Find the index of the next or previous non-comment in a parse table.
#' @param pd A parse table.
#' @param pos The position of the token to start the search from.
#'
#' @examples
#' code <- "a <- # hi \n x %>% b()"
#' writeLines(code)
#' pd <- compute_parse_data_nested(code)
#' child <- pd$child[[1]]
#' previous_non_comment(child, 4L)
#' next_non_comment(child, 2L)
#' @family third-party style guide helpers
#' @export
next_non_comment <- function(pd, pos) {
  if (length(pos) < 1L || is.na(pos) || pos >= nrow(pd)) {
    return(integer(0L))
  }
  candidates <- seq2(pos + 1L, nrow(pd))
  if (all(candidates %in% which(pd$token == "COMMENT"))) {
    return(integer(0L))
  }
  setdiff(candidates, which(pd$token == "COMMENT"))[1L]
}

#' @export
#' @rdname next_non_comment
previous_non_comment <- function(pd, pos) {
  if (length(pos) < 1L || is.na(pos) || pos > nrow(pd)) {
    return(integer(0L))
  }
  candidates <- seq2(1L, pos - 1L)
  if (all(candidates %in% which(pd$token == "COMMENT"))) {
    return(integer(0L))
  }
  last(setdiff(candidates, which(pd$token == "COMMENT")))
}

#' Tell me what the next terminal is
#'
#' If the first is a terminal, return it. If not, go inside it and search the
#' next terminal
#' @param pd A nest.
#' @param stack Whether or not to also return information on the tokens that
#'   are between `pd` and the first terminal, so the returned data frame can be
#'   understood as a transition path from `pd` to the next terminal, instead of
#'   the information at the terminal only. The order is inside-out,
#'   i.e. the first non-terminal on top, the terminal last.
#' @param vars The variables to return.
#' @param tokens_exclude A vector with tokens to exclude. This can be helpful if
#'   one wants to find the next token that is not a comment for example.
#' @return
#' Returns a data frame (which is **not** a valid parse table for
#' `stack = TRUE`), with `vars` and another variable `position` that denotes
#' the index each element in the transition. This can be helpful in conjunction
#' with [purrr::pluck()] or [purrr::modify_in()] to reach the terminal in the
#' nested structure.
#' @keywords internal
#' @examples
#' withr::with_options(
#'   list(styler.cache_name = NULL), # temporarily deactivate cache
#'   {
#'     pd <- compute_parse_data_nested("if (TRUE) f()")
#'     styler:::next_terminal(pd)
#'   }
#' )
next_terminal <- function(pd,
                          stack = FALSE,
                          vars = c("pos_id", "token", "text"),
                          tokens_exclude = NULL) {
  pd$position <- seq2(1L, nrow(pd))
  pd <- pd[!(pd$token %in% tokens_exclude), ]
  if (pd$terminal[1L]) {
    pd[1L, c("position", vars)]
  } else {
    current <- next_terminal(
      pd$child[[1L]],
      stack = stack, vars = vars, tokens_exclude = tokens_exclude
    )
    if (stack) {
      bind_rows(pd[1L, c("position", vars)], current)
    } else {
      current
    }
  }
}


#' Find the index of the last comment in the sequence of comments-only tokens
#' after the token that has position `pos` in `pd`.
#' @param pd A parse table.
#' @param pos The position of the token to start the search from.
#' @keywords internal
extend_if_comment <- function(pd, pos) {
  if (pos == nrow(pd)) {
    return(pos)
  }
  if (pd$token[pos + 1L] == "COMMENT") {
    extend_if_comment(pd, pos + 1L)
  } else {
    pos
  }
}
