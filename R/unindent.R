#' Unindent a child if necessary
#'
#' check whether any of the children of `pd` has `token` on the same line as the
#' closing `token` of pd. If so, unindent that token.
#' @inheritParams unindent_child
#' @keywords internal
set_unindention_child <- function(pd, token = "')'", unindent_by) {
  if (all(pd$indent == 0L) || all(pd$terminal)) {
    return(pd)
  }
  closing <- which(pd$token %in% token)
  if (length(closing) == 0L || pd$lag_newlines[closing] > 0L) {
    return(pd)
  }

  first_on_last_line <- last(
    c(1L, which(pd$lag_newlines > 0L | pd$multi_line > 0L))
  )
  on_same_line <- seq2(first_on_last_line, closing - 1L)
  cand_ind <- setdiff(on_same_line, which(pd$terminal))

  if (length(cand_ind) < 1L) {
    return(pd)
  }

  candidates <- vec_slice(pd, cand_ind)

  non_candidates <- vec_slice(pd, -cand_ind)

  candidates$child <- map(candidates$child,
    unindent_child,
    unindent_by = abs(pd$indent[closing] - pd$indent[closing - 1L])
  )

  vec_rbind(candidates, non_candidates) %>%
    arrange_pos_id()
}

#' Unindent a child
#'
#' @param pd A parse table.
#' @param token The token the unindention should be based on.
#' @param unindent_by By how many spaces one level of indention is reversed.
#' @keywords internal
unindent_child <- function(pd, token = c("')'", "'}'"), unindent_by = 2L) {
  closing <- which(pd$token %in% token)
  if (!("indent" %in% names(pd))) {
    pd$indent <- 0L
  }
  if ((length(closing) > 0L) && (closing == nrow(pd))) {
    pd$indent[closing] <- pd$indent[closing] - unindent_by
  }
  pd
}
