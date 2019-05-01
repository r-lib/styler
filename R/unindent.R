#' Unindent a child if necessary
#'
#' check whether any of the children of `pd` has `token` on the same line as the
#' closing `token` of pd. If so, unindent that token.
#' @inheritParams unindent_child
#' @importFrom purrr map
#' @importFrom rlang seq2
#' @keywords internal
set_unindention_child <- function(pd, token = "')'", unindent_by) {
  if (all(pd$indent == 0) || all(pd$terminal)) {
    return(pd)
  }
  closing <- which(pd$token %in% token)
  if (length(closing) == 0 || pd$lag_newlines[closing] > 0) {
    return(pd)
  }

  first_on_last_line <- last(c(1, which(pd$lag_newlines > 0 | pd$multi_line)))
  on_same_line <- seq2(first_on_last_line, closing - 1)
  cand_ind <- setdiff(on_same_line, which(pd$terminal))

  if (length(cand_ind) < 1) {
    return(pd)
  }

  candidates <- pd[cand_ind, ]

  non_candidates <- pd[-cand_ind, ]

  candidates$child <- map(candidates$child,
    unindent_child,
    unindent_by = abs(pd$indent[closing] - pd$indent[closing - 1])
  )

  bind_rows(candidates, non_candidates) %>%
    arrange(pos_id)
}

#' Unindent a child
#'
#' @param pd A parse table.
#' @param token The token the unindention should be based on.
#' @param unindent_by By how many spaces one level of indention is reversed.
#' @keywords internal
unindent_child <- function(pd, token = c("')'", "'}'"), unindent_by = 2) {
  closing <- which(pd$token %in% token)
  if (!("indent" %in% names(pd))) {
    pd$indent <- 0
  }
  if ((length(closing) > 0) && (closing == nrow(pd))) {
    pd$indent[closing] <- pd$indent[closing] - unindent_by
  }
  pd
}
