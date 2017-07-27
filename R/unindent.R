#' Unindent a chlid if necessary
#'
#' check whether any of the children of `pd` has `token` on the same line as
#'   the closing `token` of pd. If so, unindent that token.
#' @inheritParams unindent_child
#' @importFrom purrr map
set_unindention_child <- function(pd, token = "')'", unindent_by) {
  if(all(pd$indent == 0) || all(pd$terminal) ) return(pd)
  closing <- which(pd$token %in% token)
  if (length(closing) == 0 || pd$lag_newlines[closing] > 0) return(pd)

  first_on_same_line <- last(which(pd$lag_newlines > 0))
  if (is.na(first_on_same_line)) {
    on_same_line <- 1:(closing - 1)
  } else {
    on_same_line <- first_on_same_line:(closing - 1)
  }
  cand_ind <- setdiff(on_same_line, which(pd$terminal))

  candidates <- pd[cand_ind, ]

  non_candidates <- pd[-cand_ind, ]

  candidates$child <- map(candidates$child,
                          unindent_child,
                          unindent_by = abs(pd$indent[closing] - pd$indent[closing-1]))

  bind_rows(candidates, non_candidates) %>%
    arrange(line1, col1)
}

#' Unindent a child
#'
#' @param pd A parse table.
#' @param token The token the unindention should be based on.
#' @param unindent_by By how many spaces one level of indention is reversed.
unindent_child <- function(pd, token = c("')'", "'}'"), unindent_by = 2) {
  closing <- which(pd$token %in% token)
  if (!("indent" %in% names(pd))) {
    pd$indent <- 0
  }
  if (length(closing) > 0) {
    pd$indent[closing] <- pd$indent[closing] - unindent_by
  }
  pd
}
