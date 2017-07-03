#' Unindent a chlid if necessary
#'
#' check whether any of the children of `pd` has `token` on the same line as
#'   the closing `token` of pd. If so, unindent that token.
#' @inheritParams unindent_child
#' @importFrom purrr map
set_unindention_child <- function(pd, token = "')'", unindent_by) {
  if(all(pd$terminal) | all(pd$indent == 0)) return(pd)
  closing <- which(pd$token %in% token)
  if (length(closing) == 0) return(pd)
  closing_line <- pd$line1[closing]

  pd <- pd %>%
    mutate(candidate = (!terminal) & (line2 == closing_line))
  candidates <- pd %>%
    filter(candidate)

  non_candidates <- pd %>%
    filter(!candidate)
  candidates$child <- map(candidates$child,
                          unindent_child,
                          token = token,
                          unindent_by = abs(pd$indent[closing] - pd$indent[closing-1]))

  bind_rows(candidates, non_candidates) %>%
    arrange(line1, col1) %>%
    select(-candidate)
}

#' Unindent a child
#'
#' @param pd A parse table.
#' @param token The token the unindention should be based on.
#' @param unindent_by By how many spaces one level of indention is reversed.
unindent_child <- function(pd, token = "')'", unindent_by = 2) {
  closing <- which(pd$token %in% token)
  if (!("indent" %in% names(pd))) {
    pd$indent <- 0
  }
  if (length(closing) > 0) {
    pd$indent[closing] <- pd$indent[closing] - unindent_by
  }
  pd
}
