lag <- function(x, n = 1L, default = NA) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  c(rep(default, n), x[seq_len(xlen - n)])
}

lead <- function(x, n = 1L, default = NA) {
  xlen <- length(x)
  n <- pmin(n, xlen)
  c(x[-seq_len(n)], rep(default, n))
}


arrange <- function(.data, ...) {
  ord <- eval(substitute(order(...)), .data, parent.frame())
  vec_slice(.data, ord)
}

arrange_pos_id <- function(data) {
  pos_id <- data$pos_id
  if (is.unsorted(pos_id)) {
    data <- vec_slice(data, order(pos_id))
  }
  data
}

filter <- function(.data, ...) {
  subset(.data, ...)
}

left_join <- function(x, y, by) {
  if (rlang::is_named(by)) {
    by_x <- names(by)
    by_y <- unname(by)
  } else {
    by_x <- by_y <- by
  }

  res <- merge(x, y, by.x = by_x, by.y = by_y, all.x = TRUE, sort = FALSE) %>%
    arrange_pos_id()
  res <- new_styler_df(res)
  # dplyr::left_join set unknown list columns to NULL, merge sets them
  # to NA
  if (exists("child", res) && anyNA(res$child)) {
    res$child[is.na(res$child)] <- list(NULL)
  }
  res
}


last <- function(x) {
  x[[length(x)]]
}

map_dfr <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  vec_rbind(!!!res)
}
