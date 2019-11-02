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

#' @importFrom rlang abort
arrange <- function(.data, ...) {
  ord <- eval(substitute(order(...)), .data, parent.frame())
  .data[ord, , drop = FALSE]
}

arrange_pos_id <- function(data) {
  pos_id <- data$pos_id
  if (is.unsorted(pos_id)) {
    data <- data[order(pos_id), , drop = FALSE]
  }
  data
}

bind_rows <- function(x, y = NULL, ...) {
  if (is.null(x) && is.null(y)) {
    return(new_tibble(list(), nrow = 0))
  }
  if (is.null(x)) {
    if (inherits(y, "data.frame")) {
      return(y)
    }
    return(do.call(rbind.data.frame, x))
  }
  if (is.null(y)) {
    if (inherits(x, "data.frame")) {
      return(x)
    }
    return(do.call(rbind.data.frame, x))
  }
  if (NCOL(x) != NCOL(y)) {
    for (nme in setdiff(names(x), names(y))) {
      y[[nme]] <- NA
    }
  }
  bind_rows(rbind.data.frame(x, y), ...)
}

filter <- function(.data, ...) {
  subset(.data, ...)
}

left_join <- function(x, y, by, ...) {
  if (rlang::is_named(by)) {
    by_x <- names(by)
    by_y <- unname(by)
  } else {
    by_x <- by_y <- by
  }
  res <- as_tibble(merge(x, y, by.x = by_x, by.y = by_y, all.x = TRUE, ...))
  res <- arrange(res, pos_id)

  # dplyr::left_join set unknown list columns to NULL, merge sets them
  # to NA
  if (exists("child", res) && any(is.na(res$child))) {
    res$child[is.na(res$child)] <- list(NULL)
  }
  res
}


last <- function(x) {
  x[[length(x)]]
}

slice <- function(.data, ...) {
  .data[c(...), , drop = FALSE]
}

#' @importFrom purrr as_mapper map
map_dfr <- function(.x, .f, ..., .id = NULL) {
  .f <- as_mapper(.f, ...)
  res <- map(.x, .f, ...)
  bind_rows(res, .id = .id)
}
