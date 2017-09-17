lag <- function(x, n = 1L, default = NA, ...) {
  if (n == 0) {
    return(x)
  }
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(default, n), x[seq_len(xlen - n)])
  attributes(out) <- attributes(x)
  out
}

lead <- function(x, n = 1L, default = NA, ...) {
  if (n == 0) {
    return(x)
  }
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(x[-seq_len(n)], rep(default, n))
  attributes(out) <- attributes(x)
  out
}

arrange <- function(.data, ...) {
  stopifnot(is.data.frame(.data))
  ord <- eval(substitute(order(...)), .data, parent.frame())
  if (length(ord) != nrow(.data)) {
    stop("Length of ordering vectors don't match data frame size",
      call. = FALSE)
  }
  .data[ord, , drop = FALSE]
}

bind_rows <- function(x, y = NULL, ...) {
  if (is.null(x) && is.null(y)) {
    return(tibble())
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

if_else <- function(condition, true, false, missing = NULL) {
  ifelse(condition, true, false)
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
  res <- arrange(res, line1, col1, line2, col2, parent)

  # dplyr::left_join set unknown list columns to NULL, merge sets them
  # to NA
  if (exists("child", res) && any(is.na(res$child))) {
    res$child[is.na(res$child)] <- list(NULL)
  }
  res
}

nth <- function (x, n, order_by = NULL, default = x[NA_real_]) {
  stopifnot(length(n) == 1, is.numeric(n))
  n <- trunc(n)
  if (n == 0 || n > length(x) || n < -length(x)) {
    return(default)
  }
  if (n < 0) {
    n <- length(x) + n + 1
  }
  if (is.null(order_by)) {
    x[[n]]
  }
  else {
    x[[order(order_by)[[n]]]]
  }
}


last <- function (x, order_by = NULL, default = x[NA_real_]) {
  nth(x, -1L, order_by = order_by, default = default)
}

slice <- function(.data, ...) {
  .data[c(...), , drop = FALSE]
}
