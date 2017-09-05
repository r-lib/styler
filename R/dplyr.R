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

bind_rows <- function(...) {
  return(dplyr::bind_rows(...))
}

if_else <- function(condition, true, false, missing = NULL) {
  ifelse(condition, true, false)
}

filter <- function(.data, ...) {
  subset(.data, ...)
}

left_join <- function(...) {
  return(dplyr::left_join(...))
  merge(...)
}

transmute <- function(...) {
  return(dplyr::transmute(...))
}

group_by <- function(...) {
  return(dplyr::group_by(...))
}

mutate <- function(...) {
  return(dplyr::mutate(...))
}

ungroup <- function(...) {
  return(dplyr::ungroup(...))
}

summarize_ <- function(...) {
  return(dplyr::summarize_(...))
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
