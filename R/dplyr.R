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

rename_ <- function(.data, ...) {
  return(dplyr::rename_(.data, ...))
  map <- list(...)
  nms <- names(.data)
  for (i in seq_along(map)) {
    .data[[map[[i]]]] <- .data[[nms[[i]]]]
    .data[[nms[[i]]]] <- NULL
  }
  .data
}

bind_rows <- function(...) {
  return(dplyr::bind_rows(...))
}

if_else <- function(condition, true, false, missing = NULL) {
  return(dplyr::if_else(condition, true, false, missing))
  ifelse(condition, true, false)
}

filter <- function(.data, ...) {
  return(dplyr::filter(.data, ...))
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

last <- function(...) {
  return(dplyr::last(...))
}

slice <- function(...) {
  return(dplyr::slice(...))
}
