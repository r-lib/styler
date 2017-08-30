#' Enrich parse table with space and line break information
#'
#' This function computes difference (as column and line difference) between two
#'   entries in the parse table and adds this information to the table.
#' @param pd_flat A parse table.
#' @return A parse table with two three columns: lag_newlines, newlines and
#'   spaces.
#' @importFrom utils tail
create_filler <- function(pd_flat) {

  pd_flat$line3 <- lead(pd_flat$line1, default = tail(pd_flat$line2, 1))
  pd_flat$col3 <- lead(pd_flat$col1, default = tail(pd_flat$col2, 1) + 1L)
  pd_flat$newlines <- pd_flat$line3 - pd_flat$line2
  pd_flat$lag_newlines <- lag(pd_flat$newlines, default = 0L)
  pd_flat$col2_nl <- if_else(pd_flat$newlines > 0L, 0L, pd_flat$col2)
  pd_flat$spaces <- pd_flat$col3 - pd_flat$col2_nl - 1L
  pd_flat$multi_line <- ifelse(pd_flat$terminal, FALSE, NA)
  pd_flat$indention_ref_id <- NA
  ret <- pd_flat[, !(names(pd_flat) %in% c("line3", "col3", "col2_nl"))]


  if (!("indent" %in% names(ret))) {
    ret$indent <- 0
  }

  if (any(ret$spaces < 0L)) {
    stop("Invalid parse data")
  }

  ret
}

