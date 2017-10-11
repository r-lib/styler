#' Enrich parse table with space and line break information
#'
#' This function computes difference (as column and line difference) between two
#'   entries in the parse table and adds this information to the table.
#' @param pd_flat A parse table.
#' @importFrom utils tail
initialize_attributes <- function(pd_flat) {

  init_pd <-
    initialize_newlines(pd_flat) %>%
    initialize_spaces() %>%
    remove_line_col() %>%
    initialize_multi_line() %>%
    initialize_indention_ref_id() %>%
    initialize_indent() %>%
    validate_parse_data()
  init_pd
}

#' @describeIn initialize_attributes Initializes `newlines` and `lag_newlines`.
initialize_newlines <- function(pd_flat) {
  pd_flat$line3 <- lead(pd_flat$line1, default = tail(pd_flat$line2, 1))
  pd_flat$newlines <- pd_flat$line3 - pd_flat$line2
  pd_flat$lag_newlines <- lag(pd_flat$newlines, default = 0L)
  pd_flat$line3 <- NULL
  pd_flat
}

#' @describeIn initialize_attributes Initializes `spaces`.
initialize_spaces <- function(pd_flat) {
  pd_flat$col3 <- lead(pd_flat$col1, default = tail(pd_flat$col2, 1) + 1L)
  pd_flat$col2_nl <- if_else(pd_flat$newlines > 0L,
    rep(0L, nrow(pd_flat)), pd_flat$col2
  )
  pd_flat$spaces <- pd_flat$col3 - pd_flat$col2_nl - 1L
  pd_flat$col3 <- NULL
  pd_flat$col2_nl <- NULL
  pd_flat
}

remove_line_col <- function(pd_flat) {
  pd_flat[c("line1", "line2", "col1", "col2")] <- rep(list(NULL), 4)
  pd_flat
}

#' @describeIn initialize_attributes Initializes `multi_line`.
initialize_multi_line <- function(pd_flat) {
  nrow <- nrow(pd_flat)
  pd_flat$multi_line <- if_else(pd_flat$terminal,
    rep(FALSE, nrow),
    rep(NA, nrow)
  )
  pd_flat
}

#' @describeIn initialize_attributes Initializes `indention_ref_ind`.
initialize_indention_ref_id <- function(pd_flat) {
  pd_flat$indention_ref_id <- NA
  pd_flat
}

#' @describeIn initialize_attributes Initializes `indent`.
initialize_indent <- function(pd_flat) {
  if (!("indent" %in% names(pd_flat))) {
    pd_flat$indent <- 0
  }
  pd_flat
}

#' @describeIn initialize_attributes validates the parse data.
validate_parse_data <- function(pd_flat) {
  if (any(pd_flat$spaces < 0L)) {
    stop("Invalid parse data")
  }
  pd_flat
}
