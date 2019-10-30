#' Initialize default style guide attributes
#'
#' This function initializes and removes various variables from the parse
#' table.
#' @param pd_flat A parse table.
#' @importFrom utils tail
#' @examples
#' string_to_format <- "call( 3)"
#' pd <- styler:::compute_parse_data_nested(string_to_format)
#' styler:::pre_visit(pd, c(default_style_guide_attributes))
#' @export
#' @keywords internal
default_style_guide_attributes <- function(pd_flat) {
    initialize_newlines(pd_flat) %>%
    initialize_spaces() %>%
    remove_attributes(c("line1", "line2", "col1", "col2", "parent", "id")) %>%
    initialize_multi_line() %>%
    initialize_indention_ref_pos_id() %>%
    initialize_indent() %>%
    validate_parse_data()
}

#' Initialize attributes
#'
#' @name initialize_attributes
#' @inheritParams default_style_guide_attributes
#' @keywords internal
NULL

#' @describeIn initialize_attributes Initializes `newlines` and `lag_newlines`.
#' @keywords internal
initialize_newlines <- function(pd_flat) {
  pd_flat$line3 <- lead(pd_flat$line1, default = tail(pd_flat$line2, 1))
  pd_flat$newlines <- pd_flat$line3 - pd_flat$line2
  pd_flat$lag_newlines <- lag(pd_flat$newlines, default = 0L)
  pd_flat$line3 <- NULL
  pd_flat
}

#' @describeIn initialize_attributes Initializes `spaces`.
#' @keywords internal
initialize_spaces <- function(pd_flat) {
  pd_flat$col3 <- lead(pd_flat$col1, default = tail(pd_flat$col2, 1) + 1L)
  pd_flat$col2_nl <- ifelse(pd_flat$newlines > 0L,
    rep(0L, nrow(pd_flat)), pd_flat$col2
  )
  pd_flat$spaces <- pd_flat$col3 - pd_flat$col2_nl - 1L
  pd_flat$col3 <- NULL
  pd_flat$col2_nl <- NULL
  pd_flat
}

remove_attributes <- function(pd_flat, attributes) {
  pd_flat[attributes] <- rep(list(NULL), length(attributes))
  pd_flat
}

#' @describeIn initialize_attributes Initializes `multi_line`.
#' @keywords internal
initialize_multi_line <- function(pd_flat) {
  nrow <- nrow(pd_flat)
  pd_flat$multi_line <- ifelse(pd_flat$terminal,
    rep(FALSE, nrow),
    rep(NA, nrow)
  )
  pd_flat
}

#' @describeIn initialize_attributes Initializes `indention_ref_ind`.
#' @keywords internal
initialize_indention_ref_pos_id <- function(pd_flat) {
  pd_flat$indention_ref_pos_id <- NA
  pd_flat
}

#' @describeIn initialize_attributes Initializes `indent`.
#' @keywords internal
initialize_indent <- function(pd_flat) {
  if (!("indent" %in% names(pd_flat))) {
    pd_flat$indent <- 0
  }
  pd_flat
}

#' @importFrom rlang abort
#' @describeIn initialize_attributes validates the parse data.
#' @keywords internal
validate_parse_data <- function(pd_flat) {
  if (any(pd_flat$spaces < 0L)) {
    abort("Invalid parse data")
  }
  pd_flat
}
