
#' Create a style guide
#'
#' @param reindention A list of parameters for regex re-indention, most
#'   conveniently constructed using [specify_reindention()].
#' @examples
#' set_line_break_before_crly_opening <- function(pd_flat) {
#'   op <- pd_flat$token %in% "'{'"
#'   pd_flat$lag_newlines[op] <- 1L
#'   pd_flat
#' }
#' \dontshow{
#' {
#'   x
#' }
#' }
#' set_line_break_before_curly_opening_style <- function() {
#'   create_style_guide(line_break = tibble::lst(set_line_break_before_curly_opening))
#' }
#' \dontrun{
#' style_text("a <- function(x) { x }
#' ", style = set_line_break_before_curly_opening_style)
#' }
#' @importFrom purrr compact
#' @export
create_style_guide <- function(initialize = default_style_guide_attributes,
                               line_break = NULL,
                               space = NULL,
                               token = NULL,
                               indention = NULL,
                               use_raw_indention = FALSE,
                               reindention = tidyverse_reindention()) {
  lst(
    # transformer functions
    initialize = lst(initialize),
    line_break,
    space,
    token,
    indention,
    # transformer options
    use_raw_indention,
    reindention
  ) %>%
    map(compact)
}


#' Create a style guide
#'
#' @param reindention A list of parameters for regex re-indention, most
#'   conveniently constructed using [specify_reindention()].
#' @examples
#' set_line_break_before_crly_opening <- function(pd_flat) {
#'   op <- pd_flat$token %in% "'{'"
#'   pd_flat$lag_newlines[op] <- 1L
#'   pd_flat
#' }
#' \dontshow{
#' {
#'   x
#' }
#' }
#' set_line_break_before_curly_opening_style <- function() {
#'   create_style_guide(line_break = tibble::lst(set_line_break_before_curly_opening))
#' }
#' \donttest{
#' style_text("a <- function(x) { x }
#' ", style = set_line_break_before_curly_opening_style)
#' }
#' @importFrom purrr compact
#' @export
create_style_guide <- function(initialize = default_style_guide_attributes,
                               line_break = NULL,
                               space = NULL,
                               token = NULL,
                               indention = NULL,
                               use_raw_indention = FALSE,
                               reindention = tidyverse_reindention()) {
  lst(
    # transformer functions
    initialize = lst(initialize),
    line_break,
    space,
    token,
    indention,
    # transformer options
    use_raw_indention,
    reindention
  ) %>%
    map(compact)
}
