
#' Create a style guide
#'
#' @param reindention A list of parameters for regex re-indention, most
#'   conveniently constructed using [specify_reindention()].
#' @examples
#' # empty
#'
#'
#' # two
#'
#'
#'
#'
#' # more
#' a <- 3
#' # a comment
#' \dontrun{
#' x
#'
#' y # hi
#'
#' # more
#'
#' a <- 3
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
