#' Parallelization of styling
#'
#' [style_file()], [style_pkg()] and [style_dir()] leverage the `{future}`
#' framework for parallelization. To make use of parallel processing, you need
#' to have the package `{furrr}` installed. In that case, the strategy
#' [future::multiprocess()] is set temporarily during styling (only if there is
#' more than two files to style, because of start-up costs of parallelization).
#' You can prevent this temporary change in the future strategy by setting the
#' environment variable `R_STYLER_FUTURE_NO_OVERRIDE` to `TRUE` (case ignored). In
#' that case, the future strategy set before calling styler will be used, if
#' you have not set any, the future `{framework}` falls back to
#' [future::sequential()].
#'
#' @section Life-cycle:
#' The parallelization feature is experimental, in particular how its governed
#' with `R_STYLER_FUTURE_NO_OVERRIDE` and how progress bars are displayed with
#' `{furrr}`. In the future, most likely the `{progressr}` package will be used
#' to handle progress bars.
#' @name styler_future
NULL
