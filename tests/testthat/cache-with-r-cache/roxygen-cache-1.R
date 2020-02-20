#' This shot
#'
#' @examples
#' mlflow_conda_bin <- function() {
#'   conda_home <- Sys.getenv("MLFLOW_CONDA_HOME", NA)
#'   conda <- if (!is.na(conda_home)) paste(conda_home, "bin", "conda", sep = "/") else "auto"
#'   conda_try <- try(conda_binary(conda = conda), silent = TRUE)
#'   if (class(conda_try) == "try-error") {
#'     msg <- paste(attributes(conda_try)$condition$message,
#'                  paste(
#'                    "  If you are not using conda, you can set the environment variable",
#'                    "MLFLOW_PYTHON_BIN to the path of your python executable."
#'                  ),
#'                  sep = " "
#'     )
#'     stop(msg)
#'   }
#'   conda_try
#' }
#'
#' if (x) {
#'   f(x, na.rm = 4)
#' } else {
#'   99
#' }
#'
#'
#' xx <-f4()
NULL
