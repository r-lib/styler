#' Communicate a warning if necessary
#'
#' If round trip verification was not possible, issue a warning to review the
#' changes carefully.
#' @param changed Boolean with indicating for each file whether or not it has
#'   been changed.
#' @inheritParams can_verify_roundtrip
#' @keywords internal
communicate_warning <- function(changed, transformers) {
  if (any(changed, na.rm = TRUE) && !can_verify_roundtrip(transformers)) {
    cat("Please review the changes carefully!", fill = TRUE)
  }
}

#' Communicate the summary of styling
#'
#' @param changed Boolean with indicating for each file whether or not it has
#'   been changed.
#' @param ruler_width Integer used to determine the width of the ruler.
#' @keywords internal
communicate_summary <- function(changed, ruler_width) {
  cli::cat_rule(width = max(40, ruler_width))
  cat("Status\tCount\tLegend \n")
  cli::cat_bullet("\t", sum(!changed, na.rm = TRUE), "\tFile unchanged.", bullet = "tick")
  cli::cat_bullet("\t", sum(changed, na.rm = TRUE), "\tFile changed.", bullet = "info")
  cli::cat_bullet(bullet = "cross", "\t", sum(is.na(changed)), "\tStyling threw an error.")
  cli::cat_rule(width = max(40, ruler_width))
}

#' @importFrom rlang abort
#' @importFrom rlang is_installed
assert_data.tree_installation <- function() {
  if (!is_installed("data.tree")) {
    abort("The package data.tree needs to be installed for this functionality.")
  }
}

#' Assert the R.cache installation in conjunction with the cache config
#'
#' R.cache needs to be installed if caching functionality is enabled
#' @param installation_only Whether or not to only check if R.cache is
#'   installed.
#' @keywords internal
assert_R.cache_installation <- function(installation_only = FALSE,
                                        action = "abort") {
  # fail if R.cache is not installed but feature is actiavted.
  if (!rlang::is_installed("R.cache") &&
    ifelse(installation_only, TRUE, cache_is_activated())
  ) {
    msg_basic <- paste(
      "R package R.cache is not installed, which is needed when the caching ",
      "feature is activated. Please install the package with ",
      "`install.packages('R.cache')` and then restart R to enable the ",
      "caching feature of styler or permanently deactivate the feature by ",
      "adding `styler::cache_deactivate()` to your .Rprofile, e.g. via ",
      "`usethis::edit_r_profile()`.",
      sep = ""
    )

    if (action == "abort") {
      rlang::abort(msg_basic)
    } else {
      rlang::warn(paste0(
        msg_basic, " ",
        "Deactivating the caching feature for the current session."
      ))
      cache_deactivate(verbose = FALSE)
    }
  }
}
