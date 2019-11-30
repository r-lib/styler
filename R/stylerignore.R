#' Add positional information of token to next terminal
#'
#' This is needed because at serialization time, we also have terminals only
#' and positional argument of non-terminals were already propagated to terminals
#' with [context_to_terminals()].
#' @inheritParams add_stylerignore
#' @keywords internal
env_add_stylerignore <- function(pd_flat) {
  if (!env_current$any_stylerignore) {
    env_current$stylerignore <- pd_flat[0, ]
    return()
  }
  pd_flat_temp <- pd_flat[pd_flat$terminal, ] %>%
    default_style_guide_attributes()
  pd_flat_temp$lag_newlines <- pd_flat_temp$lag_newlines
  pd_flat_temp$lag_spaces <- lag(pd_flat_temp$spaces, default = 0)
  is_terminal_to_ignore <- pd_flat_temp$terminal & pd_flat_temp$stylerignore
  env_current$stylerignore <- pd_flat_temp[is_terminal_to_ignore, ]
}

#' Adds the stylerignore column
#'
#' If a token should be ignored, the column is set to `TRUE`,
#' otherwise to `FALSE`.
#' @details
#' A token is ignored iff one of the two conditions hold:
#'
#' - it falls between a start and a stop marker whereas the markers are on
#'   their own line. Which tokens are recognized as markers is controlled with
#'   the R options `styler.ignore_start` and `styler.ignore_stop`.
#' - it is not a comment, but the last token on the line is a marker.
#'
#' See examples in [stylerignore].
#' @param pd_flat A parse table.
#' @keywords internal
add_stylerignore <- function(pd_flat) {
  parse_text <- trimws(pd_flat$text)
  start_candidate <- parse_text == option_read("styler.ignore_start")
  pd_flat$stylerignore <- rep(FALSE, length(start_candidate))
  env_current$any_stylerignore <- any(start_candidate)
  if (!env_current$any_stylerignore) {
    return(pd_flat)
  }
  pd_flat_terminals <- pd_flat[pd_flat$terminal, ]
  pd_flat_lat_line1 <- lag(pd_flat$line2, default = 0)
  on_same_line <- pd_flat$line1 == pd_flat_lat_line1
  cumsum_start <- cumsum(start_candidate & !on_same_line)
  cumsum_stop <- cumsum(parse_text == option_read("styler.ignore_stop"))
  pd_flat$indicator_off <- cumsum_start + cumsum_stop
  is_invalid <- cumsum_start - cumsum_stop < 0 | cumsum_start - cumsum_stop > 1
  if (any(is_invalid)) {
    warn(paste0(
      "Invalid stylerignore sequences found, potentially ignoring some of the ",
      "markers set.\nSee `help(\"stylerignore\", \"styler\")`."
    ))
  }

  to_ignore <- as.logical(pd_flat$indicator_off %% 2)
  to_ignore[is_invalid] <- FALSE
  single_lines_to_ignore <- pd_flat$line1[start_candidate & on_same_line]
  to_ignore[pd_flat$line1 %in% single_lines_to_ignore] <- TRUE
  pd_flat$indicator_off <- NULL
  pd_flat[to_ignore & pd_flat$terminal, "stylerignore"] <- TRUE
  pd_flat
}

#' Ensure correct positional information for stylerignore expressions
#'
#' @param flattened_pd A flattened parse table.
#' @details
#' * Get the positional information for tokens with a stylerignore tag from
#'   `env_current`, which recorded that information from the input text.
#' * Replace the computed lag_newlines and lag_spaces information in the parse
#'   table with this information.
#' @keywords internal
apply_stylerignore <- function(flattened_pd) {
  if (!env_current$any_stylerignore) {
    return(flattened_pd)
  }
  pos_ids <- env_current$stylerignore$pos_id
  colnames_required_apply_stylerignore <- c(
    "pos_id", "lag_newlines", "lag_spaces", "text"
  )
  flattened_pd <- merge(
    flattened_pd,
    env_current$stylerignore[, colnames_required_apply_stylerignore],
    by = "pos_id", all.x = TRUE
  ) %>%
    as_tibble()
  flattened_pd %>%
    stylerignore_consolidate_col("lag_newlines") %>%
    stylerignore_consolidate_col("lag_spaces") %>%
    stylerignore_consolidate_col("text")
}

#' Consolidate columns after a merge
#'
#' After [base::merge()], all non-id columns that were present in `x` and `y`
#' do get a suffix `.x` and `.y`. If the `y` value is missing, use the `x`
#' value (because the information for this token was not stylerignored),
#' otherwise the `y` value (i.e. the styled value).
#' @param col A string indicating the name of the column that should be
#'   consolidated.
#' @inheritParams apply_stylerignore
#' @keywords internal
stylerignore_consolidate_col <- function(flattened_pd, col) {
  col_x <- paste0(col, ".x")
  col_y <- paste0(col, ".y")
  flattened_pd[[col]] <- ifelse(is.na(flattened_pd[[col_y]]),
    flattened_pd[[col_x]],
    flattened_pd[[col_y]]
  )
  flattened_pd[[col_x]] <- NULL
  flattened_pd[[col_y]] <- NULL
  flattened_pd
}
