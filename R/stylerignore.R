#' Add positional information of token to next terminal
#'
#' This is needed because at serialization time, we also have terminals only
#' and positional argument of non-terminals were already propagated to terminals
#' with [context_to_terminals()].
env_add_stylerignore <- function(pd) {
  if (!env_current$any_stylerignore) {
    env_current$stylerignore <- pd[0, ]
    return()
  }
  pd_temp <- pd[pd$terminal, ] %>%
    default_style_guide_attributes()
  pd_temp$lag_newlines <- pd_temp$lag_newlines
  pd_temp$lag_spaces <- lag(pd_temp$spaces, default = 0)
  env_current$stylerignore <- pd_temp[pd_temp$terminal & pd_temp$stylerignore, ]
}

#' Adds the stylerignore column
#'
#' If a token falls within a stylerignore tag, the column is set to `TRUE`,
#' otherwise to `FALSE`.
add_stylerignore <- function(parse_data) {
  parse_text <- trimws(parse_data$text)
  start_candidate <- parse_text == option_read("styler.ignore_start")
  parse_data$stylerignore <- FALSE
  env_current$any_stylerignore <- any(start_candidate)
  if (!env_current$any_stylerignore) {
    return(parse_data)
  }
  parse_data_terminals <- parse_data[parse_data$terminal, ]
  parse_data_lat_line1 <- lag(parse_data$line2, default = 0)
  on_same_line <- parse_data$line1 == parse_data_lat_line1
  cumsum_start <- cumsum(start_candidate & !on_same_line)
  cumsum_stop <- cumsum(parse_text == option_read("styler.ignore_stop"))
  parse_data$indicator_off <- cumsum_start + cumsum_stop
  is_invalid <- cumsum_start - cumsum_stop < 0 | cumsum_start - cumsum_stop > 1
  if (any(is_invalid)) {
    rlang::warn(c(
      "Invalid stylerignore sequences found. The order in which the markers",
      "must appear in code is `# styler: off`, always followed by",
      "`# styler: on`. Multiple sequences are allowed but can't be nested,",
      "the very last `# styler: on` can be omitted. stylerignore markers are",
      "ignored after the first invalid marker."
    ))
  }

  to_ignore <- as.logical(parse_data$indicator_off %% 2)
  to_ignore[is_invalid] <- FALSE
  single_lines_to_ignore <- parse_data$line1[start_candidate & on_same_line]
  to_ignore[parse_data$line1 %in% single_lines_to_ignore] <- TRUE
  parse_data$indicator_off <- NULL
  parse_data[to_ignore & parse_data$terminal, "stylerignore"] <- TRUE
  parse_data
}

#' Ensure correct positional information for stylerignore expressions
#'
#' * Get the positional information for tokens with a stylerignore tag from
#' `env_current`, which recorded that information from the input text.
#' * Replace the computed lag_newlines and lag_spaces information in the parse
#'   table with this information.
#' TODO also take token, as rules-replacement may have modified the tokens too.
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
  flattened_pd$lag_newlines <- ifelse(is.na(flattened_pd$lag_newlines.y),
    flattened_pd$lag_newlines.x,
    flattened_pd$lag_newlines.y
  )
  flattened_pd$lag_newlines.x <- NULL
  flattened_pd$lag_newlines.y <- NULL

  flattened_pd$lag_spaces <- ifelse(is.na(flattened_pd$lag_spaces.y),
    flattened_pd$lag_spaces.x,
    flattened_pd$lag_spaces.y
  )
  flattened_pd$lag_spaces.x <- NULL
  flattened_pd$lag_spaces.y <- NULL

  flattened_pd$text <- ifelse(is.na(flattened_pd$text.y),
    flattened_pd$text.x,
    flattened_pd$text.y
  )
  flattened_pd$text.x <- NULL
  flattened_pd$text.y <- NULL
  flattened_pd
}
