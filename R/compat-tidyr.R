nest_ <- function(data, key_col, nest_cols = character()) {
  key_column <- setdiff(names(data), nest_cols)
  key_data <- data[[key_column]]
  key_levels <- unique(key_data)
  key_factor <- factor(key_data, levels = key_levels)
  res <- list()
  res[[key_column]] <- key_levels
  res[[key_col]] <- split(data[, nest_cols], key_factor)
  new_styler_df(res)
}
