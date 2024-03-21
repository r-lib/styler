df <- dplyr::rename(df,
  "xValues" = "Time",
  "xUnit" = "TimeUnit",
  "yValues" = "simulationValues",
  "yUnit" = "unit",
  "yDimension" = "dimension"
)
