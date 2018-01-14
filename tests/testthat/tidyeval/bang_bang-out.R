nameshift <- c(SL = "Sepal.Length")
head(dplyr::rename(iris[, 1:2], ! ! !nameshift), 3)
head(dplyr::rename(iris[, 1:2], !! !nameshift), 3)
head(dplyr::rename(iris[, 1:2], !!!nameshift), 3)
my_summarise <- function(df, group_var) {
  df %>%
    group_by(! !group_var) %>%
    summarise(a = mean(a))
}
