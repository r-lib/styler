# touchstone:::touchstone_clear() # deletes itself and sources
refs <- c(Sys.getenv("GITHUB_BASE_REF"), Sys.getenv("GITHUB_HEAD_REF"))
warning(refs)
timings <- touchstone::benchmark_run_ref(
  refs,
  expr_before_benchmark = c("library(styler)", "cache_deactivate()"),
  expr_to_benchmark = 'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))',
  n = 2,
)

timings <- touchstone::benchmark_read(refs)

library(ggplot2)
library(magrittr)
timings %>%
  ggplot(aes(x = elapsed, color = ref)) +
  geom_density()

ggsave("touchstone/plots/density.png")

tbl <- timings %>%
  dplyr::group_by(.data$ref) %>%
  dplyr::summarise(m = mean(.data$elapsed)) %>%
  tibble::deframe()

diff_percent <- round(100 * (tbl[ref[2]] - tbl[ref[1]]) / tbl[ref[1]])
writeLines(glue::glue('{tbl[ref[2]]} -> {tbl[ref[1]]} ({diff_percent}%)'), 'touchstone/pr-comment/info.txt')
