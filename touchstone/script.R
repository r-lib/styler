touchstone:::touchstone_clear()
warning(Sys.getenv("GITHUB_EVENT_PULL_REQUEST_HEAD_SHA"))

refs <- c(Sys.getenv("GITHUB_BASE_REF"), readLines("github_ref_name"))
warning(refs)
timings <- touchstone::benchmark_run_ref(
  refs,
  expr_before_benchmark = c("library(styler)", "cache_deactivate()"),
  expr_to_benchmark = 'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))',
  n = 1,
)

timings <- touchstone::benchmark_read(refs)

library(ggplot2)
library(magrittr)
timings %>%
  ggplot(aes(x = elapsed, color = ref)) +
  geom_density()

ggsave("touchstone/plots/density.png")
