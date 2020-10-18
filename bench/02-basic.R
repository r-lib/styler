# drop all notes
# git update-ref -d refs/notes/benchmarks

library(styler)
library(magrittr)
path <- "sources/here"
dir.create("plots")
cache_clear(ask = FALSE)
cache_activate()
cache_info()

marker <- purrr::partial(
  bench::mark,
  min_iterations = 20,
  check = FALSE,
  filter_gc = FALSE,
  memory = TRUE # skip uncached first round
)

with_cache <- marker(
  with_cache = {
    style_pkg(path, filetype = c("R", "rmd"))
  }
)
cache_info()
gert::git_reset_hard(repo = path)
cache_deactivate()

without_cache <- marker(
  without_cache = {
    style_pkg(path, filetype = c("R", "rmd"))
  }
)
latest_bm <- bench::cb_read()$benchmarks[[1]]
split(latest_bm, latest_bm$name) %>%
  purrr::imap(plot_against_base)
