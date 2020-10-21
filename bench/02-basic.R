# drop all notes
# git update-ref -d refs/notes/benchmarks

library(styler)
library(magrittr)
path <- "sources/here"
dir.create("plots")
cache_clear(ask = FALSE)

marker <- purrr::partial(
  bench::mark,
  min_iterations = 20,
  check = FALSE,
  filter_gc = FALSE,
  memory = TRUE # skip uncached first round
)

# basically applying cache only
# No transformer is ran ever because all code is already compliant.
cache_activate()
cache_info()
with_cache <- marker(
  cache_appyling = {
    style_pkg(path, filetype = c("R", "rmd"))
  }
)

# basically recording cache only
# transformers are always ran on expressions that were not compliant with the
# style guide (some but not all expressions are)
with_cache <- marker(
  cache_recording = {
    cat(
      bench::system_time(gert::git_reset_hard(repo = path))[["process"]],
      sep = "\n",
      file = "timing-reset",
      append = TRUE
    )
    style_pkg(path, filetype = c("R", "rmd"))
  }
)

cache_info()

# cache turned off
# recording and applying, transformers always ran on all expressions.
gert::git_reset_hard(repo = path)
cache_deactivate()
time_for_git_reset <- as.numeric(readLines("timing-reset"))
cat(
  "Waiting ",
  round(mean(time_for_git_reset), 3),
  " seconds on average to simulate git reset. That way, `without_cache` and ",
  "`cache_recording` are comparable. The 95% interval for reset are (",
  round(quantile(time_for_git_reset, 0.025), 3), ", ",
  round(quantile(time_for_git_reset, 0.975), 3), ").",
  sep = ""
)
without_cache <- marker(
  without_cache = {
    Sys.sleep(mean(time_for_git_reset))
    style_pkg(path, filetype = c("R", "rmd"))
  }
)

# visualize results
latest_bm <- bench::cb_read()$benchmarks[[1]]
split(latest_bm, latest_bm$name) %>%
  purrr::imap(plot_against_base)
