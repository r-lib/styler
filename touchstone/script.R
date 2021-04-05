library(touchstone)

refs_install()

benchmark_run_ref(
  expr_before_benchmark = c("library(styler)", "cache_deactivate()"),
  without_cache = 'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))',
  n = 10
)

styler::cache_clear()

benchmark_run_ref(
  expr_before_benchmark = c("library(styler)", "cache_activate()"),
  cache_applying = 'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))',
  n = 10
)

styler::cache_clear()

benchmark_run_ref(
  expr_before_benchmark = c(
    "library(styler)",
    "cache_activate()"
  ),
  cache_recording = c(
    "gert::git_reset_hard(repo = 'touchstone/sources/here')",
    'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))'
  ),
  n = 10
)

styler::cache_clear()

benchmarks_analyze()
