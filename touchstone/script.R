library(touchstone)

refs_install()

clear_ref_caches <- function() {
  purrr::walk(c(ref_get_or_fail("GITHUB_BASE_REF"), ref_get_or_fail("GITHUB_HEAD_REF")),
    styler::cache_clear,
    ask = FALSE
  )
}

clear_ref_caches()


benchmark_run_ref(
  expr_before_benchmark = {
    library(styler)
    cache_deactivate()
  },
  without_cache = style_pkg("touchstone/sources/here", filetype = c("R", "rmd")),
  n = 30
)

clear_ref_caches()
benchmark_run_ref(
  expr_before_benchmark = {
    library(styler)
    cache_activate(gert::git_branch())
  },
  cache_applying = style_pkg("touchstone/sources/here", filetype = c("R", "rmd")),
  n = 30
)

clear_ref_caches()
benchmark_run_ref(
  expr_before_benchmark = {
    library(styler)
    cache_activate(gert::git_branch())
  },
  cache_recording = {
    gert::git_reset_hard(repo = "touchstone/sources/here")
    style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))
  },
  n = 30
)

clear_ref_caches()

benchmarks_analyze()
