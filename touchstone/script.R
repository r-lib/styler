# touchstone:::touchstone_clear() # deletes itself and sources
refs <- c(Sys.getenv("GITHUB_BASE_REF", "touchstone"), Sys.getenv("GITHUB_HEAD_REF", "touchstone"))
exprs_to_benchmark <- list(
  cache_applying = 'style_pkg("touchstone/sources/here", filetype = c("R", "rmd"))'
)
for (benchmark in names(exprs_to_benchmark)) {
  timings <- touchstone::benchmark_run_ref(
    refs,
    expr_before_benchmark = c("library(styler)", "cache_deactivate()"),
    !!!exprs_to_benchmark[[benchmark]],
    n = 10
  )

  timings <- touchstone::benchmark_read(benchmark, refs[1])

  library(ggplot2)
  library(magrittr)
  timings %>%
    ggplot(aes(x = elapsed, color = ref)) +
    geom_density()
  fs::path("touchstone/plots/", benchmark) %>%
    fs::path_ext_set("png") %>%
    ggsave()

  tbl <- timings %>%
    dplyr::group_by(.data$ref) %>%
    dplyr::summarise(m = mean(.data$elapsed)) %>%
    tibble::deframe()

  diff_percent <- round(100 * (tbl[refs[2]] - tbl[refs[1]]) / tbl[refs[1]], 1)
  cat(
    glue::glue("{benchmark}: round(tbl[refs[1]], 2)} -> {round(tbl[refs[2]], 2)} ({diff_percent}%)"),
    file = "touchstone/pr-comment/info.txt",
    append = TRUE
  )
}
