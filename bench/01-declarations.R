#' Plot benchmarks against a base branch, if benchmarks there exist
#'
#' @param new_bm A new benchmark object.
#' @param new_bm_label The label of the new benchmark used as a title.
#' @param file The file path to write the plot.
plot_against_base <- function(new_bm,
                              new_bm_label = deparse(substitute(new_bm)),
                              file = paste0("plots/", new_bm_label, ".pdf")) {
  new_bm <- bench::as_bench_mark(new_bm)
  branches <- gert::git_branch_list()
  last_commit_base_branch <- branches[branches$name == Sys.getenv("GITHUB_BASE_REF"), "commit", drop = TRUE]
  bm <- bench::cb_read()
  commit_is_reference <- bm$commit_hash == last_commit_base_branch
  if (any(commit_is_reference) && Sys.getenv("GITHUB_BASE_REF") != "") {
    # if a pull request
    reference <- bm[commit_is_reference, "benchmarks"][[1]][[1]]
    reference$expression <- bench:::new_bench_expr(Sys.getenv("GITHUB_BASE_REF"))
    new_bm$expression <- bench:::new_bench_expr(Sys.getenv("GITHUB_HEAD_REF"))
    new_bm <- rbind(reference, new_bm)
  }
  plot <- ggplot2::ggplot(new_bm) +
    ggplot2::geom_boxplot(ggplot2::aes(
      x = name, ymin = p0,
      ymax = p100, lower = p25,
      middle = p50, upper = p75
    ),
    stat = "identity"
    )

  ggplot2::ggsave(file, plot)
}
