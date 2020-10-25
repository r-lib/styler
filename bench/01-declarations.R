#' Plot benchmarks against a base branch, if benchmarks there exist
#'
#' @param new_bm A new benchmark object.
#' @param new_bm_label The label of the new benchmark used as a title.
#' @param file The file path to write the plot.
plot_against_base <- function(new_bm,
                              new_bm_label = deparse(substitute(new_bm)),
                              file = paste0("plots/", new_bm_label, ".pdf")) {
  new_bm <- bench::as_bench_mark(new_bm)
  new_bm$expression <- bench:::new_bench_expr(Sys.getenv("GITHUB_HEAD_REF"))
  name <- unique(new_bm$name)
  stopifnot(length(name) == 1)
  branches <- gert::git_branch_list()
  last_commit_base_branch <- branches[branches$name == Sys.getenv("GITHUB_BASE_REF"), "commit", drop = TRUE]
  bm <- bench::cb_read()
  commit_is_reference <- bm$commit_hash == last_commit_base_branch
  if (any(commit_is_reference) && Sys.getenv("GITHUB_BASE_REF") != "") {
    # if a pull request
    reference <- bm[commit_is_reference, "benchmarks"][[1]][[1]]
    # if benchmark exists in base branch
    reference <- reference %>%
      dplyr::filter(.data$name %in% !!name)
    if (nrow(reference) > 0) {
      # if benchmark exists in base branch
      reference$expression <- bench:::new_bench_expr(Sys.getenv("GITHUB_BASE_REF"))
      new_bm <- dplyr::bind_rows(reference, new_bm)
      stopifnot(nrow(new_bm) == 2)
      diff_in_percent <- round(100 * diff(new_bm$p50) / new_bm$p50[1])
      pr_comment <- glue::glue("* {name}: {new_bm$p50[1]} -> {new_bm$p50[2]} ({diff_in_percent}%)\n")
      cat(pr_comment, file = "pr-comment/info.txt", append = TRUE)
    }
  }
  new_bm$branch <- factor(new_bm$expression)
  plot <- ggplot2::ggplot(new_bm) +
    ggplot2::geom_boxplot(ggplot2::aes(
      x = branch, ymin = p0,
      ymax = p100, lower = p25,
      middle = p50, upper = p75
    ),
    stat = "identity"
    ) +
    ggplot2::ggtitle(name)

  ggplot2::ggsave(file, plot)
}
