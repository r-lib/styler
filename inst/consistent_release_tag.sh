#!/usr/bin/env Rscript


path_config <- c(
  fs::path("inst", "pre-commit-config.yaml"),
  fs::path(".pre-commit-config.yaml")
)


assert_config_has_rev <- function(path_config) {
  file <- yaml::read_yaml(path_config) 
  repo <- purrr::map(file$repos, "repo") 
  
  lorenzwalthert_precommit_idx <- which(repo == "https://github.com/lorenzwalthert/precommit")
  stopifnot(length(lorenzwalthert_precommit_idx) == 1)
  rev <- file$repos[[lorenzwalthert_precommit_idx]]$rev
  
  latest_tag <- names(git2r::tags(".")[1])
  
  
  if (latest_tag != rev) {
    rlang::abort(glue::glue(
      "latest git tag is `{latest_tag}`, but in `{path_config}`, you the  ", 
      "revision is set to `{rev}` Please make the two correspond."
    ))
  }
}

purrr::walk(path_config, assert_config_has_rev)
