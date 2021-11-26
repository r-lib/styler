#!/usr/bin/env Rscript
library(magrittr)
read_config <- function(path) {
  yaml::read_yaml(path) %>%
    purrr::pluck("repos") %>%
    purrr::keep(~ .x$repo == "https://github.com/lorenzwalthert/precommit") %>%
    purrr::pluck(1, "hooks") %>%
    purrr::keep(~ .x$id == "spell-check") %>%
    purrr::pluck(1, "exclude")
}
hooks_pkg <- read_config(here::here("inst/pre-commit-config-pkg.yaml"))
hooks_proj <- read_config(here::here("inst/pre-commit-config-proj.yaml"))
if (!identical(hooks_proj, hooks_pkg)) {
  rlang::abort(paste0(
    "The `exclude` key from the spell-check hook should be the same for all templates in `inst/`."
  ))
}
