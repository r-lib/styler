get_stage("install") %>%
  add_code_step(remotes::install_deps(upgrade = FALSE)) %>%
  k()

get_stage("script") %>%
  add_code_step(source("tests/testthat/test-all.R"))
