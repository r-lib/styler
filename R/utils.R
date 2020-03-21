is_package <- function(base_path = here::here()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}


extract_spell_check_regex <- function() {
  yaml::read_yaml(here::here(".pre-commit-hooks.yaml")) %>%
    purrr::keep(~ .x$id == "spell-check") %>%
    magrittr::extract2(1) %>%
    magrittr::extract2("exclude")
}
