test_that("exclude regex for spell check hook matches expected files", {
  skip_if(not_conda())
  skip_if(on_cran())
  re <- reticulate::import("re")
  pattern <- readLines(system.file("pre-commit-hooks.yaml", package = "precommit")) %>%
    gsub("^ *exclude *: *>", "    exclude: |", .) %>%
    yaml::yaml.load() %>%
    purrr::keep(~ .x$id == "spell-check") %>%
    magrittr::extract2(1) %>%
    magrittr::extract2("exclude")

  is_match <- function(pattern, x) {
    matches <- purrr::map_lgl(
      x,
      ~ !is.null(re$match(pattern, .x, flags = re$VERBOSE))
    )
    if (any(!matches)) {
      rlang::abort(paste(
        "The following expressions don't match the pattern: ",
        paste0(x[!matches], collapse = ", "), ". Did you forget a trailing |?"
      ))
    }
  }


  expect_silent(all(is_match(
    pattern,
    c(
      "data/x",
      ".Rprofile",
      ".Renviron",
      "vignettes/.gitignore",
      "NAMESPACE",
      "inst/WORDLIST",
      ".travis.yml",
      "appveyor.yml",
      "file.RData",
      ".Rbuildignore",
      "analysis.R",
      "linked/yids/gg.r",
      "things.py",
      "data/data.feather",
      "more/data.rds",
      "things/xx.Rds",
      ".pre-commit-",
      ".Rproj"
    )
  )))
})
