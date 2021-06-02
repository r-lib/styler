test_that("exclude regex for spell check hook matches expected files", {
  skip_if(not_conda())
  skip_if(on_cran())

  # declare
  re <- reticulate::import("re")

  pattern_read <- function(file) {
    readLines(file) %>%
      gsub("^ *exclude *: *>", "    exclude: |", .) %>%
      yaml::yaml.load() %>%
      purrr::keep(~ .x$id == "spell-check") %>%
      magrittr::extract2(1) %>%
      magrittr::extract2("exclude")
  }

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

  files_to_match <- c(
    "data/x",
    ".Rprofile",
    ".Renviron",
    "renv.lock",
    "renv/settings.dcf",
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
    ".Rproj",
    "some/X.pdf",
    "figure.png",
    "pciture.jpeg",
    ".github/workflows/r-cmd-check.yaml"
  )

  # run
  pattern <- pattern_read(system.file("pre-commit-hooks.yaml", package = "precommit"))

  individual_patterns <- setdiff(
    unlist(strsplit(pattern, "\n", fixed = TRUE)),
    c("(?x)^(", ")$")
  )

  expect_equal(length(individual_patterns), length(files_to_match))
  expect_silent(all(is_match(
    pattern,
    files_to_match
  )))
})
