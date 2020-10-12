test_that("documentation regarding desc is still accurate", {
  skip_on_cran()
  if (packageVersion('desc') >= 1.3) {
    rlang::abort("newer version of desc on CRAN. Adapt the warning and maybe add minimal version requirement?")
  }
})

test_that("reticulate dev version still required to make tests pass on conda macOS because of https://github.com/rstudio/reticulate/issues/820", {
  skip_on_cran()
  if (packageVersion('reticulate') >= 1.18) {
    rlang::abort("You can depend on the latest CRAN release of reticulate instead of the dev version and remove this assumption test.")
  }
})
