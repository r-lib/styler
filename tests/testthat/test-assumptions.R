test_that("documentation regarding desc is still accurate", {
  skip_on_cran()
  if (packageVersion("desc") >= 1.3 && Sys.Date() > as.Date("2021-04-01")) {
    rlang::abort("newer version of desc on CRAN. Adapt the warning and maybe add minimal version requirement?")
  }
})
