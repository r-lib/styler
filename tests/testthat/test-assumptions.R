test_that("documentation regarding desc is still accurate", {
  skip_on_cran()
  versions <- versions::available.versions("desc")
  if (max(as.package_version(versions$desc$version)) > "1.2") {
    rlang::abort("newer version of desc on CRAN. Adapt the warning and maybe add minimal version requirement?")
  }
})
