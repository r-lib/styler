test_that("documentation regarding desc is still accurate", {
  skip_on_cran()
  versions <- versions::available.versions("desc")
  if (max(as.package_version(versions$desc$version)) > "1.2") {
    rlang::abort("newer version of desc on CRAN. Adapt the warning and maybe add minimal version requirement?")
  }
})

test_that("use python3 with pip installations", {
  if (Sys.Date() > as.Date("2020-09-30")) {
    rlang::abort(paste0(
      "Please unpin version requirement for pre-commit in .travis.yml ",
      "for pip installations and make things work with Python3. Currently ",
      "pinned version 1.21 is the last with python2 support, which is the ",
      "system python in the travis R image as of May 2020."
    ))
  }
})
