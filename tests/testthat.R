library(testthat)
library(precommit)
if (on_cran()) {
  versions <- versions::available.versions("desc")
  if (max(as.package_version(versions$desc$version)) > "1.2") {
    rlang::abort("newer version of desc on CRAN. Adapt the warning and maybe add minimal version requirement?")
  }
}

test_check("precommit")
