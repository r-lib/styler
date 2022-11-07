library(testthat)
library(styler)
test_check("styler") # checks multiple files, in parallel

# checks file one by one, not parallel
Sys.setenv(STYLER_TEST_IS_TRULY_PARALLEL = FALSE)
test_file("testthat/test-cache-high-level-api.R")
test_file("testthat/tests-cache-require-serial.R")
