source(here::here("R", "infra.R"))
# success
run_test("style-files", suffix = "-success.R")
# fail
run_test("style-files", suffix = "-fail.R", error_msg = NA)

# success
run_test("use-tidy-description", "DESCRIPTION", suffix = "")


# success
run_test(
  "no-browser-statement",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test(
  "no-browser-statement",
  suffix = "-fail.R",
  error_msg = "contains a `browser()` statement."
)


# success
run_test("parsable-R",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test("parsable-R", suffix = "-fail.R", error_msg = "not parsable")
