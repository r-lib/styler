source(here::here("R", "infra.R"))
# success
run_test("styler-style-files", suffix = "-success.R")
# fail
run_test("styler-style-files", suffix = "-fail.R", error_msg = NA)

# success
run_test("usethis-use-tidy-description", "DESCRIPTION", suffix = "")


# success
run_test(
  "custom-no-browser-statement",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test(
  "custom-no-browser-statement",
  suffix = "-fail.R",
  error_msg = "contains a `browser()` statement."
)


# success
run_test("custom-parsable-R",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test("custom-parsable-R", suffix = "-fail.R", error_msg = "not parsable")
