path_rel <- c(
  "dirty-sample-with-scope-tokens.R",
  "dirty-sample-with-scope-spaces.R",
  "clean-sample-with-scope-tokens.R"
)

path_dir <- file.path(
    "public-api", "xyzdir-dirty", path_rel
)

test_that("can style in parallel when no strategy is set and overriding is deactivated", {
  withr::local_envvar("R_STYLER_FUTURE_NO_OVERRIDE" = "TRUE")
  expect_match(
    catch_style_file_output(path_dir),
    'Styling +3',
    all = FALSE
  )
})

test_that("can style in parallel when no strategy is set and overriding is not deactivated", {
  withr::local_envvar("R_STYLER_FUTURE_NO_OVERRIDE" = "FALSE")
  expect_match(
    catch_style_file_output(path_dir),
    'Styling +3',
    all = FALSE
  )
})

test_that("can style in parallel when no strategy is set", {

  expect_match(
    catch_style_file_output(path_dir),
    'Styling +3',
    all = FALSE
  )
})

test_that("can style in parallel when strategy is set", {
  future::plan('sequential')
  expect_match(
    catch_style_file_output(path_dir),
    'Styling +3',
    all = FALSE
  )
})

test_that("with fewer than two files, the plan is not modified", {
  expect_match(
    catch_style_file_output(path_dir[1]),
    'Styling +1',
    all = FALSE
  )
})
