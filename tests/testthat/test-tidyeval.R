context("tidyeval")

test_that("bang bang", {
  expect_warning(test_collection("tidyeval", "bang_bang",
                                transformer = style_text), NA)
})

test_that("bang bang", {
  expect_warning(test_collection("tidyeval", "setting_var",
                                 transformer = style_text), NA)
})
