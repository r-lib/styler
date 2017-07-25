context("linebreaking added / removed correctly")

test_that("other manipulations are correct", {
  expect_warning(test_collection("line_breaks_and_other", "other",
                  transformer = style_text), NA)
})


test_that("line breaks involing curly brackets", {
  expect_warning(test_collection("line_breaks_and_other", "curly",
                  transformer = style_text), NA)
})

test_that("adding and removing line breaks", {
  expect_warning(test_collection("line_breaks_and_other", "if",
                  transformer = style_text), NA)
})

test_that("adding and removing line breaks", {
  test_collection("line_breaks_and_other",
                  "if",
                  transformer = style_text)
})
