context("linebreaking added / removed correctly")

test_that("other manipulations are correct", {
  test_collection("line_breaks_and_other",
                  "other",
                  transformer = style_text)
})


test_that("line breaks involing curly brackets", {
  test_collection("line_breaks_and_other",
                  "curly",
                  transformer = style_text)
})
