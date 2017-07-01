context("unindention")

test_that("round brackets are unindented correctly", {
  expect_warning(test_collection("unindention",
                                 "mixed",
                                 transformer = style_indent_curly_round,
                                 write_back = TRUE), NA)
})
