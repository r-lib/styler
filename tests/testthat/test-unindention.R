context("unindention")

test_that("round brackets are unindented correctly", {
  expect_warning(test_collection("unindention",
                                 "mixed",
                                 transformer = style_indent_curly_round,
                                 write_back = TRUE), NA)
})

test_that("tokens are not dropped in named vector", {
  expect_warning(test_collection("unindention",
                                 "vec",
                                 transformer = style_text,
                                 write_back = TRUE), NA)
})
