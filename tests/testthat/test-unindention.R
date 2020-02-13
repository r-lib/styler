context("unindention")

test_that("round brackets are unindented correctly", {
  expect_warning(test_collection("unindention",
    "mixed",
    transformer = style_text_without_curly_curly,
    write_back = TRUE
  ), NA)
})

test_that("tokens are not dropped in named vector", {
  expect_warning(test_collection("unindention",
    "vec",
    transformer = style_text,
    write_back = TRUE
  ), NA)
})


test_that(paste(
  "if last token is multi-line and no line break precede,",
  "unindention is correct"
), {
  expect_warning(test_collection("unindention",
    "vec",
    transformer = style_text,
    write_back = TRUE
  ), NA)
})
