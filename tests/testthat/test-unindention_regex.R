test_that("forced regex token-dependent indention", {
  expect_no_warning(test_collection(
    "unindention_regex", "regex_force_with",
    transformer = style_text, reindention = specify_reindention(c(
      "^#   ",
      "^##  ",
      "^### "
    ))
  ))
})

test_that("do not force regex token-dependent indention without pattern", {
  expect_no_warning(test_collection(
    "unindention_regex", "regex_force_no",
    transformer = style_text,
    reindention = specify_reindention(NULL)
  ))
})


test_that("forced regex token-dependent indention without pattern", {
  expect_warning(
    test_collection(
      "unindention_regex", "random_non_comment_indention",
      transformer = style_text, reindention = specify_reindention(
        regex_pattern = "bbx",
        indention = 5,
        comments_only = FALSE
      )
    ),
    NA
  )
})
