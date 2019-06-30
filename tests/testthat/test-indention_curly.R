context("indent curly brackets")

test_that("indention on one-liner curley only is not changed", {
  expect_warning(test_collection("indention_curly_brackets",
                                 "one_line_curly",
                                 transformer = style_text), NA)

})

test_that("indention with multi-line curley only is correct", {
  expect_warning(test_collection("indention_curly_brackets",
                                 "multi_line_curly_only",
                                 transformer = style_text_without_curly_curly), NA)

})


test_that("indention with multi-line curley and round is correct", {
  expect_warning(test_collection("indention_curly_brackets",
                                 "multi_line_curly_round_only",
                                 transformer = style_text), NA)

})



test_that(paste("complete styling via top level api is correct",
                "(round, curly, spacing)"), {
  expect_warning(test_collection("indention_curly_brackets",
                                 "multi_line_curly_round_spacing",
                                 transformer = style_text), NA)

  expect_warning(test_collection("indention_curly_brackets",
                                 "multi_line_curly_while_for_if_fun",
                                 transformer = style_text), NA)

})
