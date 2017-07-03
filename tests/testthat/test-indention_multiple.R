context("test indent multiple")

test_that("multiple round brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "round_only",
                                 transformer = style_indent_curly_round), NA)

  expect_warning(test_collection("indention_multiple",
                                 "round_closing_on_same_line",
                                 transformer = style_indent_curly_round), NA)
})


test_that("multiple curly brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "curly_only",
                                 transformer = style_indent_curly_round), NA)

})


test_that("multiple curly and round brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "curly_and_round",
                                 transformer = style_indent_curly_round,
                                 write_back = TRUE), NA)

})



test_that("multiple curly and round brackets overall test", {
  expect_warning(test_collection("indention_multiple",
                                 "overall",
                                 transformer = style_text,
                                 write_back = TRUE), NA)

})


test_that("edge cases work", {
  expect_warning(test_collection("indention_multiple",
                                 "edge_",
                                 transformer = style_text,
                                 write_back = TRUE), NA)

})
