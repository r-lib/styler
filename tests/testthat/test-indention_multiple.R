context("test indent multiple")

test_that("multiple round brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "round_only",
                                 transformer = style_text), NA)

  expect_warning(test_collection("indention_multiple",
                                 "round_closing_on_same_line",
                                 transformer = style_text), NA)
})


test_that("multiple curly brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "curly_only",
                                 transformer = style_text_without_curly_curly), NA)

})


test_that("multiple curly and round brackets don't cause extraindention", {
  expect_warning(test_collection("indention_multiple",
                                 "curly_and_round",
                                 transformer = style_text_without_curly_curly), NA)

})



test_that("multiple curly and round brackets overall test", {
  expect_warning(test_collection("indention_multiple",
                                 "overall",
                                 transformer = style_text,
                                 write_back = TRUE), NA)

})

test_that("if and ifelse interacting with curly braces works", {
  expect_warning(test_collection("indention_multiple",
                                 "if_else_curly",
                                 transformer = style_text,
                                 write_back = TRUE, strict = FALSE), NA)
})

test_that("edge cases work", {
  expect_warning(test_collection("indention_multiple",
                                 "edge_strict",
                                 transformer = style_text_without_curly_curly), NA)
})

test_that("token / braces interaction works", {
  expect_warning(test_collection("indention_multiple",
                                 "fun_for_new_line",
                                 transformer = style_text_without_curly_curly), NA)
})

