test_that("multiple round brackets don't cause extraindention", {
  expect_no_warning(test_collection("indention_multiple",
    "round_only",
    transformer = style_text
  ))

  expect_no_warning(test_collection("indention_multiple",
    "round_closing_on_same_line",
    transformer = style_text
  ))
})


test_that("multiple curly brackets don't cause extraindention", {
  expect_no_warning(test_collection("indention_multiple",
    "curly_only",
    transformer = style_text_without_curly_curly
  ))
})


test_that("multiple curly and round brackets don't cause extraindention", {
  expect_no_warning(test_collection("indention_multiple",
    "curly_and_round",
    transformer = style_text_without_curly_curly
  ))
})



test_that("multiple curly and round brackets overall test", {
  expect_no_warning(test_collection("indention_multiple",
    "overall",
    transformer = style_text
  ))
})

test_that("if and ifelse interacting with curly braces works", {
  expect_no_warning(test_collection("indention_multiple",
    "if_else_curly",
    transformer = style_text, strict = FALSE
  ))
})

test_that("edge cases work", {
  expect_no_warning(test_collection("indention_multiple",
    "edge_strict",
    transformer = style_text_without_curly_curly
  ))
})

test_that("token / braces interaction works", {
  expect_no_warning(test_collection("indention_multiple",
    "fun_for_new_line",
    transformer = style_text_without_curly_curly
  ))
})
