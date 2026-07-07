##  ............................................................................
##  strict = TRUE                                                           ####

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "just_if_strict",
    transformer = style_text
  ))
})

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "if_else_strict",
    transformer = style_text
  ))
})

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "if_else_if_else_strict",
    transformer = style_text
  ))
})


##  ............................................................................
##  strict = FALSE                                                          ####

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "just_if_non_strict",
    transformer = style_text, strict = FALSE
  ))
})

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "if_else_non_strict",
    transformer = style_text, strict = FALSE
  ))
})

test_that("token are added correctly to conditional statements", {
  expect_no_warning(test_collection(
    "insertion_comment_interaction", "if_else_if_else_non_strict",
    transformer = style_text, strict = FALSE
  ))
})
