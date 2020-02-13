context("EQ_ASSIGN relocation")
# Tests code in R/relevel.R
test_that("tree hierarchy is the same no matter whether = or <- is used", {
  skip_if_not_installed("DiagrammeR")
  assign_left <- create_tree(
    "x <- 5

    if(x >= 5)
    y <- TRUE else
    y <- FALSE",
    structure_only = TRUE
  )
  assign_eq <- create_tree(
    "x = 5

    if(x >= 5)
    y = TRUE else
    y = FALSE",
    structure_only = TRUE
  )
  expect_equal(assign_eq, assign_left)

  assign_left <- create_tree(
    "x = b =  5",
    structure_only = TRUE
  )
  assign_eq <- create_tree(
    "x <- b <-  5",
    structure_only = TRUE
  )
  expect_equal(assign_eq, assign_left)


  assign_left_many <- create_tree(
    "x = b = c = d = r=  5",
    structure_only = TRUE
  )
  assign_eq_many <- create_tree(
    "x <- b <- c <- d <- r <- 5",
    structure_only = TRUE
  )
  expect_equal(assign_eq_many, assign_left_many)
})

test_that("braces are added in the right place in ifelse if eq_assign is in expr", {
  expect_warning(test_collection(
    "relocate_eq_assign", "eq_assign_ifelse_scope_tokens",
    transformer = style_text,
    style = tidyverse_style
  ), NA)
})

test_that("complicated reassignment works", {
  expect_warning(test_collection(
    "relocate_eq_assign", "eq_assign_multiple_tokens_eq_only",
    transformer = style_text,
    scope = "tokens",
    style = tidyverse_style
  ), NA)

  expect_warning(test_collection(
    "relocate_eq_assign", "eq_assign_multiple_tokens_mixed",
    transformer = style_text,
    scope = "tokens",
    style = tidyverse_style
  ), NA)
})


test_that("eq_assign is not replaced", {
  expect_warning(test_collection(
    "relocate_eq_assign", "eq_assign_ifelse_scope_line_breaks",
    transformer = style_text,
    scope = "line_breaks",
    style = tidyverse_style
  ), NA)
})
