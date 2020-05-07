context("Function calls with round brackets")

test_that("one-line function call yields correct indention", {
  expect_warning(test_collection("indention_round_brackets",
    "one_line",
    transformer = style_text
  ), NA)
})

##  ............................................................................

test_that(paste("multi-line function call yields correct indention"), {
  expect_warning(test_collection("indention_round_brackets",
    "multi_line",
    transformer = style_text
  ), NA)
})

##  ............................................................................


context("grouping arithmetic expressions with round brackets. ")

# Does NOT cover indention by operators such as +"

test_that("arithmetic grouping with braces yields correctly indention", {
  expect_warning(test_collection("indention_round_brackets",
    "arithmetic",
    transformer = style_text
  ), NA)
})
