context("indention operators")

test_that("pipe is indended correctly", {
  expect_warning(test_collection("indention_operators",
    "pipe",
    transformer = style_text,
    write_back = TRUE
  ), NA)
})

test_that("mathematical operators are indended correctly", {
  expect_warning(test_collection("indention_operators",
    "plus_minus",
    transformer = style_op
  ), NA)

  expect_warning(test_collection("indention_operators",
    "multiply_divide",
    transformer = style_op
  ), NA)
})


test_that("while / for / if without curly brackets", {
  expect_warning(test_collection("indention_operators",
    "while_for_if_without_curly_non_strict",
    transformer = style_text, strict = FALSE
  ), NA)
  expect_warning(test_collection("indention_operators",
    "while_for_without_curly_same_line_non_strict",
    transformer = style_text, strict = FALSE
  ), NA)

  expect_warning(test_collection("indention_operators",
    "if-else-no-braces-not-strict",
    transformer = style_text, strict = FALSE
  ), NA)
})

test_that("function multiline without curly brackets", {
  expect_warning(test_collection("indention_operators",
    "function-multiline-no-braces-strict",
    transformer = style_text, strict = TRUE
  ), NA)
  expect_warning(test_collection("indention_operators",
    "function-multiline-no-braces-non-strict",
    transformer = style_text, strict = FALSE
  ), NA)
})

test_that("while / for / if without curly brackets", {
  expect_warning(test_collection("indention_operators",
    "while_for_if_without_curly_strict",
    transformer = style_text, strict = TRUE
  ), NA)
})


test_that("nested for and indention", {
  expect_warning(
    test_collection("indention_operators",
      "nested-for-spacing-scope-indention",
      transformer = style_text, scope = "indention"
    ),
    NA
  )

  expect_warning(
    test_collection("indention_operators",
      "nested-for-spacing-scope-spaces",
      transformer = style_text, scope = "spaces"
    ),
    NA
  )
})

test_that("logical, special EQ_SUB and EQ_ASSIGN tokens are indented correctly", {
  expect_warning(test_collection("indention_operators",
    "logical_special",
    transformer = style_text, scope = "line_breaks"
  ), NA)

  expect_warning(test_collection("indention_operators",
    "eq_assign",
    transformer = style_text
  ), NA)
  expect_warning(test_collection("indention_operators",
    "eq_formal_simple",
    transformer = style_text
  ), NA)
})

test_that("dollar is indented and spaced correctly", {
  expect_warning(test_collection("indention_operators",
    "dollar",
    transformer = style_text
  ), NA)
})

test_that(
  "code is indented correctly if not first pontial trigger causes indention",
  {
    expect_warning(
      test_collection(
        "indention_operators", "not_first_trigger",
        transformer = style_text
      ),
      NA
    )
  }
)

test_that("indents eq_sub correctly with various levels of scope", {
  expect_warning(test_collection("indention_operators",
    "eq_sub_complex_indention",
    transformer = style_text, scope = "indention"
  ), NA)

  expect_warning(test_collection("indention_operators",
    "eq_sub_complex_tokens",
    transformer = style_text, scope = "tokens"
  ), NA)
})

test_that("indents eq_formals correctly with various levels of scope", {
  expect_warning(test_collection("indention_operators",
    "eq_formals_complex_indention",
    transformer = style_text, scope = "indention"
  ), NA)

  expect_warning(test_collection("indention_operators",
    "eq_formals_complex_tokens",
    transformer = style_text, scope = "tokens"
  ), NA)
})

test_that("overall", {
  expect_warning(test_collection("indention_operators",
    "overall",
    transformer = style_text
  ), NA)
})
