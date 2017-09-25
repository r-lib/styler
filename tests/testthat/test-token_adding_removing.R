context("adding / removing token")

test_that("other manipulations are correct (add braces, semi-colon etc.)", {
  expect_warning(test_collection("token_adding_removing", "mixed_token",
                                 transformer = style_text), NA)
})


test_that("braces in if-else clause are added correctly", {
  expect_warning(test_collection("token_adding_removing", "if_else_strict",
                                 transformer = style_text), NA)
})

test_that("braces in if-else clause are added correctly", {
  expect_warning(test_collection("token_adding_removing", "if_else_non_strict",
                                 transformer = style_text, strict = FALSE), NA)
})
