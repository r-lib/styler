test_that("other manipulations are correct (add braces, semi-colon etc.)", {
  expect_no_warning(test_collection("token_adding_removing", "mixed_token",
    transformer = style_text
  ))
})


test_that("braces in if-else clause are added correctly", {
  expect_no_warning(test_collection("token_adding_removing", "if_else_strict",
    transformer = style_text
  ))
  expect_no_warning(test_collection("token_adding_removing", "if_else_non_strict",
    transformer = style_text, strict = FALSE
  ))
  expect_no_warning(test_collection("token_adding_removing", "if-else-comma",
    transformer = style_text, strict = TRUE
  ))
})

test_that("double braces are treated correctly", {
  expect_no_warning(test_collection("token_adding_removing", "double_braces",
    transformer = style_text
  ))
})

test_that("double braces are treated correctly", {
  expect_no_warning(test_collection("token_adding_removing", "token_creation_find_pos",
    transformer = style_text
  ))
})

test_that("braces only added to pipe if RHS is a symbol", {
  expect_no_warning(test_collection("token_adding_removing", "add_brackets_in_pipe",
    transformer = style_text
  ))
})



test_that("No braces are added if conditional statement is within pipe", {
  expect_no_warning(test_collection("token_adding_removing", "else-pipe",
    transformer = style_text
  ))
})

test_that("No brace is added within `substitute()`", {
  expect_no_warning(test_collection("token_adding_removing", "substitute",
    transformer = style_text
  ))
})


test_that("stylreignore interacts correctly with wrap_expr_in_curly", {
  expect_no_warning(test_collection("token_adding_removing", "if_else_stylerignore",
    transformer = style_text
  ))
})

test_that("stylreignore interacts correctly with wrap_expr_in_curly", {
  expect_no_warning(test_collection("token_adding_removing", "for_while_stylerignore",
    transformer = style_text
  ))
})
