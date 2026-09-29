test_that("spacing within comments is done correctly", {
  expect_no_warning(test_collection("parse_comments",
    "within_spacing_with_force",
    transformer = style_text,
    style = tidyverse_style,
    start_comments_with_one_space = TRUE
  ))

  expect_no_warning(test_collection("parse_comments",
    "within_spacing_without_force",
    transformer = style_text,
    style = tidyverse_style,
    start_comments_with_one_space = FALSE
  ))

  expect_no_warning(test_collection("parse_comments",
    "eol_eof_spaces",
    transformer = style_text
  ))
})

test_that("comments are treated corectly", {
  expect_no_warning(test_collection("parse_comments",
    "mixed",
    transformer = style_empty
  ))

  expect_no_warning(test_collection("parse_comments",
    "just_comments",
    transformer = style_empty
  ))


  expect_no_warning(test_collection("parse_comments",
    "with_indention",
    transformer = style_text
  ))
})

test_that("rplumber tags / syntax is handled properly", {
  expect_no_warning(test_collection("parse_comments",
    "rplumber",
    transformer = style_text
  ))
})


test_that("hashbangs are respected", {
  expect_no_warning(test_collection("parse_comments",
    "shebang",
    transformer = style_text
  ))
})

test_that("xaringan markers are respected", {
  expect_no_warning(test_collection("parse_comments",
    "xaringan",
    transformer = style_text
  ))
})

test_that("output prefix markers are respected", {
  expect_no_warning(test_collection("parse_comments",
    "output-prefix",
    transformer = style_text
  ))
})

test_that("code chunk headers for spinning are respected", {
  expect_no_warning(test_collection("parse_comments",
    "spinning_code_chunk_headers",
    transformer = style_text
  ))
})
