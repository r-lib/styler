context("styler")

test_that("can style example source file", {
  text <- style_text(
    utf8::read_lines_enc(rprojroot::find_testthat_root_file(
      "example", "in.R"))
  )
  message(utf8::transform_lines_enc(
      rprojroot::find_testthat_root_file("example", "out.R"),
      function(x) text))
})

test_that("can style example source file (relaxed)", {
  text <- style_text(
    utf8::read_lines_enc(rprojroot::find_testthat_root_file("example", "in.R")),
    strict = FALSE)
  expect_false(
    utf8::transform_lines_enc(
      rprojroot::find_testthat_root_file("example", "out-relaxed.R"),
      function(x) text)
  )
})

test_that("removes space at EOL", {
  expect_equal(style_text("a() "), "a()")
  expect_equal(style_text("a() # comment "), "a() # comment")
})

test_that("removes blank lines at EOF", {
  expect_equal(style_text(c("a() ", "", "")), "a()")
})


test_that("Space placed after 'if' and before '('", {
  expect_equal(style_text(c("if(TRUE) x else y")), "if (TRUE) x else y")
})

test_that("space before comma is removed", {
  expect_equal(style_text("c(    1,       16    , 333 , 33 ,  1)"),
                          "c(1, 16, 333, 33, 1)")
})
