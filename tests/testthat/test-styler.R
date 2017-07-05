context("styler")

test_that("can style example source file", {
  text <- style_text(utf8::read_lines_enc(
    rprojroot::find_testthat_root_file("example", "in.R")),
    flat = TRUE)
  expect_false(
    utf8::transform_lines_enc(
      rprojroot::find_testthat_root_file("example", "out.R"),
      function(x) text))
})

test_that("can style example source file (relaxed)", {
  text <- style_text(utf8::read_lines_enc(
    rprojroot::find_testthat_root_file("example", "in.R")),
    flat = TRUE,
    transformers = get_transformers(flat = TRUE, strict = FALSE))
  expect_false(
    utf8::transform_lines_enc(
      rprojroot::find_testthat_root_file("example", "out-relaxed.R"),
      function(x) text))
})

test_that("removes space at EOL", {
  expect_equal(style_text("a() ", flat = TRUE), "a()")
  expect_equal(style_text("a()  # comment ", flat = TRUE),
               "a()  # comment")
})

test_that("removes blank lines at EOF", {
  expect_equal(style_text(c("a() ", "", ""), flat = TRUE),
               "a()")
})


test_that("Space placed after 'if' and before '('", {
  expect_equal(style_text(c("if(TRUE) x else y"), flat = TRUE),
               "if (TRUE) x else y")
})

test_that("space before comma is removed", {
  expect_equal(style_text("c(    1,       16    , 333 , 33 ,  1)"),
                          "c(1, 16, 333, 33, 1)")
})
