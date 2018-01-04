context("rmd")

test_that("can style .Rmd files", {
  expect_error(test_collection(
    "rmd", "simple",
    transformer = transform_rmd,
    transformer_fun = style_text,
    write_tree = FALSE
  ), NA)
  expect_error(test_collection(
    "rmd", "r_and_non_r_code_chunks",
    transformer = transform_rmd,
    transformer_fun = style_text,
    write_tree = FALSE
  ), NA)
})
