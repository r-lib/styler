context("rnw")

test_that("can style .Rnw files", {
  expect_warning(test_collection(
    "rnw", "008-outdec",
    transformer = transform_rnw,
    transformer_fun = style_text,
    write_tree = FALSE
  ), NA)
  expect_warning(test_collection(
    "rnw", "011-conditional-eval",
    transformer = transform_rnw,
    transformer_fun = style_text,
    write_tree = FALSE
  ), NA)
})
