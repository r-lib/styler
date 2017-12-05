context("rmd")

test_that("can style .Rmd files", {
  test_collection(
    "rmd",
    transformer = transform_rmd,
    transformer_fun = style_text,
    write_tree = FALSE
  )
})
