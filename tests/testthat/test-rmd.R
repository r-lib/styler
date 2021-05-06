context("rmd")

test_that("can style .Rmd files", {
  expect_warning(test_collection("rmd", "simple",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)

  expect_warning(test_collection("rmd", "r_and_non_r_code_chunks",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)

  expect_warning(test_collection("rmd", "nested",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)

  expect_warning(test_collection("rmd", "no-tidy",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)

  ## new 3-5
  expect_warning(test_collection("rmd", "random3",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)
  expect_warning(test_collection("rmd", "random5",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)
  expect_warning(test_collection("rmd", "random6",
    transformer = transform_mixed,
    transformer_fun = style_text,
    filetype = "Rmd",
    write_tree = FALSE
  ), NA)
})
