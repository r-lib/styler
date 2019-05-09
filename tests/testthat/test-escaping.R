test_that("escaping of characters works", {
  expect_warning(test_collection("escaping",
    transformer = style_text,
    write_back = TRUE
  ), "macro '")
})
