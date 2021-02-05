test_that("escaping of characters works", {
  expect_warning(test_collection("escaping", "basic",
    transformer = style_text
  ), NA)

  expect_error(test_collection("escaping", "fail",
    transformer = style_text
  ), "<text>:1:7: unexpected ")
})
