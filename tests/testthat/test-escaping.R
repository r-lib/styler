test_that("escaping of characters works", {
  expect_warning(test_collection("escaping", "basic",
    transformer = style_text
  ), NA)

  expect_error(test_collection("escaping", "fail-parsing-1",
    transformer = style_text
  ), "<text>:1:7: unexpected ")

  expect_error(test_collection("escaping", "fail-parsing-2",
    transformer = style_text
  ), "x <-")
})
