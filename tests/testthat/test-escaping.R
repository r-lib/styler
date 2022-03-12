test_that("escaping of characters works", {
  expect_warning(test_collection("escaping", "basic",
    transformer = style_text
  ), NA)

  expect_error(test_collection("escaping", "fail-parsing-1",
    transformer = style_text
  ), "<text>:2:7: unexpected ")

  expect_error(test_collection("escaping", "fail-parsing-2",
    transformer = style_text
  ), "x <-")

  expect_error(test_collection("escaping", "fail-parsing-3",
    transformer = style_text
  ))

  expect_error(test_collection("escaping", "fail-parsing-4",
    transformer = style_text
  ), "<text>:6:0:")
})
