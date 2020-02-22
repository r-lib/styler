context("test-varia")

test_that("ensure_last_n_empty", {
  expect_equal(
    ensure_last_n_empty("x"),
    c("x", "")
  )
  expect_equal(
    ensure_last_n_empty(c("x", "")),
    c("x", "")
  )
  expect_equal(
    ensure_last_n_empty(c("1", "2")),
    c("1", "2", "")
  )
})

test_that("unsaved file is recognized from path", {
  expect_true(is_unsaved_file(""))
})

test_that("inexistant levels in factor creation lead to error", {
  expect_error(character_to_ordered(c("x", "Y"), levels = "x"))
})
