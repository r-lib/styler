context("test-utils")

test_that("non-comment-helpers", {
  pd <- compute_parse_data_nested("a <- # hi \n x %>% b()")
  child <- pd$child[[1]]
  expect_equal(previous_non_comment(child, 4), 2)
  expect_equal(next_non_comment(child, 2), 4)
})

test_that("files with and without blank EOF line are read correctly", {
  expect_known_value(
    read_utf8(test_path("reference-objects/missing-blank-at-EOF.R")),
    test_path("reference-objects/return-read-utf8-missing-EOF")
  )

  expect_known_value(
    read_utf8(test_path("reference-objects/non-missing-blank-at-EOF.R")),
    test_path("reference-objects/return-read-utf8-non-missing-EOF")
  )
})
