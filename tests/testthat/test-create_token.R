context("token insertion")

test_that("can create a token that has relevant columns", {
  pd_names <- c(
    "token", "text", "short", "lag_newlines", "newlines", "pos_id",
    "token_before", "token_after", "terminal", "internal",
    "spaces", "multi_line", "indention_ref_pos_id", "indent", "child",
    "stylerignore", "block", "is_cached"
  )

  expect_equal(
    names(create_tokens("'{'", "{", pos_ids = 3)),
    pd_names
  )
})

test_that("pos_id can be created", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3)
  new_id <- create_pos_ids(pd, 1L, by = 0.4)
  expect_error(
    bind_rows(
      create_tokens("XZY_TEST", "test", pos_ids = new_id),
      pd
    ),
    NA
  )
})


test_that("unambiguous pos_id won't be created (down)", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3)
  new_id <- create_pos_ids(pd, 1L, by = 0.4)
  pd <- bind_rows(
    create_tokens("XZY_TEST", "test", pos_ids = new_id),
    pd
  )
  expect_error(create_pos_id(pd, 1L, by = 0.4))
})

test_that("unambiguous pos_id won't be created (up)", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3)
  new_id <- create_pos_ids(pd, 1L, by = 0.4, after = TRUE)

  pd <- bind_rows(
    create_tokens("XZY_TEST", "test", pos_ids = new_id),
    pd
  )
  expect_error(create_pos_id(pd, 1L, by = 0.4, after = TRUE))
})
