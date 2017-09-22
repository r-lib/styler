context("token insertion")

test_that("can create a token that has relevant columns", {
  pd_names <-c(
    "token", "text", "short", "lag_newlines", "newlines", "pos_id",
    "parent", "token_before", "token_after", "terminal", "internal",
    "spaces", "multi_line", "indention_ref_id", "indent", "child"
  )

  expect_equal(
    names(create_tokens("'{'", "{", pos_ids = 3, parents = 99)),
    pd_names
  )
})

test_that("pos_id can be created", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3, parents = 0)
  new_id <- create_pos_ids(pd, 1L, by = 0.4)
  parents <- create_parent_id(pd)
  expect_error(
    bind_rows(
      create_tokens("XZY_TEST", "test", pos_ids = new_id, parents = parents),
      pd
    ),
  NA
  )
})


test_that("unambiguous pos_id won't be created (down)", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3, parents = 0)
  new_id <- create_pos_ids(pd, 1L, by = 0.4)
  parents <- create_parent_id(pd)
  pd <- bind_rows(
    create_tokens("XZY_TEST", "test", pos_ids = new_id, parents = parents),
    pd
  )
  expect_error(create_pos_id(pd, 1L, by = 0.4))
})

test_that("unambiguous pos_id won't be created (up)", {
  pd <- create_tokens("XZY_TEST", "test", pos_ids = 3, parents = 0)
  new_id <- create_pos_ids(pd, 1L, by = 0.4, after = TRUE)
  parents <- create_parent_id(pd)

  pd <- bind_rows(
    create_tokens("XZY_TEST", "test", pos_ids = new_id, parents = parents),
    pd
  )
  expect_error(create_pos_id(pd, 1L, by = 0.4, after = TRUE))
})

