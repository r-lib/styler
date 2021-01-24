# c/cp from remove_space_after_excl but for self-containement repeated
remove_space_after_excl_ <- function(pd_flat) {
  excl <- (pd_flat$token == "'!'") &
    (pd_flat$token_after != "'!'") &
    (pd_flat$newlines == 0L)
  pd_flat$spaces[excl] <- 0L
  pd_flat
}

t <- create_style_guide(
  space = lst(remove_space_after_excl_),
  transformers_drop = list(space = list(remove_space_after_excl_ = c("'!'"))),
  style_guide_name = "styler::t@https://github.com/r-lib",
  style_guide_version = as.character(packageVersion("styler"))
)

t_no_drop <- create_style_guide(
  space = lst(remove_space_after_excl_),
  transformers_drop = NULL,
)

t_empty_drop1 <- create_style_guide(
  space = lst(remove_space_after_excl_),
  transformers_drop = list(space = list()),
)

t_empty_drop2 <- create_style_guide(
  space = lst(remove_space_after_excl_),
  transformers_drop = list(),
)

test_that("transformers are not removed if they are used", {
  t_new <- transformers_drop(
    "!x", t
  )
  expect_equal(t_new, t)
})

test_that("transformers are removed if they are unused", {
  t_fun <- transformers_drop(
    "x", t
  )
  t_manual <- t
  t_manual$space$remove_space_after_excl_ <- NULL
  expect_equal(t_fun, t_manual)
})


test_that("if no transformers_drop is specified, no transformer is removed and no error issued", {
  t_fun <- transformers_drop(
    "x", t_no_drop
  )
  expect_equal(t_fun, t_no_drop)

  t_fun <- transformers_drop(
    "x", t_empty_drop1
  )
  expect_equal(t_fun, t_empty_drop1)

  t_fun <- transformers_drop(
    "x", t_empty_drop2
  )
  expect_equal(t_fun, t_empty_drop2)
})

test_that('semi-colon is parsed without error', {
  expect_equal(
    transformers_drop(c("!a", ";", "b"), t),
    t
  )
})


test_that('can handle old style guide without transformer object', {
  t_new <- t
  t_new$transformers_drop <- NULL
  expect_error(
    transformers_drop(c("!a", ";", "b"), t_new),
    NA
  )
  expect_error(
    style_text('1;3', transformers = t_new),
    NA
  )
})

test_that("can handle default", {
  t_no_drop <- create_style_guide(
    space = lst(remove_space_after_excl_),
    style_guide_name = "styler::t@https://github.com/r-lib",
    style_guide_version = as.character(packageVersion("styler"))
  )
  expect_error(
    transformers_drop(c("!a", ";", "b"), t_no_drop),
    NA
  )
  expect_error(
    style_text('a =2 ', transformers = t_no_drop),
    NA
  )
})
