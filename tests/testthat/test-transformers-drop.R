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

test_that("tidyverse transformers are correctly dropped", {
  t_style <- tidyverse_style()

  t_fun <- transformers_drop(
    "x", t_style
  )
  # test that all dropping rules match an actual rule in the style guide
  scopes <- intersect(
    names(t_fun$transformers_drop),
    names(t_fun)
  )
  purrr::map2(t_fun$transformers_drop, t_style[scopes], function(x, y) {
    # all x must be in y. select the x that are not in y
    diff <- setdiff(names(x),names(y))
    if (length(diff) > 0) {
      rlang::abort(paste(
        "transformer_dropping specifies exclusion rules for transformers that ",
        "are not in the style guilde. Please add the rule to the style guide ",
        "or remove the dropping rules:", paste(diff, collapse = ", "))
      )
    }
  })
  names_line_break <- c(
    "set_line_break_around_comma_and_or",
    "set_line_break_after_assignment",
    "set_line_break_after_opening_if_call_is_multi_line",
    "set_line_break_before_closing_call",
    "remove_line_break_in_fun_call",
    "set_linebreak_after_ggplot2_plus"
  )
  expect_setequal(names(t_fun$line_break), names_line_break)

  names_spaces <- c(
    "remove_space_before_closing_paren",
    "remove_space_before_opening_paren",
    "remove_space_before_comma",
    "spacing_around_op",
    "remove_space_after_opening_paren",
    "set_space_between_levels"
  )

  expect_setequal(names(t_fun$space), names_spaces)

  names_indention <- c("indent_braces", "indent_op", "indent_without_paren")
  expect_setequal(names(t_fun$indention), names_indention)

  names_tokens <- c(
    "fix_quotes",
    if (getRversion() < 3.6) "force_assignment_op",
    "remove_terminal_token_before_and_after"
  )
  expect_setequal(names(t_fun$token), names_tokens)

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
