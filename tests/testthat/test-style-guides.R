test_that("inconsistent scope intput raises an error", {
  # inexistant scope
  expect_error(scope_normalize('animal'), 'must be one of ')
  expect_error(scope_normalize(I('animal')), 'must be one of ')
  expect_error(scope_normalize(I(c('animal', 'spaces'))), 'must be one of ')

  # other than one with character
  expect_error(scope_normalize(c("none", "tokens")), 'either of class `AsIs` or length')
})

test_that('consistent input yields right output', {
  levels <- c("none", "spaces", "indention", "line_breaks", "tokens")
  expect_equal(
    scope_normalize(I('tokens')),
    factor('tokens', levels = levels, ordered = TRUE)
  )
  expect_equal(
    scope_normalize(I('none')),
    factor('none', levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalize(I('indention')),
    factor('indention', levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalize(I(c('indention', 'tokens'))),
    factor(c('indention', 'tokens'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalize('spaces'),
    factor(c('none', 'spaces'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalize('indention'),
    factor(c('none', 'spaces', 'indention'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalize('line_breaks'),
    factor(c('none', 'spaces', 'indention', 'line_breaks'), levels = levels, ordered = TRUE)
  )
  expect_equal(
    scope_normalize('tokens'),
    factor(levels, levels = levels, ordered = TRUE)
  )

})
