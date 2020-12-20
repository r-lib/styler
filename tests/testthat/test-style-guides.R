test_that("inconsistent scope intput raises an error", {
  # inexistant scope
  expect_error(scope_normalise('animal'), 'must be one of ')
  expect_error(scope_normalise(I('animal')), 'must be one of ')
  expect_error(scope_normalise(I(c('animal', 'spaces'))), 'must be one of ')

  # other than one with character
  expect_error(scope_normalise(c("none", "tokens")), 'either of class `AsIs` or length')
})

test_that('consistent input yields right output', {
  levels <- c("none", "spaces", "indention", "line_breaks", "tokens")
  expect_equal(
    scope_normalise(I('tokens')),
    factor('tokens', levels = levels, ordered = TRUE)
  )
  expect_equal(
    scope_normalise(I('none')),
    factor('none', levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalise(I('indention')),
    factor('indention', levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalise(I(c('indention', 'tokens'))),
    factor(c('indention', 'tokens'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalise('spaces'),
    factor(c('none', 'spaces'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalise('indention'),
    factor(c('none', 'spaces', 'indention'), levels = levels, ordered = TRUE)
  )

  expect_equal(
    scope_normalise('line_breaks'),
    factor(c('none', 'spaces', 'indention', 'line_breaks'), levels = levels, ordered = TRUE)
  )
  expect_equal(
    scope_normalise('tokens'),
    factor(levels, levels = levels, ordered = TRUE)
  )

})
