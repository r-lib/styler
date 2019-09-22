# the brace expression is the last argument (classical testthat case)
test_that(x, {
  hh
})

test_that(x,
  {
    hh
  }
)


# there are multiple brace expressions that spread over multiple lines
# (classical tryCatch)
tryCatch({
  exp(x)
}, error = function(x) x)

tryCatch(
  {
    exp(x)
  },
  error = function(x) x
)

call({
  blibla
}, {
  blublo
})

# curly-curly is respected
fio({{x}})

test_that("x", {{ k }})
