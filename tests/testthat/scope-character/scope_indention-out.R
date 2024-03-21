# not adding line-break
if (x) {1 + 1 + +1} else {3}

# not removing line-break
# FIXME If linebreaks are not touched: Do not indent token-dependent before '{'
test_that("x",
  {
    my_test(call)
})

# do not replace assignment
a = 3
data_frame(a = 3)

# do not resolve semicolon
a <- function(x) x + 1;b;c

# don't add brackets in pipes
a %>%
  b %>%
  c
