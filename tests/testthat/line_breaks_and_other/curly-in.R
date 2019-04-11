# { never on its own line
if (y == 0)
{
  1
} else {
  2
}

test_that("I am here",
          {
            a_test(x)
          })


# A { should always be followed by a new line
if (x > 3) { "x"
}

# A } should always go on its own line, unless it's followed by else or ).
if (x > 3) {
  "x"}

# ELSE can't be tested fully since "[...] } \n else [...]" is invalid R code.
if (1 > 3) {
  "x"
} else {
  "y"
}

test_that("I am here", {
  a_test(x)
}
)
