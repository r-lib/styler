context("test-roxygen-examples-parse")

test_that("simple examples can be parsed", {
  expect_equal(parse_roxygen(c("#' @examples", "#' x <- 1")), "x <- 1\n")
})

test_that("donts can be parsed", {
  expect_equal(
    parse_roxygen(c("#' @examples", "#' \\dontrun{1}")),
    c("\\dontrun", "{", "1", "}", "\n")
  )
  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' \\donttest{",
        "#' fu(x = 3)\n", "#' }"
      )
    ),
    c(
      "\\donttest",
      "{", "\n",
      "fu(x = 3)\n",
      "}",
      "\n"
    )
  )
})

test_that("Duplicate tags can be parsed", {
  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' fu(x = 3)\n",
        "#'@examples # more",
        "#' x == 3"
      )
    ),
    c(
      "fu(x = 3)\n",
      "# more\n",
      "x == 3\n"
    )
  )
})

test_that("braces examples can be parsed", {
  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' \\donttest{",
        "#' fu(x = 3)\n", "#' }"
      )
    ),
    c(
      "x <- '",
      "{", "'\n",
      "\\donttest", "{", "\n",
      "fu(x = 3)\n",
      "}",
      "\n",
      "\n"
    )
  )

  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' \\dontrun{",
        "#' fu(x = 3)\n",
        "#' }"
      )
    ),
    c(
      "x <- '", "{", "'\n",
      "\\dontrun", "{", "\n",
      "fu(x = 3)\n",
      "}", "\n", "\n"
    )
  )
})
