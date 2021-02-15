context("test-roxygen-examples-parse")

test_that("simple examples can be parsed", {
  expected_out <- "x <- 1\n"
  expect_equal(parse_roxygen(c("#' @examples", "#' x <- 1")), expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples", "#' x <- 1")), expected_out)
  expect_equal(parse_roxygen(c("#'@examples ", "#' x <- 1")), expected_out)
  expect_equal(parse_roxygen(c("#'@examples \t", "#' x <- 1")), expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples \t", "#' x <- 1")), expected_out)
  expect_equal(parse_roxygen(c("#' \t@examples \t", "#' x <- 1")), expected_out)

  # with code on same line
  expect_equal(parse_roxygen(c("#' @examples 2", "#' x <- 1")), c("2\n", expected_out))
  expect_equal(parse_roxygen(c("#'\t@examples 2", "#' x <- 1")), c("2\n", expected_out))
  expect_equal(parse_roxygen(c("#'@examples  2", "#' x <- 1")), c("2\n", expected_out))
  expect_equal(parse_roxygen(c("#'@examples \t 2", "#' x <- 1")), c("2\n", expected_out))
  expect_equal(parse_roxygen(c("#'\t@examples \t 2", "#' x <- 1")), c("2\n", expected_out))
  expect_equal(parse_roxygen(c("#' \t@examples \t2", "#' x <- 1")), c("2\n", expected_out))
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
        "#' fu(x = 3)", "#' }"
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
        "#' fu(x = 3)",
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
        "#' fu(x = 3)", "#' }"
      )
    ),
    c(
      "x <- '", "",
      "{", "'\n",
      "\\donttest", "{", "\n",
      "fu(x = 3)\n",
      "}",
      "\n"
    )
  )

  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' \\dontrun{",
        "#' fu(x = 3)",
        "#' }"
      )
    ),
    c(
      "x <- '", "", "{", "'\n",
      "\\dontrun", "{", "\n",
      "fu(x = 3)\n",
      "}", "\n"
    )
  )

  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' \\dontrun{",
        "#' c('{', \"'{{{\" ,\"[\")",
        "#'",
        "#'",
        "#' }",
        "#'",
        "#'"
      )
    ),
    c(
      "x <- '", "", "{", "'\n",
      "\\dontrun", "{", "\n",
      "c('{', \"'{{{\" ,\"[\")\n",
      "\n", "\n",
      "}\n", "\n", "\n"
    )
  )
  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' \\dontrun{",
        "#' x<-'{'",
        "#' }"
      )
    ),
    c(
      "x <- '", "", "{", "'\n",
      "\\dontrun", "{", "\n",
      "x<-'{'\n",
      "}\n"
    )
  )

  expect_equal(
    parse_roxygen(
      c(
        "#' @examples",
        "#' x <- '{'",
        "#' {",
        "#' 1 + 1",
        "#' }",
        "#' \\dontrun{",
        "#' {",
        "#' 1 + 1",
        "#' }",
        "#' }"
      )
    ),
    c(
      "x <- '", "", "{", "'\n",
      "", "{", "\n",
      "1 + 1\n",
      "}", "\n",
      "\\dontrun", "{", "\n",
      "{\n",
      "1 + 1\n",
      "}\n",
      "}", "\n"
    )
  )

  expect_equal(
    parse_roxygen(c(
      "#' @examples",
      "#' parse_roxygen(",
      "#'   c(",
      "#'     \"#' @examples\",",
      "#'     \"#' c(\\\"'{{{\\\")\"",
      "#'   )",
      "#' )"
    )),
    c(
      "parse_roxygen(\n",
      "  c(\n",
      "    \"#' @examples\",\n",
      "    \"#' c(\\\"'", "", "{", "", "{", "", "{", "\\\")\"\n",
      "  )\n",
      ")\n"
    )
  )
})
