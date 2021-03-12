context("test-roxygen-examples-parse")

test_that("simple examples can be parsed", {
  expected_out <- c("\n", "x <- 1\n")
  expect_equal(parse_roxygen(c("#' @examples", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'@examples ", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'@examples \t", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples \t", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#' \t@examples \t", "#' x <- 1"))$text, expected_out)

  # with code on same line
  expected_out <- c("2\n", "x <- 1\n")
  expect_equal(parse_roxygen(c("#' @examples 2", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples 2", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'@examples  2", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'@examples \t 2", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#'\t@examples \t 2", "#' x <- 1"))$text, expected_out)
  expect_equal(parse_roxygen(c("#' \t@examples \t2", "#' x <- 1"))$text, expected_out)
})

test_that("donts can be parsed", {
  expect_equal(
    parse_roxygen(c("#' @examples", "#' \\dontrun{1}"))$text,
    c("\n", "\\dontrun", "{", "1", "}", "\n")
  )
  expect_equal(
    parse_roxygen(
      c(
        "#' @examplesIf (TRUE)",
        "#' \\donttest{",
        "#' fu(x = 3)", "#' }"
      )
    )$text,
    c(
      "(TRUE)\n",
      "\\donttest",
      "{", "\n",
      "fu(x = 3)\n",
      "}",
      "\n"
    )
  )
})

test_that("braces examples can be parsed", {
  expect_equal(
    parse_roxygen(
      c(
        "#' @examples x <- '{'",
        "#' \\donttest{",
        "#' fu(x = 3)", "#' }"
      )
    )$text,
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
        "#' @examplesIf c(c(c(TRUE)))",
        "#' x <- '{'",
        "#' \\dontrun{",
        "#' fu(x = 3)",
        "#' }"
      )
    )$text,
    c(
      "c(c(c(TRUE)))\n",
      "x <- '", "", "{", "'\n",
      "\\dontrun", "{", "\n",
      "fu(x = 3)\n",
      "}", "\n"
    )
  )

  expect_equal(
    parse_roxygen(
      c(
        "#' @examples  x <- '{'",
        "#' \\dontrun{",
        "#' c('{', \"'{{{\" ,\"[\")",
        "#'",
        "#'",
        "#' }",
        "#'",
        "#'"
      )
    )$text,
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
    )$text,
    c(
      "\n",
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
    )$text,
    c(
      "\n",
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
      "#' @examples parse_roxygen(",
      "#'   c(",
      "#'     \"#' @examples\",",
      "#'     \"#' c(\\\"'{{{\\\")\"",
      "#'   )",
      "#' )"
    ))$text,
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
