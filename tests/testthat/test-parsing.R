test_that("long strings are parsed correctly", {
  expect_warning(
    test_collection("parsing", "long_strings", transformer = style_text),
    NA
  )
})

test_that("0x number representation is preserved with(out) L", {
  text <- "0x00000002L"
  expect_true(all(tokenize(text)$text == text))
  text <- "0x00000002"
  expect_true(all(tokenize(text)$text == text))
  text <- "a <- 0x2L"
  pd <- get_parse_data(text)
  expect_equal(gsub("0x2L?", "0x2L", pd$text), tokenize(text)$text)
})


test_that("CRLF EOLs fail with informative error", {
  expect_error(
    style_text("glück <- 3\r\n glück + 1"),
    "Please change the EOL character in your editor to Unix style and try again."
  )
  expect_error(
    style_text(c("glück <- 3", "glück + 1\r\n 3")),
    "Please change the EOL character in your editor to Unix style and try again."
  )
})


test_that("mixed CRLF / LF EOLs fail", {
  expect_error(
    style_text("a + 3 -4 -> x\nx + 2\r\n glück + 1"),
    "unexpected input"
  )
})

test_that("unicode can't be propprely handled on Windows for R < 4.2", {
  msg <- ifelse(getRversion() < "4.2" && is_windows(),
    "Can't parse input due to unicode restriction in base R\\.",
    NA
  )
  expect_error(style_text('suit <- "♠"'), msg)
})
