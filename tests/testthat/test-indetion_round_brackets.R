context("Function calls with round brackets")

test_that("one-line function call yields correct indention", {
  code <- "a <- xyz(x, 22, if(x > 1) 33 else 4)"

  back_and_forth <- code %>%
    compute_parse_data_nested() %>%
    create_filler_nested() %>%
    indent_round_nested() %>%
    strip_eol_spaces_nested() %>%
    serialize_parse_data_nested()

  expect_identical(code, back_and_forth)
})

##  ............................................................................

indented_multi_line_correct <- c(
  "call(",
  "  1,",
  "  call2(",
  "    2, 3,",
  "    call3(1, 2, 22),",
  "    5",
  "  ),",
  "  144",
  ")"
)

indented_multi_line_random <- c(
  "            call(       ",
  "  1,",
  "  call2(               ",
  "    2, 3,",
  "call3(1, 2, 22),",
  "    5",
  "),",
  "           144",
  ")"
)

not_indented <- trimws(indented_multi_line_correct)

test_that(paste("multi-line function call without any indention",
                "yields correct indention"), {

  back_and_forth <- not_indented %>%
    compute_parse_data_nested() %>%
    create_filler_nested() %>%
    indent_round_nested() %>%
    strip_eol_spaces_nested() %>%
    serialize_parse_data_nested()

  expect_identical(indented_multi_line_correct, back_and_forth)
})

test_that(paste("multi-line function call with random indention",
                "yields correct indention"), {
  back_and_forth <- indented_multi_line_random %>%
    compute_parse_data_nested() %>%
    create_filler_nested() %>%
    indent_round_nested() %>%
    strip_eol_spaces_nested() %>%
    serialize_parse_data_nested()

  expect_identical(indented_multi_line_correct, back_and_forth)
})

