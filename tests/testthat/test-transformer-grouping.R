context("grouped transformers")

test_that("no line-break manipulation", {
  expect_warning(
    test_collection("transformer_grouping", "scope_spaces",
                    transformer = style_text,
                    transformers = get_transformers(
                      flat = FALSE, scope = "spaces")
    ), NA)
})


test_that("no token manipulation", {
  expect_warning(
    test_collection("transformer_grouping", "scope_line_breaks",
                    transformer = style_text,
                    transformers = get_transformers(
                      flat = FALSE, scope = "line_breaks")
    ), NA)
})

test_that("no space manipulation", {
  expect_warning(
    test_collection("transformer_grouping", "scope_tokens",
                    transformer = style_text,
                    transformers = get_transformers(
                      flat = FALSE, scope = "tokens")
    ), NA
  )
})


test_that("no manipulation at all", {
  expect_warning(
    test_collection("transformer_grouping", "scope_none",
                    transformer = style_text,
                    transformers = get_transformers(
                      flat = FALSE,
                      scope = "none")
    ), NA)
})
