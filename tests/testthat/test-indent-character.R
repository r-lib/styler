test_that("indention character can be arbitrary", {
  sg <- function(indent_by = 1) {
    create_style_guide(
      indention = list(purrr::partial(indent_braces, indent_by = indent_by)),
      indent_character = "\t",
      style_guide_name = "test",
      style_guide_version = 1
    )
  }
  expect_equal(
    style_text("{\n1\n}", style = sg) %>%
      as.character(),
    c("{", "\t1", "}")
  )
})
