context("test tree creation")

test_that("create_trees outputs identical structure if trees have same structure", {
  skip_if_not_installed("DiagrammeR")
  eq <- "a <- fun(a = b)"
  arrow <- "a <- data.frame(x = qq)"
  expect_equal(
    create_tree(eq, structure_only = TRUE),
    create_tree(arrow, structure_only = TRUE)
  )
})

test_that("create_trees outputs are not identical structure if trees have different structure", {
  skip_if_not_installed("DiagrammeR")
  eq <- "a <- fun(a = 1:3)"
  arrow <- "a <- data.frame(x = qq)"
  expect_true(
    nrow(create_tree(eq, structure_only = TRUE)) !=
      nrow(create_tree(arrow, structure_only = TRUE))
  )
})
