context("public API")



test_that("styler can style package", {
  expect_false(all(style_pkg(testthat_file("public-api", "xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(testthat_file("public-api", "xyzdir"))))
})

test_that("styler can style file", {
  expect_false(
    style_file(testthat_file("public-api", "xyzfile", "random-script.R"), strict = FALSE)
  )
})

test_that("styler does not return error when there is no file to style", {
  expect_error(style_dir(testthat_file("public-api", "xyzemptydir"), strict = FALSE), NA)
})



##  ............................................................................
##  highlighted region                                                      ####



test_that("styling active region works", {
  region_path_temp <- testthat_file("public-api", "xyzaddin", "addin_region_temp-in.R")

  region_path_perm <- testthat_file("public-api", "xyzaddin", "addin_region-in.R")
  file.copy(region_path_perm, region_path_temp)

  context <-
    structure(list(id = "2DBC78B7", path = "~/datasciencecoursera/styler/tests/testthat/public-api/xyzaddin/addin_region_temp-in.R",
    contents = c("fjkdsfa 2jy+wj/ 1+1 <?+d", ""), selection = structure(list(
    structure(list(range = structure(list(start = structure(c(1, 17),
    .Names = c("row", "column"), class = "document_position"), end = structure(c(1, 20),
    .Names = c("row", "column"
    ), class = "document_position")), .Names = c("start",
    "end"), class = "document_range"), text = "1+1"), .Names = c("range",
    "text"))), .Names = "", class = "document_selection")), .Names = c("id",
    "path", "contents", "selection"), class = "document_context")
  reference <- utf8::read_lines_enc(
    testthat_file("public-api", "xyzaddin", "addin_region-out.R")
  )

  result <- expect_error(
    {
      mockr::with_mock(
      get_rstudio_context = function() context,
      style_active_region(),
      .env = asNamespace("styler")
    )
    rstudioapi::documentSave(id = context$id)
    },
    NA)

  after_styling <- utf8::read_lines_enc(region_path_temp)
  expect_equivalent(after_styling, reference)
  unlink(region_path_temp)
})
