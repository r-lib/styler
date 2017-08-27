context("public API")

base <- rprojroot::find_testthat_root_file("public-api")

test_that("styler can style package", {
  expect_false(all(style_pkg(paste0(base, "/xyzpackage"))))
})

test_that("styler can style directory", {
  expect_false(all(style_dir(paste0(base, "/xyzdir/"))))
})

test_that("styler can style file", {
  expect_false(
    style_file(paste0(base, "/xyzfile/random-script.R"), strict = FALSE)
  )
})

test_that("styler does not return error when there is no file to style", {
  expect_error(style_dir(paste0(base, "/xyzemptydir"), strict = FALSE), NA)
})

# style_active_file() must be tested manually.
test_that("...", {
  before_styling <- utf8::read_lines_enc(paste0(base, "/xyzaddin/addin_region-in.R"))
  reference <- utf8::read_lines_enc(paste0(base, "/xyzaddin/addin_region-out.R"))
  result <- expect_error(
    testthat::with_mock(
    find_active_context = function() {
      context <- structure(list(
        id = "C533793B",
        path = paste0(base, "/xyzaddin/addin_region-in.R"),
        contents = c("fjkdsfa 2jy+wj/ 1+1 <?+d", ""),
        selection = structure(list(structure(list(range = structure(list(
          start = structure(c(1, 17), .Names = c("row", "column"), class = "document_position"),
          end = structure(c(1, 21), .Names = c("row", "column"),
                          class = "document_position")), .Names = c("start", "end"),
          class = "document_range"), text = "1+1 "), .Names = c("range", "text"))),
          .Names = "", class = "document_selection")),
        .Names = c("id", "path", "contents", "selection"), class = "document_context"
      )
      path <- context$path
      start <- context$selection[[1]]$range$start
      end <- context$selection[[1]]$range$end
      if (all(start == end)) return()
      if (end[2] == 1) {
        end[1] <- end[1] - 1
        end[2] <- 1000000L # because of range constraint in substr()
      }
      list(start = start, end = end, path = path)
      },
    style_active_region()
  ), NA)
  after_styling <- utf8::read_lines_enc(paste0(base, "/xyzaddin/addin_region-in.R"))
  expect_equivalent(after_styling, reference)
  utf8::write_lines_enc(before_styling, paste0(base, "/xyzaddin/addin_region-in.R"))
})
