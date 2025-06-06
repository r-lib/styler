test_that("gives warning markers are not correct", {
  expect_warning(style_text(c(
    "1+1",
    "# styler: on",
    "# styler: off"
  )))
})

test_that("trailing spaces are stripped when checking marker and written back", {
  expect_equal(
    style_text(c(
      "# styler: off ",
      "1+1",
      "# styler: on "
    )) %>%
      as.character(),
    c("# styler: off", "1+1", "# styler: on")
  )
})

test_that("last stopping marker can be omitted", {
  expect_equal(
    style_text(c(
      "# styler: off",
      "1+1"
    )) %>%
      as.character(),
    c("# styler: off", "1+1")
  )
})

test_that("last stopping marker can be omitted", {
  expect_equal(
    style_text(c(
      "# styler: off",
      "call( 1)",
      " # styler: on",
      "call(2  +0)",
      "# styler: off",
      "x=2"
    )) %>%
      as.character(),
    c(
      "# styler: off", "call( 1)", "# styler: on", "call(2 + 0)",
      "# styler: off", "x=2"
    )
  )
})

test_that("works for one line", {
  expect_equal(
    style_text(c(
      "1+1",
      "1+1# styler: off",
      "1+1"
    )) %>%
      as.character(),
    c("1 + 1", "1+1# styler: off", "1 + 1")
  )
})


test_that("works with other markers", {
  expect_equal(
    withr::with_options(
      list(styler.ignore_start = "# startignore", styler.ignore_stop = "# xxx"),
      {
        style_text(c(
          "1+1",
          "1+1# startignore",
          "1+1"
        )) %>%
          as.character()
      }
    ),
    c("1 + 1", "1+1# startignore", "1 + 1")
  )
})


test_that("works for multiple markers inline", {
  withr::local_options(styler.ignore_start = "# noeq", )
  expect_equal(
    style_text(c(
      "1+1",
      "1+1# noeq",
      "1+1"
    )) %>%
      as.character(),
    c("1 + 1", "1+1# noeq", "1 + 1")
  )
})


test_that("works for multiple markers inline on one line", {
  withr::local_options(styler.ignore_start = "nolint start|styler: off")
  expect_equal(
    style_text(c(
      "1+1",
      "1+1# nolint start styler: off",
      "1+1"
    )) %>%
      as.character(),
    c("1 + 1", "1+1# nolint start styler: off", "1 + 1")
  )
})


test_that("works with other markers", {
  expect_warning(
    withr::with_options(
      list(styler.ignore_start = "# startignore", styler.ignore_stop = "# xxx"),
      {
        style_text(c(
          "1+1",
          "# xxx",
          "1+1",
          "1+1",
          "# startignore"
        )) %>%
          as.character()
      }
    ),
    "Invalid stylerignore sequence"
  )
})


test_that("Simple example works", {
  expect_no_warning(test_collection("stylerignore", "simple",
    transformer = style_text
  ))
})

test_that("stylerignore does not need coincidence with top-level expressions", {
  expect_no_warning(test_collection("stylerignore", "crossing",
    transformer = style_text
  ))
})

test_that("token adding or removing works in stylerignore", {
  expect_no_warning(test_collection("stylerignore", "adding-removing",
    transformer = style_text
  ))
})

test_that("no token added or removed in complex case", {
  expect_no_warning(test_collection("stylerignore", "braces",
    transformer = style_text
  ))
})

test_that("stylerignore sequences are respected in alignment detection", {
  expect_no_warning(test_collection("stylerignore", "alignment",
    transformer = style_text
  ))
})
