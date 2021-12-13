test_that("can set path to local config", {
  tmp <- tempdir()
  test.pkg <- fs::dir_create(tmp, "test.proj")
  expect_equal(
    set_config_source(NULL, root = test.pkg),
    system.file("pre-commit-config-proj.yaml", package = "precommit")
  )
  expect_error(
    set_config_source(tempfile()),
    "does not exist"
  )
})

test_that("can set path to remote config", {
  skip_on_cran()
  path <- set_config_source(
    example_remote_config()
  )
  expect_equal(fs::path_ext(path), "yaml")
  expect_silent(yaml::read_yaml(path))

  expect_error(set_config_source("https://apple.com"), "valid yaml")
})

test_that("defaults to right config depending on whether or not root is a pkg", {
  tmp <- tempdir()
  test.pkg <- fs::dir_create(tmp, "test.pkg")
  withr::with_dir(test.pkg, {
    desc <- desc::description$new("!new")
    desc$set(Package = "test.pkg")
    desc$write("DESCRIPTION")
  })
  expect_message(
    set_config_source(NULL, root = test.pkg),
    "pkg\\.yaml"
  )
  fs::file_delete(fs::path(test.pkg, "DESCRIPTION"))
  expect_message(
    set_config_source(NULL, root = test.pkg),
    "proj\\.yaml"
  )
})

test_that(".Rbuildignore is written to the right directory when root is relative", {
  root <- tempfile()
  fs::dir_create(root)

  withr::with_dir(root, {
    desc <- desc::description$new("!new")
    desc$set(Package = "test.pkg")
    desc$write("DESCRIPTION")
  })
  withr::with_dir(
    fs::path_dir(root),
    use_precommit_config(root = fs::path_file(root))
  )
  expect_true(file_exists(fs::path(root, ".Rbuildignore")))
})

test_that(".Rbuildignore is written to the right directory when root is absolute", {
  root <- tempfile()
  fs::dir_create(root)

  withr::with_dir(root, {
    desc <- desc::description$new("!new")
    desc$set(Package = "test.pkg")
    desc$write("DESCRIPTION")
  })
  use_precommit_config(root = root)
  expect_true(file_exists(fs::path(root, ".Rbuildignore")))
})
