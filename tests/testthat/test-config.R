test_that("can set path to local config", {
  tmp <- tempdir()
  test.pkg <- fs::dir_create(tmp, "test.proj")
  expect_equal(
    set_path_cp_config_from(NULL, path_root = test.pkg),
    system.file("pre-commit-config-proj.yaml", package = "precommit")
  )
  expect_error(
    set_path_cp_config_from(tempfile()),
    "does not exist"
  )
})

test_that("can set path to remote config", {
  path <- set_path_cp_config_from(
    example_remote_config()
  )
  expect_equal(fs::path_ext(path), "yaml")
  expect_silent(yaml::read_yaml(path))

  expect_error(set_path_cp_config_from("https://apple.com"), "valid yaml")
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
    set_path_cp_config_from(NULL, path_root = test.pkg),
    "pkg\\.yaml"
  )
  fs::file_delete(fs::path(test.pkg, "DESCRIPTION"))
  expect_message(
    set_path_cp_config_from(NULL, path_root = test.pkg),
    "proj\\.yaml"
  )
})
