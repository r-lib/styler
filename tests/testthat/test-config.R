test_that("can set path to local config", {
  expect_equal(
    set_path_cp_config_from(NULL),
    system.file("pre-commit-config.yaml", package = "precommit")
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
