test_that("Path can be derived for windows Python >= 3.0", {
  paths_base <- with_mock(
    "precommit::path_derive_precommit_exec_win_python3plus_candidates" = function() {
      c(
        fs::path_home("AppData/Roaming/Python/Python35"),
        fs::path_home("AppData/Roaming/Python/Python37")
      )
    },
    path_derive_precommit_exec_win_python3plus_base()
  )

  expect_equal(
    paths_base,
    c(
      fs::path(fs::path_home(), "AppData/Roaming/Python/Python37/Scripts"),
      fs::path(fs::path_home(), "AppData/Roaming/Python/Python35/Scripts")
    )
  )
  skip_if(!is_windows())
  skip_if(!not_conda())
  skip_if(on_cran())
  expect_match(path_derive_precommit_exec_win_python3plus_base(), "AppData/Roaming")
  expect_equal(
    fs::path_file(path_derive_precommit_exec_win()),
    precommit_executable_file()
  )
})


test_that("Warns when there are multiple installations found (2x os)", {
  expect_warning(
    with_mock(
      "precommit::path_derive_precommit_exec_path" = function(candidate) {
        fs::path_home("AppData/Roaming/Python/Python35")
      },
      "Sys.info" = function(...) {
        c(sysname = "windows")
      },
      "precommit:::path_derive_precommit_exec_win" = function() {
        c(
          fs::path_home("AppData/Roaming/Python/Python34"),
          fs::path_home("AppData/Roaming/Python/Python37")
        )
      },
      path_derive_precommit_exec()
    ),
    "We detected multiple pre-commit executables"
  )
})

test_that("Warns when there are multiple installations found (2x path)", {
  expect_warning(
    with_mock(
      "precommit::path_derive_precommit_exec_path" = function(candidate) {
        c(
          fs::path_home("AppData/Roaming/Python/Python35"),
          fs::path_home("AppData/Roaming/Python/Python37")
        )
      },
      "Sys.info" = function(...) {
        c(sysname = "windows")
      },
      "precommit:::path_derive_precommit_exec_win" = function() {
        fs::path_home("AppData/Roaming/Python/Python34")
      },
      path_derive_precommit_exec()
    ),
    "We detected multiple pre-commit executables"
  )
})

test_that("Warns when there are multiple installations found (path and os)", {
  expect_warning(
    with_mock(
      "precommit::path_derive_precommit_exec_path" = function(candidate) {
        fs::path_home("AppData/Roaming/Python/Python35")
      },
      "Sys.info" = function(...) {
        c(sysname = "windows")
      },
      "precommit:::path_derive_precommit_exec_win" = function() {
        fs::path_home("AppData/Roaming/Python/Python34")
      },
      path_derive_precommit_exec()
    ),
    "We detected multiple pre-commit executables"
  )
})
