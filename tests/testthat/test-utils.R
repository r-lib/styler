test_that("call capturing for pass", {
  expect_silent(captured <- call_and_capture("echo", "1"))
  expect_true(
    captured$exit_status == 0
  )
  expect_true(
    length(captured$stdout) == 1
  )

  expect_true(
    length(captured$stderr) == 0
  )
  expect_warning(
    communicate_captured_call(captured),
    "stdout"
  )
})

test_that("git repo status can be evaluated", {
  tmpdir <- local_test_setup(git = FALSE)
  expect_false(is_git_repo(root = tmpdir))
  git_init(tmpdir)
  expect_true(is_git_repo(root = tmpdir))
})

test_that("call capturing for error (command that does not exist)", {
  expect_silent(captured <- call_and_capture("j23lkjsdi", "1"))
  expect_true(
    captured$exit_status != 0
  )
  expect_true(
    length(captured$stdout) == 0
  )

  expect_true(
    length(captured$stderr) == 1
  )
  if (is_windows()) {
    expected <- "Could not recover stderr."
  } else {
    expected <- "not found"
  }
  expect_error(
    communicate_captured_call(captured),
    expected
  )
})

test_that("call capturing for error fo command that exists (but arguments that don't)", {
  expect_silent(captured <- call_and_capture("ls", "djdfjdoidj"))
  expect_true(
    captured$exit_status != 0
  )
  expect_true(
    length(captured$stdout) == 0
  )

  expect_true(
    length(captured$stderr) > 0
  )
  expect_error(
    communicate_captured_call(captured),
    "No such file or "
  )
})

test_that("inputs meet requirements", {
  expect_error(captured <- call_and_capture("echo", 1), "character vector.")
  expect_error(captured <- call_and_capture("echo", list("x")), "character vector.")
})
