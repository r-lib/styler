test_that("call capturing for pass", {
  expect_silent(captured <- call_and_capture("echo", 1))
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

test_that("call capturing for error (command that does not exist)", {
  expect_silent(captured <- call_and_capture("j23lkjsdi", 1))
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
  expect_silent(captured <- call_and_capture(path_pre_commit_exec(), "hdif dijf"))
  expect_true(
    captured$exit_status != 0
  )
  expect_true(
    length(captured$stdout) == 0
  )

  expect_true(
    length(captured$stderr) > 1
  )
  expect_error(
    communicate_captured_call(captured),
    "invalid choice"
  )
})
