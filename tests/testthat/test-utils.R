test_that("call capturing for pass", {
  expect_silent(captured <- call_and_capture("echo", 1))
  expect_known_value(
    captured,
    test_path('reference-objects-captured-call-1')
  )
  expect_known_value(
    communicate_captured_call(captured),
    test_path('reference-objects-captured-call-2')
  )
})

test_that("call capturing for error", {
  expect_silent(captured <- call_and_capture("j23lkjsdi", 1))
  expect_known_value(
    captured,
    test_path('reference-objects-captured-call-3')
  )
  expect_error(
    communicate_captured_call(captured),
    "not found"
  )
})
