test_that("assert passes when condition is TRUE", {
  expect_no_error(assert(TRUE))
  expect_no_error(assert(1 == 1))
})

test_that("assert throws error when condition is FALSE", {
  expect_error(assert(FALSE, "custom message"), "custom message")
  expect_error(assert(isFALSE(TRUE), "custom message"), "custom message")
})

test_that("assert includes call chain in error message", {
  expect_error(assert(FALSE, "msg"), "Call [0-9]+:")
})

test_that("assert accepts custom trace", {
  expect_error(assert(FALSE, "msg", trace = list()), "msg")
})