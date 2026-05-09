test_that("is_vector returns TRUE for atomic vectors", {
  expect_true(is_vector(1:5))
  expect_true(is_vector(c("a", "b", "c")))
  expect_true(is_vector(c(1.1, 2.2)))
  expect_true(is_vector(TRUE))
})

test_that("is_vector returns FALSE for lists", {
  expect_false(is_vector(list(1, 2, 3)))
  expect_false(is_vector(list(a = 1, b = 2)))
})

test_that("is_vector returns FALSE for non-vector types", {
  expect_false(is_vector(data.frame(x = 1)))
  expect_false(is_vector(matrix(1:4, 2, 2)))
})

test_that("check_vector passes for valid vectors", {
  expect_no_error(check_vector(1:5))
  expect_no_error(check_vector(c("a", "b")))
})

test_that("check_vector throws error for non-vectors", {
  expect_error(check_vector(list(1, 2)), "Object must be a vector!")
  expect_error(check_vector(data.frame(x = 1)), "Object must be a vector!")
})

test_that("check_vector accepts custom error message", {
  expect_error(check_vector(list(1), "must be atomic"), "must be atomic")
})