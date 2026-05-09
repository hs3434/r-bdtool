test_that("cm2inch converts centimeters to inches", {
  expect_equal(cm2inch(2.54), 1)
  expect_equal(cm2inch(5.08), 2)
  expect_equal(cm2inch(0), 0)
})

test_that("cm2inch handles vectorized input", {
  expect_equal(cm2inch(c(2.54, 5.08)), c(1, 2))
})