test_that("color_map maps colors to groups", {
  colors <- c("red", "blue")
  groups <- c("A", "B", "A", "C")

  result <- color_map(colors, groups)
  expect_equal(unname(result["A"]), "red")
  expect_equal(unname(result["B"]), "blue")
  expect_equal(unname(result["C"]), "red")
})

test_that("color_map extends colors when groups exceed color count", {
  colors <- c("red")
  groups <- c("A", "B", "C")

  result <- color_map(colors, groups)
  expect_equal(unname(result), c("red", "red", "red"))
  expect_equal(names(result), c("A", "B", "C"))
})

test_that("color_map accepts various color formats", {
  expect_no_error(color_map(c("#FF0000", "#00FF00"), c("A", "B")))
  expect_no_error(color_map(1:3, c("A", "B", "C")))
})