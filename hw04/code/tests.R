context("Testing for remove_missing")

x <- c(25, 51, NA, 88, 19, NA, 7, 74)

test_that("NA is removed correctly", {
  expect_equal(remove_missing(x), c(25, 51, 88, 19, 7, 74))
})

context("Testing for get_minimum")

test_that("Minimum value is identified", {
  expect_equal(get_minimum(x, na.rm = TRUE), min(x, na.rm = TRUE))
  expect_equal(get_minimum(x, na.rm = FALSE), min(x))
  expect_equal(2 * (get_minimum(x, na.rm = TRUE)), (2 * min(x, na.rm = TRUE)))
  expect_equal(2 * (get_minimum(x, na.rm = FALSE)), (2 * min(x)))
})