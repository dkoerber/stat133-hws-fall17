context("Testing for remove_missing")

x <- c(25, 51, NA, 88, 19, NA, 7, 74)

test_that("NA is removed correctly", {
  expect_equivalent(remove_missing(x), c(25, 51, 88, 19, 7, 74))
  expect_equivalent(remove_missing(c(NA, NA)), logical(0))
  expect_equivalent(remove_missing(x)[-1], c(51, 88, 19, 7, 74))
  expect_equivalent(remove_missing(c(NA, NA))[-1], logical(0))
})

context("Testing for get_minimum")
test_that("Minimum value is properly identified", {
  expect_equal(get_minimum(x, na.rm = TRUE), min(x, na.rm = TRUE))
  expect_equal(get_minimum(x[-7], na.rm = TRUE), 19)
  expect_equal(2 * (get_minimum(x, na.rm = TRUE)), 2 * min(x, na.rm = TRUE))
  expect_error(2 * (get_minimum(x, na.rm = FALSE)), NA)
})

context("Testing for get_maximum")
test_that("Maximum value is properly identified", {
  expect_equal(get_maximum(x, na.rm = TRUE), max(x, na.rm = TRUE))
  expect_equal(get_maximum(x[-4], na.rm = TRUE), 74)
  expect_equal(2 * (get_maximum(x, na.rm = TRUE)), 2 * max(x, na.rm = TRUE))
  expect_error(2 * (get_maximum(x, na.rm = FALSE)), NA)
})

context("Testing for get_range")
test_that("Range is properly identified", {
  expect_equal(get_range(x, na.rm = TRUE), 81)
  expect_equal(get_range(x[-4], na.rm = TRUE), 67)
  expect_equal(2 * (get_range(x, na.rm = TRUE)), 162)
  expect_error(2 * (get_range(x, na.rm = FALSE)), NA)
})

context("Testing for get_percentile10")
test_that("10th percentile is properly identified", {
  expect_equal(get_percentile10(x, na.rm = TRUE), 13)
  expect_equal(get_percentile10(x[-4], na.rm = TRUE), 11.8)
  expect_equal(2 * (get_percentile10(x, na.rm = TRUE)), 26)
  expect_error((get_percentile10(x, na.rm = FALSE)), NA)
})

context("Testing for get_percentile90")
test_that("90th percentile is properly identified", {
  expect_equal(get_percentile90(x, na.rm = TRUE), 81)
  expect_equal(get_percentile90(x[-4], na.rm = TRUE), 64.8)
  expect_equal(2 * (get_percentile90(x, na.rm = TRUE)), 162)
  expect_error((get_percentile90(x, na.rm = FALSE)), NA)
})

context("Testing for get_quartile1")
test_that("1st quartile is properly identified", {
  expect_equal(get_quartile1(x, na.rm = TRUE), 20.5)
  expect_equal(get_quartile1(x[-4], na.rm = TRUE), 19)
  expect_equal(2 * (get_quartile1(x, na.rm = TRUE)), 41)
  expect_error((get_quartile1(x, na.rm = FALSE)), NA)
})

context("Testing for get_quartile3")
test_that("3rd quartile is properly identified", {
  expect_equal(get_quartile3(x, na.rm = TRUE), 68.25)
  expect_equal(get_quartile3(x[-4], na.rm = TRUE), 51)
  expect_equal(2 * (get_quartile3(x, na.rm = TRUE)), 136.5)
  expect_error((get_quartile3(x, na.rm = FALSE)), NA)
})

context("Testing for get_median")
test_that("Median is properly identified", {
  expect_equal(get_median(x, na.rm = TRUE), 38)
  expect_equal(get_median(x[-4], na.rm = TRUE), 25)
  expect_equal(2 * (get_median(x, na.rm = TRUE)), 76)
  expect_error((get_median(x, na.rm = FALSE)), NA)
})

context("Testing for get_average")
test_that("Average is properly identified", {
  expect_equal(get_average(x, na.rm = TRUE), 44)
  expect_equal(get_average(x[-4], na.rm = TRUE), 35.2)
  expect_equal(2 * (get_average(x, na.rm = TRUE)), 88)
  expect_error((get_average(x, na.rm = FALSE)), NA)
})

context("Testing for get_stdev")
test_that("Standard deviation is properly identified", {
  expect_equal(get_stdev(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  expect_equal(get_stdev(x[-4], na.rm = TRUE), sd(x[-4], na.rm = TRUE))
  expect_equal(2 * (get_stdev(x, na.rm = TRUE)), 2 * sd(x, na.rm = TRUE))
  expect_error((get_stdev(x, na.rm = FALSE)), NA)
})

context("Testing for count_missing")
test_that("Number of missing values is properly identified", {
  expect_equal(count_missing(x), 2)
  expect_equal(count_missing(x[-3]), 1)
  expect_equal(2 * (count_missing(x)), 4)
  expect_equal((count_missing(x[-3][-5])), 0)
})

context("Testing for summary_stats")
test_that("Summary stats are properly calculated", {
  expect_equal(summary_stats(x)[[1]], min(x, na.rm = TRUE))
  expect_equal(summary_stats(x)[[2]], quantile(x, 0.1, na.rm = TRUE)[[1]])
  expect_equal(summary_stats(x)[[3]], quantile(x, 0.25, na.rm = TRUE)[[1]])
  expect_equal(summary_stats(x)[[4]], median(x, na.rm = TRUE))
})

context("Testing for drop_lowest")
test_that("Lowest score is properly dropped", {
  expect_equal(drop_lowest(x), c(25, 51, NA, 88, 19, NA, 74))
  expect_equal(2 * drop_lowest(x), c(50, 102, NA, 176, 38, NA, 148))
  expect_equal(drop_lowest(x[-3][-5]), c(25, 51, 88, 19, 74))
  expect_equal(2 * drop_lowest(x[-3][-5]), c(50, 102, 176, 38, 148))
})

context("Testing for rescale100")
test_that("Data is properly rescaled to 100", {
  expect_equal(rescale100(x, xmin = 0, xmax = 90)[1], 27.777778)
  expect_equal(rescale100(x, xmin = 0, xmax = 100)[1], 25)
  expect_equal(rescale100(x, xmin = 10, xmax = 90)[1], 18.75)
  expect_equal(rescale100(x, xmin = 0, xmax = 80)[4], 110)
})

x <- x[-3][-5]

context("Testing for score_homework")
test_that("Homework is properly scored", {
  expect_equal(score_homework(x), 44)
  expect_equal(score_homework(x, drop = TRUE), 51.4)
  expect_equal(score_homework(x[-6]), 38)
  expect_equal(score_homework(x[-6], drop = TRUE), 45.75)
})

context("Testing for score_quiz")
test_that("Quizzes are properly scored", {
  expect_equal(score_quiz(x), 44)
  expect_equal(score_quiz(x, drop = TRUE), 51.4)
  expect_equal(score_quiz(x[-6]), 38)
  expect_equal(score_quiz(x[-6], drop = TRUE), 45.75)
})

context("Testing for score_lab")
test_that("Lab attendance is properly scored", {
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(11), 100)
  expect_equal(score_lab(2), 0)
  expect_equal(score_lab(7), 20)
})