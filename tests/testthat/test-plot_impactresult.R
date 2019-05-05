# Copyright 2019 Robert Carnell

context("test-plot_impactresult")

test_that("plot impact result works", {
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                   pre_post = rep(c("pre","post"), each = 3, times = 2),
                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))

  test_result <- test_level_shift(dat, type = "group", method = "ratio", R = 100)
  expect_silent(plot(test_result) + xlab("Test Statistic") + ggtitle("Test Result"))

  # test are in the matchid
  dat$matchid <- factor(c(1,2,3,1,2,3,1,2,3,1,2,3))
  test_result <- test_level_shift(dat, type = "1-1", method = "diff", R = 100)
  expect_silent(plot(test_result) + xlab("Test Statistic") + ggtitle("Test Result"))

  test_result <- test_level_shift(dat, type = "1-1", method = "ratio", R = 100)
  expect_silent(plot(test_result) + xlab("Test Statistic") + ggtitle("Test Result"))
})

test_that("plot impact result works with an impact result from test response change", {
  set.seed(1999)
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 12),
                    pre_post = rep(c("pre","post"), each = 6, times = 2),
                    id = c(rep(c("1","2","3"), times = 4), rep(c("4","5","6"), times = 4)),
                    time = rep(1:4, each = 3, times = 2),
                    val = pmax(rnorm(24, mean = 10, sd = 5), 0.01))
  # expect output "Mixed model not used"
  expect_output(test_result <- test_response_change(dat, R = 100))
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))
  # TODO:  Tests are printed in the wrong order left to right
  #   it is because of the alphabetical order of the factor labels

  expect_output(test_result <- test_response_change(dat, R = 100, type = "group", method = "diff"))
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "group", method = "ratio"))
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))

  dat$matchid <- factor(rep(c("1","2","3"), times = 8), levels = c("1","2","3","4","5","6"))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "1-1", method = "diff"))
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "1-1", method = "ratio"))
  g <- plot(test_result)
  expect_silent(g + xlab("Test Statistic") + ggtitle("Test Result"))
})

