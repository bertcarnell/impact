context("test-plot_impactresult")

test_that("plot impact result works", {
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                   pre_post = rep(c("pre","post"), each = 3, times = 2),
                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
  expect_silent(plot(test_result, main = "Test Result", xlab = "Test Statistic"))

  test_result <- test_level_shift(dat, type = "group", method = "ratio", R = 100)
  expect_silent(plot(test_result, main = "Test Result", xlab = "Test Statistic"))

  # test are in the matchid
  dat$matchid <- factor(c(1,2,3,1,2,3,1,2,3,1,2,3))
  test_result <- test_level_shift(dat, type = "1-1", method = "diff", R = 100)
  expect_silent(plot(test_result, main = "Test Result", xlab = "Test Statistic"))

  test_result <- test_level_shift(dat, type = "1-1", method = "ratio", R = 100)
  expect_silent(plot(test_result, main = "Test Result", xlab = "Test Statistic"))
})
