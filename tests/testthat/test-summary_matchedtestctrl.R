context("test-summary_matchedtestctrl")

test_that("1-1 type, diff method", {
  set.seed(123)
  df <- data.frame(id = factor(LETTERS),
                   trt = factor(c(rep("test", 4), rep("ctrl", 22))),
                   metric1_time1 = rnorm(26),
                   metric1_time2 = rnorm(26),
                   metric1_time3 = rnorm(26),
                   metric2_time1 = rnorm(26),
                   metric2_time2 = rnorm(26),
                   metric2_time3 = rnorm(26))
  m1 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-1", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = NA, group_tol = 0.01)
  expect_output(summary(m1))
})

test_that("1-m type, diff method", {
  set.seed(222)
  df <- data.frame(id = factor(LETTERS),
                   trt = factor(c(rep("test", 4), rep("ctrl", 22))),
                   metric1_time1 = rnorm(26),
                   metric1_time2 = rnorm(26),
                   metric1_time3 = rnorm(26),
                   metric2_time1 = rnorm(26),
                   metric2_time2 = rnorm(26),
                   metric2_time3 = rnorm(26))
  m1 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-m", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = 3, group_tol = 0.01)
  expect_output(summary(m1))
})

test_that("group type, diff method", {
  set.seed(4245)
  df <- data.frame(id = factor(LETTERS),
                   trt = factor(c(rep("test", 4), rep("ctrl", 22))),
                   metric1_time1 = rnorm(26),
                   metric1_time2 = rnorm(26),
                   metric1_time3 = rnorm(26),
                   metric2_time1 = rnorm(26),
                   metric2_time2 = rnorm(26),
                   metric2_time3 = rnorm(26))
  m1 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "group", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = 3, group_tol = 0.01)
  expect_output(summary(m1))
})
