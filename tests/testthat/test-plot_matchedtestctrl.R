# Copyright 2019 Robert Carnell

context("test-plot_matchedtestctrl")

test_that("plot types work", {
  df <- data.frame(id = factor(LETTERS),
                   trt = factor(c(rep("test", 4), rep("ctrl", 22))),
                   metric1_time1 = c(0,5,10,15,1,6,11,16:34),
                   metric1_time2 = c(0,5,10,15,1,6,11,16:34),
                   metric1_time3 = c(0,5,10,15,1,6,11,16:34),
                   metric2_time1 = rnorm(26),
                   metric2_time2 = rnorm(26),
                   metric2_time3 = rnorm(26))

  # 1-1
  m3 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-1", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = TRUE,
    m = NA, group_tol = 0.01)

  expect_silent(plot(m3))

  # 1-m
  m3 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-m", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = 3, group_tol = 0.01)

  expect_silent(plot(m3))

  m3 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "group", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = 3, group_tol = 0.9)

  expect_silent(plot(m3))

  m3 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "group", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = 3, group_tol = 0.01) # will return a 1-1 object

  expect_silent(plot(m3))
})
