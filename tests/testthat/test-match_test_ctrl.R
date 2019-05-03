context("test-match_test_ctrl")

test_that("test .which.is.min", {
  expect_equal(.which.is.min(1:20), 1)
  expect_equal(.which.is.min(30:2), 29)
  expect_equal(.which.is.min(c(5:1,2:5)), 5)
  # random choice
  set.seed(1976)
  temp <- .which.is.min(c(5:1,1:5))
  expect_lte(temp, 6)
  expect_gte(temp, 5)
})

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

  expect_equal(m1$cases, 1:4)
  expect_equal(length(m1$controls), 4)
  expect_equal(m1$type, "1-1")
  expect_equal(m1$method, "diff")
  expect_equal(length(unique(m1$controls)), length(m1$controls))

  m2 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-1", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = TRUE,
    m = NA, group_tol = 0.01)

  expect_equal(m2$cases, 1:4)
  expect_equal(length(m2$controls), 4)
  expect_equal(m2$type, "1-1")
  expect_equal(m2$method, "diff")

  df <- data.frame(id = factor(LETTERS),
                   trt = factor(c(rep("test", 4), rep("ctrl", 22))),
                   metric1_time1 = c(0,5,10,15,1,6,11,16:34),
                   metric1_time2 = c(0,5,10,15,1,6,11,16:34),
                   metric1_time3 = c(0,5,10,15,1,6,11,16:34),
                   metric2_time1 = rep(1, 26),
                   metric2_time2 = rep(1, 26),
                   metric2_time3 = rep(1, 26))
  m3 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-1", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = TRUE,
    m = NA, group_tol = 0.01)

  expect_equal(m3$cases, 1:4)
  expect_equal(m3$controls, 5:8)
  expect_equal(m2$type, "1-1")
  expect_equal(m2$method, "diff")

  m4 <- match_test_ctrl(paste("trt ~", paste(
    paste("metric", rep(1:2, each = 3),
          "_time", rep(1:3, times = 2),
          sep = ""), collapse = " + ")),
    data = df,
    type = "1-1", method = "diff",
    test_name = "test", ctrl_name = "ctrl",
    replace = FALSE,
    m = NA, group_tol = 0.01)

  expect_equal(m4$cases, 1:4)
  expect_equal(m4$controls, 5:8)
  expect_equal(m4$type, "1-1")
  expect_equal(m4$method, "diff")
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

  expect_equal(m1$cases, 1:4)
  expect_equal(length(m1$controls), 4)
  expect_equal(length(m1$controls[[1]]), 3)
  expect_equal(m1$type, "1-m")
  expect_equal(m1$method, "diff")
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

  # this case returns the 1-1 results
  expect_equal(m1$cases, 1:4)
  expect_equal(length(m1$controls), 4)
  expect_equal(m1$type, "1-1")
  expect_equal(m1$method, "diff")
})
