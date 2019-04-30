# Copyright 2019 Robert Carnell

context("test-test_response_change")

test_that("group, step change, diff method", {
  set.seed(1976)
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 12),
                    pre_post = rep(c("pre","post"), each = 6, times = 2),
                    id = c(rep(c("1","2","3"), times = 4), rep(c("4","5","6"), times = 4)),
                    time = rep(1:4, each = 3, times = 2),
                    val = rnorm(24, mean = 10, sd = 5))
  test_result <- test_response_change(dat, R = 100)
  expect_equal(length(test_result$result), 3)

  set.seed(1976)
  params2 <- params
  X1_pre <- createSample(params2$mu1_pre, params2$sigma1_pre, params2$N_pre)
  X2_pre <- createSample(params2$mu2_pre, params2$sigma2_pre, params2$N_pre)
  X1_post <- createSample(params2$mu1_post, params2$sigma1_post, params2$N_post)
  X2_post <- createSample(params2$mu2_post, params2$sigma2_post, params2$N_post)

  dat <- create_dat(params2, X1_pre, X2_pre, X1_post, X2_post)

  temp <- test_response_change(dat, R = 200)
  expect_gt(2 * (1 - temp$pvalue[1]), 0.05)
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "group")
  expect_lt(temp$result[1], 0.1)
  expect_lt(temp$bootstrap_mean[1], 0.1)
  expect_lt(temp$bootstrap_interval[1,1], 0)
  expect_gt(temp$bootstrap_interval[2,1], 0)
})

test_that("1-1, step change, diff method", {
  set.seed(1111)
  params2 <- params
  params2$m <- 10
  params2$mu2_pre <- rep(2, params2$m)
  params2$sigma2_pre <- rep(0.01, params2$m)
  params2$mu2_post <- rep(4, params2$m)
  params2$sigma2_post <- rep(0.01, params2$m)
  X1_pre <- createSample(params2$mu1_pre, params2$sigma1_pre, params2$N_pre)
  X2_pre <- createSample(params2$mu2_pre, params2$sigma2_pre, params2$N_pre)
  X1_post <- createSample(params2$mu1_post, params2$sigma1_post, params2$N_post)
  X2_post <- createSample(params2$mu2_post, params2$sigma2_post, params2$N_post)

  dat <- create_dat(params2, X1_pre, X2_pre, X1_post, X2_post)
  dat$matchid <- c(rep(as.character((params2$n + 1):(params2$n + params2$m)), times = params2$N_pre),
                   rep(NA, times = params2$n * params2$N_pre),
                   rep(as.character((params2$n + 1):(params2$n + params2$m)), times = params2$N_post),
                   rep(NA, times = params2$n * params2$N_post))

  # expect output from the test about Mixed Model Not Used
  expect_output(temp <- test_response_change(dat, type = "1-1", method = "diff", R = 100))
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "1-1")
  expect_lt(temp$result[1], 0.1)
  expect_lt(temp$bootstrap_mean[1], 0.1)
  expect_lt(temp$bootstrap_interval[1,1], 0)
  expect_gt(temp$bootstrap_interval[2,1], 0)
})
