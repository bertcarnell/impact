# Copyright 2019 Robert Carnell

context("test-test_level_shift")

test_that("group means, step change, diff method", {
  set.seed(1976)
  params2 <- params
  X1_pre <- createSample(params2$mu1_pre, params2$sigma1_pre, params2$N_pre)
  X2_pre <- createSample(params2$mu2_pre, params2$sigma2_pre, params2$N_pre)
  X1_post <- createSample(params2$mu1_post, params2$sigma1_post, params2$N_post)
  X2_post <- createSample(params2$mu2_post, params2$sigma2_post, params2$N_post)

  dat <- create_dat(params2, X1_pre, X2_pre, X1_post, X2_post)

  temp <- test_level_shift(dat, R = 100)
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "group")
  expect_lt(temp$result, 0.1)
  expect_lt(temp$bootstrap_mean, 0.1)
  expect_lt(temp$bootstrap_interval[1], 0)
  expect_gt(temp$bootstrap_interval[2], 0)
  # plot(temp)
  # plot_impact_data_step_change(dat, type = "raw")
  # plot_impact_data_step_change(dat, type = "corrected")

  # single time post event
  set.seed(1010)
  params3 <- params
  params3$N_post <- 1

  X1_pre <- createSample(params3$mu1_pre, params3$sigma1_pre, params3$N_pre)
  X2_pre <- createSample(params3$mu2_pre, params3$sigma2_pre, params3$N_pre)
  X1_post <- createSample(params3$mu1_post, params3$sigma1_post, params3$N_post)
  X2_post <- createSample(params3$mu2_post, params3$sigma2_post, params3$N_post)

  dat <- create_dat(params3, X1_pre, X2_pre, X1_post, X2_post)

  temp <- test_level_shift(dat, R = 100)
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "group")
  expect_lt(temp$result, 0.1)
  expect_lt(temp$bootstrap_mean, 0.1)
  expect_lt(temp$bootstrap_interval[1], 0)
  expect_gt(temp$bootstrap_interval[2], 0)
})

test_that("group means, step change, ratio method", {
  set.seed(1976)
  params2 <- params
  params2$mu1_pre <- rep(2, params2$n)
  params2$mu2_pre <- rep(4, params2$m)
  params2$mu1_post <- rep(4, params2$n)
  params2$mu2_post <- rep(8, params2$m)
  X1_pre <- createSample(params2$mu1_pre, params2$sigma1_pre, params2$N_pre)
  X2_pre <- createSample(params2$mu2_pre, params2$sigma2_pre, params2$N_pre)
  X1_post <- createSample(params2$mu1_post, params2$sigma1_post, params2$N_post)
  X2_post <- createSample(params2$mu2_post, params2$sigma2_post, params2$N_post)

  dat <- create_dat(params2, X1_pre, X2_pre, X1_post, X2_post)
  temp2 <- test_level_shift(dat, method = "ratio", R = 100)
  expect_equal(temp2$method, "ratio")
  expect_equal(temp2$type, "group")
  expect_lt(temp2$result, 0.1)
  expect_lt(temp2$bootstrap_mean, 0.1)
  expect_lt(temp2$bootstrap_interval[1], 0)
  expect_gt(temp2$bootstrap_interval[2], 0)

  # plot(temp2)
  # plot_impact_data_step_change(dat, type = "raw", method = "ratio")
  # plot_impact_data_step_change(dat, type = "corrected", method = "ratio")

  set.seed(19760)
  params3 <- params2
  params3$N_post <- 1
  X1_pre <- createSample(params3$mu1_pre, params3$sigma1_pre, params3$N_pre)
  X2_pre <- createSample(params3$mu2_pre, params3$sigma2_pre, params3$N_pre)
  X1_post <- createSample(params3$mu1_post, params3$sigma1_post, params3$N_post)
  X2_post <- createSample(params3$mu2_post, params3$sigma2_post, params3$N_post)

  dat <- create_dat(params3, X1_pre, X2_pre, X1_post, X2_post)
  temp2 <- test_level_shift(dat, method = "ratio", R = 100)
  expect_equal(temp2$method, "ratio")
  expect_equal(temp2$type, "group")
  expect_lt(temp2$result, 0.1)
  expect_lt(temp2$bootstrap_mean, 0.1)
  expect_lt(temp2$bootstrap_interval[1], 0)
  expect_gt(temp2$bootstrap_interval[2], 0)
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

  temp <- test_level_shift(dat, type = "1-1", method = "diff", R = 100)
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "1-1")
  expect_lt(temp$result, 0.1)
  expect_lt(temp$bootstrap_mean, 0.1)
  expect_lt(temp$bootstrap_interval[1], 0)
  expect_gt(temp$bootstrap_interval[2], 0)

  #plot(temp, main = "", xlab = "Difference in Test vs Ctrl")

  set.seed(11112)
  params3 <- params2
  params3$N_post <- 1
  X1_pre <- createSample(params3$mu1_pre, params3$sigma1_pre, params3$N_pre)
  X2_pre <- createSample(params3$mu2_pre, params3$sigma2_pre, params3$N_pre)
  X1_post <- createSample(params3$mu1_post, params3$sigma1_post, params3$N_post)
  X2_post <- createSample(params3$mu2_post, params3$sigma2_post, params3$N_post)

  dat <- create_dat(params3, X1_pre, X2_pre, X1_post, X2_post)
  dat$matchid <- c(rep(as.character((params3$n + 1):(params3$n + params3$m)), times = params3$N_pre),
                   rep(NA, times = params3$n * params3$N_pre),
                   rep(as.character((params3$n + 1):(params3$n + params3$m)), times = params3$N_post),
                   rep(NA, times = params3$n * params3$N_post))

  temp <- test_level_shift(dat, type = "1-1", method = "diff", R = 100)
  expect_equal(temp$method, "diff")
  expect_equal(temp$type, "1-1")
  expect_lt(temp$result, 0.1)
  expect_lt(temp$bootstrap_mean, 0.1)
  expect_lt(temp$bootstrap_interval[1], 0)
  expect_gt(temp$bootstrap_interval[2], 0)
})

test_that("1-1, step change, ratio method", {
  set.seed(1111)
  params2 <- params
  params2$m <- 10
  params2$mu1_pre <- rep(2, params2$n)
  params2$mu2_pre <- rep(4, params2$m)
  params2$mu1_post <- rep(4, params2$n)
  params2$mu2_post <- rep(8, params2$m)
  params2$sigma2_pre <- rep(0.01, params2$m)
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

  temp2 <- test_level_shift(dat, type = "1-1", method = "ratio", R = 100)
  expect_equal(temp2$method, "ratio")
  expect_equal(temp2$type, "1-1")
  expect_lt(temp2$result, 0.1)
  expect_lt(temp2$bootstrap_mean, 0.1)
  expect_lt(temp2$bootstrap_interval[1], 0)
  expect_gt(temp2$bootstrap_interval[2], 0)

  #plot(temp2, main = "", xlab = "Difference in Ratio of Test vs Ctrl")

  set.seed(1122)
  params3 <- params2
  params3$N_post <- 1
  X1_pre <- createSample(params3$mu1_pre, params3$sigma1_pre, params3$N_pre)
  X2_pre <- createSample(params3$mu2_pre, params3$sigma2_pre, params3$N_pre)
  X1_post <- createSample(params3$mu1_post, params3$sigma1_post, params3$N_post)
  X2_post <- createSample(params3$mu2_post, params3$sigma2_post, params3$N_post)

  dat <- create_dat(params3, X1_pre, X2_pre, X1_post, X2_post)
  dat$matchid <- c(rep(as.character((params3$n + 1):(params3$n + params3$m)), times = params3$N_pre),
                   rep(NA, times = params3$n * params3$N_pre),
                   rep(as.character((params3$n + 1):(params3$n + params3$m)), times = params3$N_post),
                   rep(NA, times = params3$n * params3$N_post))

  temp2 <- test_level_shift(dat, type = "1-1", method = "ratio", R = 100)
  expect_equal(temp2$method, "ratio")
  expect_equal(temp2$type, "1-1")
  expect_lt(temp2$result, 0.1)
  expect_lt(temp2$bootstrap_mean, 0.1)
  expect_lt(temp2$bootstrap_interval[1], 0)
  expect_gt(temp2$bootstrap_interval[2], 0)
})
