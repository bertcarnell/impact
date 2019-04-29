context("test-test_response_change")

test_that("test group type works", {
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

  temp <- test_response_change(dat, R = 1000)
  expect_gt(2 * (1 - temp$pvalue[1]), 0.05)
})

