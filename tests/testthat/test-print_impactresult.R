# Copyright 2019 Robert Carnell

context("test-print_impactresult")

test_that("print is called for an impact result", {
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                   pre_post = rep(c("pre","post"), each = 3, times = 2),
                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
  expect_output(print(test_result), regexp = "^Impact Analysis Result")
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
  expect_output(print(test_result))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "group", method = "diff"))
  expect_output(print(test_result))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "group", method = "ratio"))
  expect_output(print(test_result))

  dat$matchid <- factor(rep(c("1","2","3"), times = 8), levels = c("1","2","3","4","5","6"))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "1-1", method = "diff"))
  expect_output(print(test_result))

  expect_output(test_result <- test_response_change(dat, R = 100, type = "1-1", method = "ratio"))
  expect_output(print(test_result))
})
