# Copyright 2019 Robert Carnell

context("test-validate_impact_data")

test_that("Validate works", {
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                    pre_post = rep(c("pre","post"), each = 3, times = 2),
                    id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                    time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                    val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  expect_true(validate_impact_data(dat))

  dat2 <- dat
  dat2$id <- 1:12
  expect_error(validate_impact_data(dat2))

  dat3 <- dat
  dat3$trt <- 1:12
  expect_error(validate_impact_data(dat3))

  dat4 <- dat
  dat4$time <- "2019-01-01"
  expect_error(validate_impact_data(dat4))

  dat5 <- dat
  dat5$val <- "A"
  expect_error(validate_impact_data(dat5))

  dat6 <- dat
  names(dat6) <- LETTERS[1:5]
  expect_error(validate_impact_data(dat6))
})
