# Copyright 2019 Robert Carnell

context("test-plot_impact_data_step_change")

test_that("plot is generated", {
  dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                   pre_post = rep(c("pre","post"), each = 3, times = 2),
                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  expect_silent(plot_impact_data_step_change(dat))
  expect_silent(plot_impact_data_step_change(dat, method = "diff", type = "raw"))
  expect_silent(plot_impact_data_step_change(dat, method = "diff", type = "corrected"))
  expect_silent(plot_impact_data_step_change(dat, method = "ratio", type = "raw"))
  expect_silent(plot_impact_data_step_change(dat, method = "ratio", type = "corrected"))

  dat2 <- data.frame(trt = rep(c("test","ctrl"), each = 6),
                    pre_post = rep(c("pre","post"), each = 3, times = 2),
                    id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
                    time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
                    height = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
  expect_silent(plot_impact_data_step_change(dat2, val_name = "height"))
  expect_silent(plot_impact_data_step_change(dat2, method = "diff", type = "raw", val_name = "height"))
  expect_silent(plot_impact_data_step_change(dat2, method = "diff", type = "corrected", val_name = "height"))
  expect_silent(plot_impact_data_step_change(dat2, method = "ratio", type = "raw", val_name = "height"))
  expect_silent(plot_impact_data_step_change(dat2, method = "ratio", type = "corrected", val_name = "height"))
})
