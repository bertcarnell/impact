# Copyright 2019 Robert Carnell

#' Validate the input data for the impact package
#'
#' @param dat an input data.frame
#'
#' @return TRUE if the data.frame is valid or an exception otherwise
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' validate_impact_data(dat)
validate_impact_data <- function(dat)
{
  assertthat::assert_that(all(c("pre_post", "time", "id", "trt") %in% names(dat)),
                          msg = "The dataframe, dat, must include columns named time, id, and trt")
  assertthat::assert_that(is.factor(dat$pre_post) & length(levels(dat$pre_post)) == 2,
                          msg = "The pre_post variable must be a factor with two levels")
  assertthat::assert_that(is.factor(dat$id),
                          msg = "The id variable must be a factor")
  assertthat::assert_that(is.factor(dat$trt) & length(levels(dat$trt)) == 2,
                          msg = "The trt variable must be a factor with at least two levels")
  assertthat::assert_that(is.factor(dat$time) | is.numeric(dat$time) |
                            "Date" %in% class(dat$time) |
                            "POSIXt" %in% class(dat$time),
                          msg = "The time variable must be a factor, numeric, Date, or POSIXt")
  assertthat::assert_that(is.numeric(dat$val),
                          msg = "The val variable must be numeric")
  return(TRUE)
}
