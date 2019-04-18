# Copyright 2019 Robert Carnell

#' Plot impact Result Histogram
#'
#' @param x impactResult object
#' @param ... parameters passed to stats::hist
#'
#' @export
#'
#' @importFrom graphics hist abline
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
#' plot(test_result)
plot.impactResult <- function(x, ...)
{
  hist(x$bootstrap_results, ...)
  abline(v = 0, col = "red", lwd = 2)
}
