# Copyright 2019 Robert Carnell

#' Print Impact Result
#'
#' @param x impactResult object
#' @param ... future parameters
#'
#' @export
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
#' test_result
#' print(test_result)
print.impactResult <- function(x, ...)
{
  cat("Impact Analysis Result\n")
  if (length(x$result) > 1)
  {
    cat("  Effect Names: ", names(x$result), "\n")
  }
  cat("  Effect: ", x$result, "\n")
  cat("  Bootstrap Mean: ", x$bootstrap_mean, "\n")
  if (length(x$result) > 1)
  {
    cat("  Confidence Interval:", x$bootstrap_interval[1,], "\n")
    cat("                      ", x$bootstrap_interval[2,], "\n")
  } else {
    cat("  Confidence Interval:", x$bootstrap_interval, "\n")
  }
  cat("  Method: ", x$method, "\n")
  cat("  Type: ", x$type, "\n\n")
}
