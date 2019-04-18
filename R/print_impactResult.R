# Copyright 2019 Robert Carnell

#' Print Impact Result
#'
#' @param x impactResult object
#' @param ... future parameters
#'
#' @export
#'
#' @examples
#' a <- 4
print.impactResult <- function(x, ...)
{
  cat("Impact Analysis Result\n")
  cat("  Effect: ", x$result, "\n")
  cat("  Bootstrap Mean: ", x$bootstrap_mean, "\n")
  cat("  Confidence Interval:", x$bootstrap_interval, "\n")
  cat("  Method: ", x$method, "\n")
  cat("  Type: ", x$type, "\n\n")
}
