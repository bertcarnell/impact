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
#' a <- 4
plot.impactResult <- function(x, ...)
{
  hist(x$bootstrap_results, ...)
  abline(v = 0, col = "red", lwd = 2)
}
