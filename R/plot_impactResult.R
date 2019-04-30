# Copyright 2019 Robert Carnell

#' Plot impact Result Histogram
#'
#' @param x impactResult object
#' @param ... future parameters passed to internal functions
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
#' plot(test_result, main = "Test Result", xlab = "Test Statistic")
plot.impactResult <- function(x, ...)
{
  mybins <- max(min(nrow(x$bootstrap_results) * 20 / 100, 100), 10)
  if (ncol(x$bootstrap_results) > 1)
  {
    df <- data.frame(x = c(x$bootstrap_results),
                     nam = rep(names(x$result), each = nrow(x$bootstrap_results)))
    g <- ggplot(df, aes(x = x, group = nam)) +
      geom_histogram(bins = mybins) +
      facet_wrap(. ~ nam, scales = "free_x") +
      geom_vline(xintercept = 0, col = "red")
  } else {
    df <- data.frame(x = c(x$bootstrap_results))
    g <- ggplot(df, aes(x = x)) +
      geom_histogram(bins = mybins) +
      geom_vline(xintercept = 0, col = "red")
  }
  return(g)
}
