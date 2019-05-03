# Copyright 2019 Robert Carnell

#' Plot a matchedTestCtrl object using Principal components
#'
#' @param x a matchedTestCtrl object
#' @param ... other parameters passed to internal methods (future use)
#'
#' @return a ggplot object
#' @export
#'
#' @import ggplot2
#' @importFrom stats prcomp
#'
#' @examples
#' df <- data.frame(id = factor(LETTERS),
#'                  trt = factor(c(rep("test", 4), rep("ctrl", 22))),
#'                  metric1_time1 = c(0,5,10,15,1,6,11,16:34),
#'                  metric1_time2 = c(0,5,10,15,1,6,11,16:34),
#'                  metric1_time3 = c(0,5,10,15,1,6,11,16:34),
#'                  metric2_time1 = rnorm(26),
#'                  metric2_time2 = rnorm(26),
#'                  metric2_time3 = rnorm(26))
#'
#' m3 <- match_test_ctrl(paste("trt ~", paste(
#'   paste("metric", rep(1:2, each = 3),
#'         "_time", rep(1:3, times = 2),
#'         sep = ""), collapse = " + ")),
#'   data = df,
#'   type = "1-1", method = "diff",
#'   test_name = "test", ctrl_name = "ctrl",
#'   replace = TRUE,
#'   m = NA, group_tol = 0.01)
#'
#' plot(m3)
plot.matchedTestCtrl <- function(x, ...)
{
  # find the principle components of the data minus the first column of trt-ctrl
  temp_prcomp <- stats::prcomp(x$data[,-1], scale = TRUE)
  temp_df <- as.data.frame(predict(temp_prcomp))

  temp_df$unit <- "unmatched"
  temp_df$unit[x$cases] <- "trt"
  if (x$type == "1-m")
  {
    temp_df$unit[unlist(x$controls)] <- "ctrl"
  } else
  {
    temp_df$unit[x$controls] <- "ctrl"
  }
  temp_df$unit <- factor(temp_df$unit)

  if (x$type == "1-1")
  {
    temp_lines <- data.frame(
      x = temp_df$PC1[x$cases],
      y = temp_df$PC2[x$cases],
      xend = temp_df$PC1[x$controls],
      yend = temp_df$PC2[x$controls]
    )

    g <- ggplot(temp_df, aes(x = PC1, y = PC2, col = unit)) +
           geom_point() +
           geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                        data = temp_lines,
                        col = "black", lty = 2)
  } else if (x$type == "1-m")
  {
    temp_lines <- data.frame(
      x = temp_df$PC1[rep(x$cases, each = 3)],
      y = temp_df$PC2[rep(x$cases, each = 3)],
      xend = temp_df$PC1[unlist(x$controls)],
      yend = temp_df$PC2[unlist(x$controls)]
    )

    g <- ggplot(temp_df, aes(x = PC1, y = PC2, col = unit)) + geom_point() +
           geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                        data = temp_lines,
                        col = "black", lty = 2)
  } else if (x$type == "group")
  {
    temp_mean <- data.frame(x = c(mean(temp_df$PC1[temp_df$unit == "trt"]),
                                  mean(temp_df$PC1[temp_df$unit == "ctrl"])),
                            y = c(mean(temp_df$PC2[temp_df$unit == "trt"]),
                                  mean(temp_df$PC2[temp_df$unit == "ctrl"])),
                            unit = c("trt","ctrl"),
                            size = c(6, 6),
                            pch = c(1, 1)
    )

    g <- ggplot(temp_df, aes(x = PC1, y = PC2, col = unit)) +
           geom_point() +
           geom_point(aes(x = x, y = y, col = unit), size = temp_mean$size,
                      pch = temp_mean$pch,
                      data = temp_mean)

  } else
  {
    stop("type not recognized")
  }
  return(g)
}
