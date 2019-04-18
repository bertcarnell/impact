# Copyright 2019 Robert Carnell

#' Plot the impact data step change
#'
#' @param dat impact data
#' @param method diff or ratio
#' @param type raw or corrected
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
#' @importFrom plyr ddply
#'
#' @examples
#' a <- 4
plot_impact_data_step_change <- function(dat, method = "diff", type = "raw")
{
  #method = "ratio"
  #type = "corrected"
  assertthat::assert_that(type %in% c("raw", "corrected"))
  assertthat::assert_that(method %in% c("diff", "ratio"))
  # TODO:  Eliminate false positive note in R CMD check.  Eliminate this...
  trt <- NULL
  if (type == "raw")
  {
    datsum <- ddply(dat, c("time","trt"), summarize, val = mean(val, na.rm = TRUE))
    g <- ggplot(dat, aes_string(x = "time", y = "val", group = "trt", col = "trt")) +
      geom_point() +
      geom_line(aes_string(x = "time", y = "val", group = "trt", col = "trt"),
                data = datsum)
    return(g)
  } else {
    datsum <- ddply(
      ddply(dat, c("time", "trt", "pre_post"), summarize, val = mean(val, na.rm = TRUE)),
      c("trt", "pre_post"), summarize, val = mean(val))
    if (method == "diff")
    {
      adjust <- with(datsum, val[trt == "ctrl" & pre_post == "pre"])
      dat$adjust <- adjust
      plotdat <- within(dat, val <- ifelse(trt == "test", val - adjust, val))
    } else {
      adjust <- with(datsum, val[trt == "ctrl" & pre_post == "pre"]) /
        with(datsum, val[trt == "test" & pre_post == "pre"])
      dat$adjust <- adjust
      plotdat <- within(dat, val <- ifelse(trt == "test", val * adjust, val))
    }
    plotdatsum <- ddply(plotdat, c("time", "trt"), summarize,
                        val = mean(val, na.rm = TRUE))
    g <- ggplot(plotdat, aes_string(x = "time", y = "val", group = "trt", col = "trt")) +
      geom_point() +
      geom_line(aes_string(x = "time", y = "val", group = "trt", col = "trt"),
                data = plotdatsum)
    return(g)
  }
}
