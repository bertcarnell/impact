# Copyright 2019 Robert Carnell

#' Plot the impact data step change
#'
#' @param dat impact data
#' @param method diff or ratio
#' @param type raw or corrected
#' @param val_name the name of the value column
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
#' @importFrom plyr ddply summarize
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' plot_impact_data_step_change(dat)
#' plot_impact_data_step_change(dat, type = "raw")
plot_impact_data_step_change <- function(dat, method = "diff", type = "raw",
                                         val_name = "val")
{
  #method = "ratio"
  #type = "corrected"
  assertthat::assert_that(type %in% c("raw", "corrected"))
  assertthat::assert_that(method %in% c("diff", "ratio"))
  # TODO:  Eliminate false positive note in R CMD check.  Eliminate this...
  trt <- NULL
  if (type == "raw")
  {
    #datsum <- ddply(dat, c("time","trt"), summarize, val = mean(val, na.rm = TRUE))
    datsum <- ddply(dat, c("time","trt"), function(x) {
      data.frame(time = x$time[1],
                 trt = x$trt[1],
                 val = mean(x[[val_name]], na.rm = TRUE))
    })
    g <- ggplot(dat, aes_string(x = "time", y = val_name, group = "trt", col = "trt")) +
      geom_point() +
      geom_line(aes_string(x = "time", y = "val", group = "trt", col = "trt"),
                data = datsum)
    return(g)
  } else {
    #datsum <- ddply(
    #  ddply(dat, c("time", "trt", "pre_post"), summarize, val = mean(val, na.rm = TRUE)),
    #  c("trt", "pre_post"), summarize, val = mean(val))
    datsum_pre <- ddply(dat, c("time","trt","pre_post"), function(x) {
      data.frame(time = x$time[1],
                 trt = x$trt[1],
                 pre_post = x$pre_post[1],
                 val = mean(x[[val_name]], na.rm = TRUE))
    })
    datsum <- ddply(datsum_pre, c("trt","pre_post"), function(x){
      data.frame(trt = x$trt[1],
                 pre_post = x$pre_post[1],
                 val = mean(x$val))
    })
    if (method == "diff")
    {
      adjust <- with(datsum, val[trt == "ctrl" & pre_post == "pre"])
      dat$adjust <- adjust
      dat$val <- ifelse(dat$trt == "test", dat[[val_name]] - adjust, dat[[val_name]])
      plotdat <- dat
    } else {
      adjust <- with(datsum, val[trt == "ctrl" & pre_post == "pre"]) /
        with(datsum, val[trt == "test" & pre_post == "pre"])
      dat$adjust <- adjust
      dat$val <- ifelse(dat$trt == "test", dat[[val_name]] * adjust, dat[[val_name]])
      plotdat <- dat
    }
    #plotdatsum <- ddply(plotdat, c("time", "trt"), summarize,
    #                    val = mean(val, na.rm = TRUE))
    plotdatsum <- ddply(plotdat, c("time", "trt"), function(x){
      data.frame(time = x$time[1],
                 trt = x$trt[1],
                 val = mean(x$val, na.rm = TRUE))
    })
    g <- ggplot(plotdat, aes_string(x = "time", y = "val", group = "trt", col = "trt")) +
      geom_point() +
      geom_line(aes_string(x = "time", y = "val", group = "trt", col = "trt"),
                data = plotdatsum)
    return(g)
  }
}
