% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_matchedTestCtrl.R
\name{summary.matchedTestCtrl}
\alias{summary.matchedTestCtrl}
\title{Summary method for matchedTestCtrl object}
\usage{
\method{summary}{matchedTestCtrl}(object, ...)
}
\arguments{
\item{object}{an object of class matchedTestCtrl}

\item{...}{furhter parameters passed to teh summary method}
}
\value{
invisible list of summary information
}
\description{
Summary method for matchedTestCtrl object
}
\examples{
 df <- data.frame(id = factor(LETTERS),
    trt = factor(c(rep("test", 4), rep("ctrl", 22))),
    metric1_time1 = rnorm(26),
    metric1_time2 = rnorm(26),
    metric1_time3 = rnorm(26),
    metric2_time1 = rnorm(26),
    metric2_time2 = rnorm(26),
    metric2_time3 = rnorm(26))
 m1 <- match_test_ctrl(paste("trt ~", paste(
   paste("metric", rep(1:2, each = 3),
         "_time", rep(1:3, times = 2),
         sep = ""), collapse = " + ")),
   data = df,
   type = "1-1", method = "diff",
   test_name = "test", ctrl_name = "ctrl",
   replace = FALSE,
   m = NA, group_tol = 0.01)
   summary(m1)
}
