# Copyright 2019 Robert Carnell

#' Summary method for matchedTestCtrl object
#'
#' @param object an object of class matchedTestCtrl
#' @param ... furhter parameters passed to teh summary method
#'
#' @return invisible list of summary information
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom stats var
#'
#' @examples
#'  df <- data.frame(id = factor(LETTERS),
#'     trt = factor(c(rep("test", 4), rep("ctrl", 22))),
#'     metric1_time1 = rnorm(26),
#'     metric1_time2 = rnorm(26),
#'     metric1_time3 = rnorm(26),
#'     metric2_time1 = rnorm(26),
#'     metric2_time2 = rnorm(26),
#'     metric2_time3 = rnorm(26))
#'  m1 <- match_test_ctrl(paste("trt ~", paste(
#'    paste("metric", rep(1:2, each = 3),
#'          "_time", rep(1:3, times = 2),
#'          sep = ""), collapse = " + ")),
#'    data = df,
#'    type = "1-1", method = "diff",
#'    test_name = "test", ctrl_name = "ctrl",
#'    replace = FALSE,
#'    m = NA, group_tol = 0.01)
#'    summary(m1)
summary.matchedTestCtrl <- function(object, ...)
{
  if (object$type == "1-1")
  {
    assertthat::assert_that(length(object$cases) == length(object$controls),
                            msg = "matchedTestCtrl object of type 1-1 with length of test not equal to the length of controls")
    if (object$type == "diff")
    {
      test_stat <- object$data[object$cases,-1] - object$data[object$controls, -1]
    } else
    {
      test_stat <- object$data[object$cases,-1] / object$data[object$controls, -1]
    }
    me <- apply(test_stat, 2, mean)
    s <- apply(test_stat, 2, stats::var)
    n <- length(object$cases)
    m <- length(object$controls)
  } else if (object$type == "1-m")
  {
    assertthat::assert_that(length(object$cases) == length(object$controls),
                            msg = "matchedTestCtrl object of type 1-1 with length of test not equal to the length of controls")
    if (object$type == "diff")
    {
      test_stat <- object$data[object$cases,-1] -
        t(sapply(object$controls, function(x) apply(object$data[x,-1], 2, mean)))
    } else
    {
      test_stat <- object$data[object$cases,-1] /
        t(sapply(object$controls, function(x) apply(object$data[x,-1], 2, mean)))
    }
    me <- apply(test_stat, 2, mean)
    s <- apply(test_stat, 2, stats::var)
    n <- length(object$cases)
    m <- length(object$controls[[1]])
  } else if (object$type == "group")
  {
    if (object$type == "diff")
    {
      test_stat <- apply(object$data[object$cases,-1], 2, mean) -
        apply(object$data[object$controls,-1], 2, mean)
      test_stat_var <- apply(object$data[object$cases,-1], 2, stats::var) +
        apply(object$data[object$controls,-1], 2, stats::var)
    } else
    {
      test_stat <- apply(object$data[object$cases,-1], 2, mean) /
        apply(object$data[object$controls,-1], 2, mean)
      # TODO:  supply an approximate variance calculation here
      test_stat_var <- rep(NA, ncol(object$data))
    }
    me <- test_stat
    s <- test_stat_var
    n <- length(object$cases)
    m <- length(object$controls)
  } else
  {
    stop("non recognized type")
  }
  cat("Matched Test and Control\n")
  cat("  Test Cases:     ", n, "\n")
  cat("  Control Cases:  ", m, "\n")
  cat("  Type:           ", object$type, "\n")
  cat("\n  Means:\n")
  print(me)
  cat("\n  Standard Deviations: \n")
  print(s)
  invisible(list(mean = me, var = s))
}

# TODO:  What to do about standardiztion in the package?
# TODO:  What to do about weighting?

