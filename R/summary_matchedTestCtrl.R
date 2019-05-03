summary.matchedTestCtrl <- function(object, ...)
{
  if (object$type == "1-1")
  {
    test_stat <- object$data[object$cases,-1] - object$data[object$controls,-1]
    me <- apply(test_stat, 2, mean)
    s <- apply(test_stat, 2, var)
    n <- length(object$cases)
    m <- length(object$controls)
  } else if (object$type == "1-m")
  {
    test_stat <- object$data[object$cases,-1] -
      t(sapply(object$controls, function(x) apply(object$data[x,-1], 2, mean)))
    me <- apply(test_stat, 2, mean)
    s <- apply(test_stat, 2, var)
    n <- length(object$cases)
    m <- length(object$controls[[1]])
  } else if (object$type == "group")
  {
    test_stat <- apply(object$data[object$cases,-1], 2, mean) -
      apply(object$data[object$controls,-1], 2, mean)
    test_stat_var <- apply(object$data[object$cases,-1], 2, var) +
      apply(object$data[object$controls,-1], 2, var)
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

