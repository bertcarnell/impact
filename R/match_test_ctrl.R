# Copyright 2019 Robert Carnell

# return the minimum index of a vector x, breking ties at random
.which.is.min <- function(x, na.rm = TRUE)
{
  y <- seq_along(x)[(x == min(x, na.rm = na.rm))]
  if (length(y) > 1L)
    sample(y, 1L)
  else y
}

#' Match ctrl units to identified test units
#'
#' Inspired by the matchControl function in e1071
#'
#' @param cluster_formula a formula of the type trt ~ covariates
#' @param data a data.frame where each row is a unit and the columns are observations of the covariates to match on.  Must at least contain a trt variable.
#' @param type the type of analysis to perform, diff or ratio
#' @param method the method of control matches, group, 1-1, or 1-m
#' @param test_name the name of the test level in the trt variable
#' @param ctrl_name the name of the control level in the trt variable
#' @param replace should the control groups be replaced in the pool after they are matched to a test unit
#' @param m the number of ctrl units should be matched to each test unit
#' @param group_tol the relative tolerance of the group matching algorithm.  One stopping criteria is that the distance between the full control group and the test group is less than group_tol times the original distance.
#'
#' @return an object of matchedTestCtrl
#' @export
#'
#' @importFrom cluster daisy
#' @importFrom stats as.dist model.frame na.fail
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(id = factor(LETTERS),
#'                  trt = factor(c(rep("test", 4), rep("ctrl", 22))),
#'                  metric1_time1 = rnorm(26),
#'                  metric1_time2 = rnorm(26),
#'                  metric1_time3 = rnorm(26),
#'                  metric2_time1 = rnorm(26),
#'                  metric2_time2 = rnorm(26),
#'                  metric2_time3 = rnorm(26))
#' m1 <- match_test_ctrl(paste("trt ~", paste(
#'   paste("metric", rep(1:2, each = 3),
#'         "_time", rep(1:3, times = 2),
#'         sep = ""), collapse = " + ")),
#'   data = df,
#'   type = "1-1", method = "diff",
#'   test_name = "test", ctrl_name = "ctrl",
#'   replace = FALSE,
#'   m = NA, group_tol = 0.01)
match_test_ctrl <- function(cluster_formula, data,
                            type = "group", method = "diff",
                            test_name = "test", ctrl_name = "ctrl",
                            replace = FALSE,
                            m = NA, group_tol = 0.01)
{
  #cluster_formula <- formula(paste0("trt ~ ",
  #                                  paste(names(Y[,-which(names(Y) %in% c("trt","CostCenter"))]),
  #                                        collapse = "+")))
  #data = Y[apply(Y, 1, function(y) !any(is.na(y))),]
  #ctrl_name = "ctrl"
  #test_name = "test"
  #replace = FALSE
  #m = 5
  #group_tol = 0.01

  assertthat::assert_that(!any(is.na(data)),
                          msg = "The data object cannot contain NA's")
  assertthat::assert_that("trt" %in% names(data),
                          msg = "Data object must contain a trt variable")
  if (type == "1-m")
  {
    assertthat::assert_that(!is.na(m),
                            msg = "m cannot be NA if method is 1-m")
  }
  if (method == "ratio")
    stop("Not implemented yet")

  model_frame <- stats::model.frame(cluster_formula, data = data, subset = NULL,
                                    na.action = stats::na.fail,
                                    drop.unused.levels = FALSE,
                                    xlev = NULL)

  controls <- which(model_frame$trt == ctrl_name)
  cases <- which(model_frame$trt == test_name)
  # need to take the trt column out before clustering
  my_clusters <- cluster::daisy(model_frame[,-1])
  d <- as.matrix(stats::as.dist(my_clusters))

  if (type == "1-1")
  {
    retval <- rep(NA, length(cases))
    for (k in 1:length(cases))
    {
      retval[k] <- controls[.which.is.min(d[cases[k], controls], na.rm = TRUE)]
      if (!replace)
        controls <- controls[controls != retval[k]]
    }
  } else if (type == "1-m")
  {
    retval <- vector("list", length(cases))
    if (replace == TRUE)
    {
      for (k in seq_along(cases))
      {
        o <- order(d[cases[k], controls], na.last = TRUE, decreasing = FALSE)
        retval[[k]] <- controls[o[1:m]]
      }
    } else {
      for (k in seq_along(cases))
      {
        retval[[k]] <- integer(m)
      }
      for (j in 1:m)
      {
        for (k in seq_along(cases))
        {
          retval[[k]][j] <- controls[.which.is.min(d[cases[k], controls], na.rm = TRUE)]
          if (!replace)
            controls <- controls[controls != retval[[k]][j]]
        }
      }
    }
  } else if (type == "group")
  {
    # first grab all the controls
    #   then drop the farthest away until the mean is in range
    mean_test <- apply(model_frame[cases,-1], 2, mean, na.rm = TRUE)
    mean_ctrl <- apply(model_frame[controls,-1], 2, mean, na.rm = TRUE)
    dist_init <- sqrt(sum((mean_test - mean_ctrl)^2))
    temp_controls <- controls
    dist_temp_new <- dist_init
    dist_temp_best <- dist_init
    while (dist_temp_new > group_tol*dist_init &
           length(temp_controls) > length(cases))
    {
      ind <- which.max(apply(d[cases, temp_controls], 2, sum, na.rm = TRUE))
      temp_controls <- temp_controls[-ind]
      mean_ctrl_temp <- apply(model_frame[temp_controls,-1], 2, mean, na.rm = TRUE)
      dist_temp_new <- sqrt(sum((mean_test - mean_ctrl_temp)^2))
      # store the best one so far
      if (dist_temp_new < dist_temp_best)
      {
        stored_controls <- temp_controls
        dist_temp_best <- dist_temp_new
      }
    }
    # if the control group gets too small, default to the 1-1 case
    if (length(temp_controls) <= length(cases))
      return(match_test_ctrl(cluster_formula, data = data,
                             type = "1-1", method = method,
                             test_name = "test", ctrl_name = "ctrl",
                             replace = replace,
                             m = m, group_tol = group_tol))
      retval <- stored_controls
  } else
  {
    stop("method not recognized")
  }
  ret <- list(data = model_frame, cases = cases, controls = retval,
              method = method, type = type,
              dist = d)
  class(ret) <- "matchedTestCtrl"
  return(ret)
}
