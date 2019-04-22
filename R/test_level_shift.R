# Copyright 2019 Robert Carnell

# Internal function to take means across time
#
# dat =  a data.frame with columns pre_post, trt, and val at least
# pre_post_name = The value of pre_post to use to subset the data
# trt_name = The value of trt to use to subset the data
#
# return the mean vector
.mean_across_time <- function(dat, pre_post_name, trt_name)
{
  mean(with(dat, val[pre_post == pre_post_name & trt == trt_name]), na.rm = TRUE)
}

# Internal function used in the group means bootstrap
#
# boot_dat = a data.frame of measurement unit id's and strata to boostrap
# i = the row numbers of the bootstrap sample
# full_dat = the full data.frame containing the raw data
# method = diff or ratio
# val_name = name of the val column
# pre_name = the name of the pre level
# post_name = the name of the post level
# test_name = the name of the test treatment
# ctrl_name = the name of the ctrl treatment
#
# @return the test statistic
.boot_func_group_mean <- function(boot_dat, i, full_dat, method, val_name,
                                  pre_name, post_name, test_name,
                                  ctrl_name)
{
  int_dat <- NULL
  for (j in i)
  {
    # this is highly memory intensive - would be better to do with sequential means
    #  The time steps makes it hard
    # would normally use subset here but for R CMD check
    ind <- with(full_dat, id == boot_dat$id[j] & trt == boot_dat$strata[j])
    int_dat <- rbind(int_dat, full_dat[ind,])
  }

  #temp <- plyr::ddply(int_dat, .(pre_post, time, trt), summarize, val = mean(val, na.rm = TRUE))
  temp <- plyr::ddply(int_dat, c("pre_post", "time", "trt"), function(x) {
    data.frame(pre_post = x$pre_post[1],
               time = x$time[1],
               trt = x$trt[1],
               val = mean(x[[val_name]], na.rm = TRUE))
  })
  ctrl_avg_pre <- .mean_across_time(temp, pre_name, ctrl_name)
  test_avg_pre <- .mean_across_time(temp, pre_name, test_name)
  ctrl_avg_post <- .mean_across_time(temp, post_name, ctrl_name)
  test_avg_post <- .mean_across_time(temp, post_name, test_name)

  if (method == "diff")
  {
    test_stat <- (test_avg_post - ctrl_avg_post) - (test_avg_pre - ctrl_avg_pre)
  } else
  {
    test_stat <- test_avg_post - test_avg_pre / ctrl_avg_pre * ctrl_avg_post
  }
  return(test_stat)
}


# Bootstrap function for the 1-1 type
#
# boot_dat data.frame of ids of measurement units (with an associated matched pair)
# i the rows of the bootstrap sample
# full_dat the full data.frame where the matched pairs are merged
# method ratio or diff
# pre_name = the name of the pre level
# post_name = the name of the post level
#
# @return the boostrap test statistic
.boot_func_1_1 <- function(boot_dat, i, full_dat, method,
                           pre_name = pre_name, post_name = post_name)
{
  int_dat <- NULL
  for (j in i)
  {
    # this is highly memory intensive - would be better to do with sequential means
    #  The time steps makes it hard
    # These are the test group ids in full_dat
    #int_dat <- rbind(int_dat, subset(full_dat, id == boot_dat$id[j]))
    int_dat <- rbind(int_dat, full_dat[full_dat$id == boot_dat$id[j],])
  }

  #boottemp <- plyr::ddply(int_dat, .(time, pre_post), val = mean(val, na.rm = TRUE))
  boottemp <- plyr::ddply(int_dat, c("time", "pre_post"), function(x){
    data.frame(time = x$time[1],
               pre_post = x$pre_post[1],
               val = mean(x$val, na.rm = TRUE))
  })
  pre <- mean(boottemp$val[boottemp$pre_post == pre_name], na.rm = TRUE)
  post <- mean(boottemp$val[boottemp$pre_post == post_name], na.rm = TRUE)
  test_stat <- post - pre

  return(test_stat)
}

#' Test Level Shift in Test-Control Experiment
#'
#' @param dat a dataframe with columns including pre_post, time, id, trt, val
#' @param type \code{group} for testing two groups of size m and n, \code{1-1} for testing matched pairs, \code{1-m} for testing matches between one test and m ctrl
#' @param method \code{diff} for normalizing by the difference in test=control and \code{ratio} for normalizing by the ratio of test / control
#' @param val_name the name the value column in the data.frame dat
#' @param test_name the name of the test group in the trt column
#' @param ctrl_name the name of the control group in the trt column
#' @param pre_name the name of the pre time period in the pre_post column
#' @param post_name the name of the post time period in the pre_post column
#' @param R the number of boostrap replicates
#'
#' @return impactResult object
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom boot boot
#' @importFrom plyr ddply
#' @importFrom stats quantile
#'
#' @examples
#' dat <- data.frame(trt = rep(c("test","ctrl"), each = 6),
#'                   pre_post = rep(c("pre","post"), each = 3, times = 2),
#'                   id = as.character(c(1,2,3,1,2,3,4,5,6,4,5,6)),
#'                   time = as.character(c(1,1,1,2,2,2,1,1,1,2,2,2)),
#'                   val = c(10,11,12,13,14,15,10,11,10,12,10,10.5))
#' test_result <- test_level_shift(dat, type = "group", method = "diff", R = 100)
test_level_shift <- function(dat, type = "group", method = "diff",
                             val_name = "val",
                             test_name = "test", ctrl_name = "ctrl",
                             pre_name = "pre", post_name = "post",
                             R = 1000)
{
  # type = "group"
  # method = "diff"
  # test_name = "test"
  # ctrl_name = "ctrl"
  # pre_name = "pre"
  # post_name = "post"
  # R = 100

  validate_impact_data(dat)
  assertthat::assert_that(type %in% c("group", "1-1", "1-m"),
                          msg = "The type variable must be one of group, 1-1, or 1-m")
  assertthat::assert_that(method %in% c("diff", "ratio"),
                          msg = "The method variable must be either diff or ratio")

  if (type == "group")
  {
    # mean across units, within time
    # would normally use "summarize" here, but using an alternate to stop R CMD check notes
    #temp <- plyr::ddply(dat, .(pre_post, time, trt), summarize, val = mean(val, na.rm = TRUE))
    temp <- plyr::ddply(dat, c("pre_post", "time", "trt"), function(x) {
      data.frame(pre_post = x$pre_post[1],
                 time = x$time[1],
                 trt = x$trt[1],
                 val = mean(x[[val_name]], na.rm = TRUE))
    })
    ctrl_avg_pre <- .mean_across_time(temp, pre_name, ctrl_name)
    test_avg_pre <- .mean_across_time(temp, pre_name, test_name)
    ctrl_avg_post <- .mean_across_time(temp, post_name, ctrl_name)
    test_avg_post <- .mean_across_time(temp, post_name, test_name)

    if (method == "diff")
    {
      test_stat <- (test_avg_post - ctrl_avg_post) - (test_avg_pre - ctrl_avg_pre)
    } else
    {
      test_stat <- test_avg_post - test_avg_pre / ctrl_avg_pre * ctrl_avg_post
    }

    # bootstrap measurement units
    #  If there is more than one unique treatment per id, then this will fail
    #boot_dat <- plyr::ddply(dat, c("id"), summarize, strata = unique(trt))
    boot_dat <- plyr::ddply(dat, c("id"), function(x){
      data.frame(id = x$id[1],
                 strata = x$trt[1])
    })
    b1 <- boot::boot(boot_dat, .boot_func_group_mean, R = R,
                     stype = "i", strata = boot_dat$strata,
                     full_dat = dat, method = method, val_name = val_name,
                     pre_name = pre_name, post_name = post_name,
                     test_name = test_name, ctrl_name = ctrl_name)
  } else if (type == "1-1")
  {
    assertthat::assert_that("matchid" %in% names(dat),
                            msg = "the input data.frame must contain a matchid column for the 1-1 type")
    ind_ctrl <- which(dat$trt == ctrl_name)
    ind_test <- which(dat$trt == test_name)
    temp <- merge(dat[ind_ctrl,], dat[ind_test,],
                  by.x = c("pre_post","time","matchid"),
                  by.y = c("pre_post","time","id"))

    if (method == "diff")
    {
      temp$val <- temp[[paste0(val_name, ".y")]] - temp[[paste0(val_name, ".x")]]
    } else
    {
      temp$val <- temp[[paste0(val_name, ".y")]] / temp[[paste0(val_name, ".x")]]
    }
    pre <- mean(temp$val[temp$pre_post == pre_name], na.rm = TRUE)
    post <- mean(temp$val[temp$pre_post == post_name], na.rm = TRUE)
    test_stat <- post - pre

    # bootstrap pairs together
    boot_dat <- data.frame(id = unique(temp$id))
    b1 <- boot::boot(boot_dat, .boot_func_1_1, R = R, stype = "i",
                     full_dat = temp, method = method, pre_name = pre_name,
                     post_name = post_name)
  }

  ret <- list(result = test_stat,
              bootstrap_mean = b1$t0,
              bootstrap_results = b1$t,
              bootstrap_interval = stats::quantile(b1$t, probs = c(0.025, 0.975)),
              pvalue = length(which(b1$t < 0)) / length(b1$t),
              type = type,
              method = method
              )
  class(ret) <- "impactResult"
  return(ret)
}
