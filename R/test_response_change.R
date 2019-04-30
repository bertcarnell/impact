# Copyright 2019 Robert Carnell

# default formula
.defaultFormula <- formula(val ~ pre_post * trt * numtime)
.defaultFormula1_1 <- formula(val ~ pre_post * numtime)

# bootstrap function model for groups
#
# boot_dat = bootstrap ids
# i = the bootstrap sample
# full_dat = the full data set
# method = diff or ratio
# val_name = name of the value column
# pre_name = name of the pre-period
# post_name = name of the post period
# test_name = name of the test treatment
# ctrl_name = name of the control treatment
# test_formula = formula for the test
# test_time = time at which to test the hypothesis
#
# return = bootstrap sample results
.boot_func_group_mean_model <- function(boot_dat, i, full_dat, method,
                                        val_name, pre_name, post_name,
                                        test_name, ctrl_name, test_formula,
                                        test_time)
{
  beta6 <- paste0("pre_post", post_name, ":trt", test_name)
  beta7 <- paste0(beta6, ":numtime")
  int_dat <- NULL
  for (j in i)
  {
    # this is highly memory intensive - would be better to do with sequential means
    #  The time steps makes it hard
    # would normally use subset here but for R CMD check
    ind <- with(full_dat, id == boot_dat$id[j] & trt == boot_dat$strata[j])
    int_dat <- rbind(int_dat, full_dat[ind,])
  }

  # bootstrap the linear model
  lm1_boot <- stats::lm(test_formula, data = int_dat)
  lm2_boot <- stats::lm(update(test_formula, val ~ . - pre_post:trt:numtime), data = int_dat)
  # don't need to bootstrap the mixed model because the parameter estimates are the same.
  #   The variance is what is different
  names_coef_lm1_boot <- names(coef(lm1_boot))
  ind_lm <- c(which(names_coef_lm1_boot == beta6),
              which(names_coef_lm1_boot == beta7))
  ind_lm1 <- which(names_coef_lm1_boot == beta7)
  ind_lm2 <- which(names(coef(lm2_boot)) == beta6)

  # scale the parameters by the time and sum them
  ret <- c(coef(lm1_boot)[ind_lm1],
           sum(coef(lm1_boot)[ind_lm]*c(1, test_time)),
           coef(lm2_boot)[ind_lm2])
  names(ret) <- c(beta7, paste0(beta6, "+", beta7, "*numtime"), beta6)
  return(ret)
}

.boot_func_1_1_model <- function(boot_dat, i, full_dat, method,
                                 val_name, pre_name, post_name,
                                 test_name, ctrl_name, test_formula,
                                 test_time)
{
  beta1 <- paste0("pre_post", post_name)
  beta3 <- paste0(beta1, ":numtime")
  int_dat <- NULL
  for (j in i)
  {
    # this is highly memory intensive - would be better to do with sequential means
    #  The time steps makes it hard
    # would normally use subset here but for R CMD check
    ind <- with(full_dat, id == boot_dat$id[j])
    int_dat <- rbind(int_dat, full_dat[ind,])
  }

  # bootstrap the linear model
  lm1_boot <- stats::lm(test_formula, data = int_dat)
  lm2_boot <- stats::lm(update(test_formula, val ~ . - pre_post:trt:numtime), data = int_dat)
  # don't need to bootstrap the mixed model because the parameter estimates are the same.
  #   The variance is what is different
  names_coef_lm1_boot <- names(coef(lm1_boot))
  ind_lm <- c(which(names_coef_lm1_boot == beta1),
              which(names_coef_lm1_boot == beta3))
  ind_lm1 <- which(names_coef_lm1_boot == beta3)
  ind_lm2 <- which(names(coef(lm2_boot)) == beta1)

  # scale the parameters by the time and sum them
  ret <- c(coef(lm1_boot)[ind_lm1],
           sum(coef(lm1_boot)[ind_lm]*c(1, test_time)),
           coef(lm2_boot)[ind_lm2])
  names(ret) <- c(beta3, paste0(beta1, "+", beta3, "*numtime"), beta1)
  return(ret)
}


#' Test a change in response between test and control
#'
#' @inheritParams test_level_shift
#' @param test_time the time at which to conduct the test
#'
#' @return impactResult object
#' @export
#'
#' @importFrom lme4 lmer
#' @importFrom multcomp glht
#' @importFrom stats lm
#' @importFrom stats quantile
#' @importFrom boot boot
#' @importFrom plyr ddply
#' @importFrom assertthat assert_that
#'
#' @examples
#' set.seed(1976)
#'  dat <- data.frame(trt = rep(c("test","ctrl"), each = 12),
#'                    pre_post = rep(c("pre","post"), each = 6, times = 2),
#'                    id = c(rep(c("1","2","3"), times = 4), rep(c("4","5","6"), times = 4)),
#'                    time = rep(1:4, each = 3, times = 2),
#'                    val = rnorm(24, mean = 10, sd = 5))
#' test_result <- test_response_change(dat, R = 100)
test_response_change <- function(dat, type = "group", method = "diff",
                                 val_name = "val",
                                 test_name = "test", ctrl_name = "ctrl",
                                 pre_name = "pre", post_name = "post",
                                 test_time = NA,
                                 R = 1000)
{
  # type = "group"
  # method = "diff"
  # test_name = "test"
  # ctrl_name = "ctrl"
  # pre_name = "pre"
  # post_name = "post"
  # val_name = "val"
  # R = 100

  validate_impact_data(dat)
  assertthat::assert_that(type %in% c("group", "1-1", "1-m"),
                          msg = "The type variable must be one of group, 1-1, or 1-m")
  assertthat::assert_that(method %in% c("diff", "ratio"),
                          msg = "The method variable must be either diff or ratio")
  assertthat::assert_that(!(method == "ratio" & all(dat[[val_name]] > 0)),
                          msg = "If the ratio method is selected then the values must be strictly positive")

  if (type == "group")
  {
    test_formula <- .defaultFormula
  } else
  {
    test_formula <- .defaultFormula1_1
  }
  # normalize the val_name to make the formula writing easier
  if (!("val" %in% names(dat)))
  {
    dat$val <- dat[[val_name]]
  }
  # if the ratio method is specified, then log the values (values are tested to be positive)
  if (method == "ratio")
  {
    dat$val <- log(dat$val)
  }
  # name the date numeric
  dat$numtime <- as.numeric(dat$time)
  if (is.na(test_time))
  {
    test_time <- max(dat$numtime)
  } else
  {
    test_time <- as.numeric(test_time)
  }
  # ensure the factor ordering is pre, then post
  if (all(levels(dat$pre_post) != c(pre_name, post_name)))
  {
    dat$pre_post <- factor(as.character(dat$pre_post), levels = c(pre_name, post_name))
  }

  if (type == "1-1")
  {
    assertthat::assert_that("matchid" %in% names(dat),
                            msg = "the input data.frame must contain a matchid column for the 1-1 type")
    ind_ctrl <- which(dat$trt == ctrl_name)
    ind_test <- which(dat$trt == test_name)
    temp <- merge(dat[ind_ctrl,], dat[ind_test,],
                  by.x = c("pre_post","numtime","matchid"),
                  by.y = c("pre_post","numtime","id"))

    if (method == "diff")
    {
      temp$val <- temp[[paste0(val_name, ".y")]] - temp[[paste0(val_name, ".x")]]
    } else
    {
      temp$val <- temp[[paste0(val_name, ".y")]] / temp[[paste0(val_name, ".x")]]
    }
    dat <- temp

  } else if (type == "1-m")
  {
    stop("not implemented")
  }

  # fit linear models
  lm1 <- stats::lm(test_formula, data = dat)
  if (type == "group")
  {
    lm2 <- stats::lm(update(test_formula, val ~ . - pre_post:trt:numtime), data = dat)
  } else
  {
    lm2 <- stats::lm(update(test_formula, val ~ . - pre_post:numtime), data = dat)
  }
  # check for NA coefficients meaning there is not enough data
  if (any(is.na(lm1)))
  {
    stop("Insufficient data to fit the requested model.  Consider using another method")
  }
  # add a random effect of id nested within treatment
  #   if there are not enough observations to next id within treatment, then
  #   this can fail
  bUseMixed <- FALSE
  tryCatch({
    lmer1 <- lme4::lmer(update(test_formula, ~ . + (trt | id)), data = dat)
    if (any(lmer1@optinfo$conv$lme4$message == "singular fit"))
    {
      cat("Mixed model not used\n")
    } else {
      bUseMixed <- TRUE
    }
  }, error = function(e) {cat("Mixed Model not used\n")})
  # create linear combinations of variables
  names_coef_lm1 <- names(coef(lm1))
  if (type == "group")
  {
    beta6 <- paste0("pre_post", post_name, ":trt", test_name)
    beta7 <- paste0(beta6, ":numtime")
    K <- matrix(0, nrow = 2, ncol = length(coef(lm1)))
    K[1, names_coef_lm1 == beta7] <- 1 # not test time
    K[2, names_coef_lm1 == beta6] <- 1
    K[2, names_coef_lm1 == beta7] <- test_time
    K2 <- matrix(0, nrow = 1, ncol = length(coef(lm2)))
    K2[1, names(coef(lm2)) == beta6] <- 1
    rownames(K) <- c(beta7,
                     paste0(beta6, "+", beta7, "*numtime"))
    rownames(K2) <- beta6
  } else {
    beta1 <- paste0("pre_post", post_name)
    beta3 <- paste0(beta1, ":numtime")
    K <- matrix(0, nrow = 2, ncol = length(coef(lm1)))
    K[1, names_coef_lm1 == beta3] <- 1 # not test time
    K[2, names_coef_lm1 == beta1] <- 1
    K[2, names_coef_lm1 == beta3] <- test_time
    K2 <- matrix(0, nrow = 1, ncol = length(coef(lm2)))
    K2[1, names(coef(lm2)) == beta1] <- 1
    rownames(K) <- c(beta3,
                     paste0(beta1, "+", beta3, "*numtime"))
    rownames(K2) <- beta1
  }
  glht1 <- multcomp::glht(lm1, linfct = K)
  if (bUseMixed)
  {
    glmmht1 <- multcomp::glht(lmer1, linfct = K)
  }
  glht2 <- multcomp::glht(lm2, linfct = K2)

  # now bootstrap the test statistic
  if (type == "group")
  {
    boot_dat <- plyr::ddply(dat, c("id"), function(x){
      data.frame(id = x$id[1],
                 strata = x$trt[1])
    })
    b1 <- boot::boot(boot_dat, .boot_func_group_mean_model, R = R,
                     stype = "i", strata = boot_dat$strata,
                     full_dat = dat, method = method, val_name = val_name,
                     pre_name = pre_name, post_name = post_name,
                     test_name = test_name, ctrl_name = ctrl_name,
                     test_formula = test_formula, test_time = test_time)
  } else # 1-1 and 1-m
  {
    boot_dat <- data.frame(id = unique(dat$id))
    b1 <- boot::boot(boot_dat, .boot_func_1_1_model, R = R,
                     stype = "i",
                     full_dat = dat, method = method, val_name = val_name,
                     pre_name = pre_name, post_name = post_name,
                     test_name = test_name, ctrl_name = ctrl_name,
                     test_formula = test_formula, test_time = test_time)
  }

  # first test pre_post:trt:time
  #  if significant, then test pre_post:trt:time + pre_post:trt
  #  if not significant, then test pre_post:trt
  resultVector <- c(apply(glht1$linfct %*% matrix(glht1$coef, ncol = 1), 1, sum),
                    sum(glht2$linfct * glht2$coef))
  if (type == "group")
  {
    names(resultVector) <- c(beta7,
                             paste0(beta6, "+", beta7, "*numtime"),
                             beta6)
  } else {
    names(resultVector) <- c(beta3,
                             paste0(beta1, "+", beta3, "*numtime"),
                             beta1)
  }
  resultInterval <- apply(b1$t, 2, stats::quantile, probs = c(0.025, 0.975))
  colnames(resultInterval) <- names(resultVector)
  resultPvalue <- apply(b1$t, 2, function(x) length(which(x < 0)) / length(x))
  names(resultPvalue) <- names(resultVector)
  # can't use ifelse on a S4 object
  if (bUseMixed)
  {
    mixed_model_return <- lmer1
    mixed_glht_return <- glmmht1
  } else {
    mixed_model_return <- NA
    mixed_glht_return <- NA
  }

  ret <- list(result = resultVector,
              bootstrap_mean = b1$t0,
              bootstrap_results = b1$t,
              bootstrap_interval = resultInterval,
              pvalue = resultPvalue,
              type = type,
              method = method,
              model = lm1,
              mixed_model = mixed_model_return,
              glht = glht1,
              mixed_glht = mixed_glht_return
  )

  class(ret) <- "impactResult"
  return(ret)
}
