# Copyright 2019 Robert Carnell

require(lm4)
require(multcomp)
require(stats)
require(boot)
require(plyr)

.defaultFormula <- formula(val ~ pre_post * trt * numtime)

.boot_func_group_mean_model <- function(boot_dat, i, full_dat, method,
                                        val_name, pre_name, post_name,
                                        test_name, ctrl_name, test_formula,
                                        test_time)
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

  # bootstrap the linear model
  lm1_boot <- stats::lm(test_formula, data = int_dat)
  lm2_boot <- stats::lm(update(test_formula, val ~ . - pre_post:trt:numtime), data = int_dat)
  # don't need to bootstrap the mixed model because the parameter estimates are the same.
  #   The variance is what is different
  ind_lm <- c(which(names(coef(lm1_boot)) == "pre_postpost:trttest"),
              which(names(coef(lm1_boot)) == "pre_postpost:trttest:numtime"))
  #ind_lm <- which(names(coef(lm1_boot)) %in% c("pre_postpost:trttest",
  #                                     "pre_postpost:trttest:numtime"))
  ind_lm1 <- which(names(coef(lm1_boot)) == "pre_postpost:trttest:numtime")
  ind_lm2 <- which(names(coef(lm2_boot)) == "pre_postpost:trttest")

  # scale the parameters by the time and sumt them
  return(c("pre_postpost:trttest:numtime" = coef(lm1_boot)[ind_lm1],
           "pre_postpost:trttest+pre_postpost:trttest:numtime" = sum(coef(lm1_boot)[ind_lm]*c(1, test_time)),
           "pre_postpost:trttest" = coef(lm2_boot)[ind_lm2]))
}

test_response_change <- function(dat, type = "group", method = "diff",
                                 val_name = "val",
                                 test_name = "test", ctrl_name = "ctrl",
                                 pre_name = "pre", post_name = "post",
                                 test_formula = .defaultFormula,
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
  # test_formula = .defaultFormula

  validate_impact_data(dat)
  assertthat::assert_that(type %in% c("group", "1-1", "1-m"),
                          msg = "The type variable must be one of group, 1-1, or 1-m")
  assertthat::assert_that(method %in% c("diff", "ratio"),
                          msg = "The method variable must be either diff or ratio")

  dat$val <- dat[[val_name]]
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

  lm1 <- stats::lm(test_formula, data = dat)
  lm2 <- stats::lm(update(test_formula, val ~ . - pre_post:trt:numtime), data = dat)
  # add a random effect of id nested within treatment
  lmer1 <- lme4::lmer(update(test_formula, ~ . + (trt | id)), data = dat)
  K <- matrix(0, nrow = 2, ncol = length(coef(lm1)))
  K[1, names(coef(lm1)) == "pre_postpost:trttest:numtime"] <- 1 # not test time
  K[2, names(coef(lm1)) == "pre_postpost:trttest"] <- 1
  K[2, names(coef(lm1)) == "pre_postpost:trttest:numtime"] <- test_time
  K2 <- matrix(0, nrow = 1, ncol = length(coef(lm2)))
  K2[1, names(coef(lm2)) == "pre_postpost:trttest"] <- 1
  rownames(K) <- c("pre_postpost:trttest:numtime",
                   "pre_postpost:trttest+pre_postpost:trttest:numtime*time")
  rownames(K2) <- c("pre_postpost:trttest")
  glht1 <- multcomp::glht(lm1, linfct = K)
  glmmht1 <- multcomp::glht(lmer1, linfct = K)
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
  }

  # first test pre_post:trt:time
  #  if significant, then test pre_post:trt:time + pre_post:trt
  #  if not significant, then test pre_post:trt
  ret <- list(result = c(apply(glht1$linfct %*% matrix(glht1$coef, ncol = 1), 1, sum),
                         sum(glht2$linfct * glht2$coef)),
              bootstrap_mean = b1$t0,
              bootstrap_results = b1$t,
              bootstrap_interval = apply(b1$t, 2, stats::quantile, probs = c(0.025, 0.975)),
              pvalue = apply(b1$t, 2, function(x) length(which(x < 0)) / length(x)),
              type = type,
              method = method,
              model = lm1,
              mixed_model = lmer1,
              glht = glht1,
              mixed_glht = glmmht1
  )
  class(ret) <- "impactResult"
  return(ret)
}

