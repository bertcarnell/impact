require(wfe)

### NOTE: this example illustrates the use of wfe function with randomly
### generated panel data with arbitrary number of units and time.

## generate panel data with number of units = N, number of time = Time
N <- 10 # number of distinct units
Time <- 15 # number of distinct time

## treatment effect
beta <- 1

## generate treatment variable
treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
## make sure at least one observation is treated for each unit
while ((sum(apply(treat, 2, mean) == 0) > 0) | (sum(apply(treat, 2, mean) == 1) > 0) |
       (sum(apply(treat, 1, mean) == 0) > 0) | (sum(apply(treat, 1, mean) == 1) > 0)) {
  treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
}
treat.vec <- c(treat)

## unit fixed effects
alphai <- rnorm(N, mean = apply(treat, 2, mean))

## geneate two random covariates
x1 <- matrix(rnorm(N*Time, 0.5,1), ncol=N)
x2 <- matrix(rbeta(N*Time, 5,1), ncol=N)
x1.vec <- c(x1)
x2.vec <- c(x2)
## generate outcome variable
y <- matrix(NA, ncol = N, nrow = Time)
for (i in 1:N) {
  y[, i] <- alphai[i] + treat[, i] + x1[,i] + x2[,i] + rnorm(Time)
}
y.vec <- c(y)

## generate unit and time index
unit.index <- rep(1:N, each = Time)
time.index <- rep(1:Time, N)

Data.str <- as.data.frame(cbind(y.vec, treat.vec, unit.index, x1.vec, x2.vec))
colnames(Data.str) <- c("y", "tr", "strata.id", "x1", "x2")

Data.obs <- as.data.frame(cbind(y.vec, treat.vec, unit.index, time.index, x1.vec, x2.vec))
colnames(Data.obs) <- c("y", "tr", "unit", "time", "x1", "x2")

############################################################
# Example 1: Stratified Randomized Experiments
############################################################

## run the weighted fixed effect regression with strata fixed effect.
## Note: the quantity of interest is Average Treatment Effect ("ate")
## and the standard errors allow heteroskedasticity and arbitrary
## autocorrelation.


### Average Treatment Effect
mod.ate <- wfe(y ~ tr + x1 + x2, data = Data.str, treat = "tr",
               unit.index = "strata.id", method = "unit",
               qoi = "ate", hetero.se=TRUE, auto.se=TRUE)
## summarize the results
summary(mod.ate)

### Average Treatment Effect for the Treated
mod.att <- wfe(y~ tr+x1+x2, data = Data.str, treat = "tr",
               unit.index = "strata.id", method = "unit",
               qoi = "att", hetero.se=TRUE, auto.se=TRUE)
## summarize the results
summary(mod.att)


############################################################
# Example 2: Observational Studies with Unit Fixed-effects
############################################################

## run the weighted fixed effect regression with unit fixed effect.
## Note: the quantity of interest is Average Treatment Effect ("ate")
## and the standard errors allow heteroskedasticity and arbitrary
## autocorrelation.

mod.obs <- wfe(y~ tr+x1+x2, data = Data.obs, treat = "tr",
               unit.index = "unit", time.index = "time", method = "unit",
               qoi = "ate", hetero.se=TRUE, auto.se=TRUE,
               White = TRUE, White.alpha = 0.05)

## summarize the results
summary(mod.obs)

## extracting weigths
summary(mod.obs)$W


## Not run: 
###################################################################
# Example 3: Observational Studies with differences-in-differences
###################################################################

## run difference-in-differences estimator.
## Note: the quantity of interest is Average Treatment Effect ("ate")
## and the standard errors allow heteroskedasticity and arbitrary
## autocorrelation.

mod.did <- wfe(y~ tr+x1+x2, data = Data.obs, treat = "tr",
               unit.index = "unit", time.index = "time", method = "unit",
               qoi = "ate", estimator ="did", hetero.se=TRUE, auto.se=TRUE,
               White = TRUE, White.alpha = 0.05, verbose = TRUE)

## summarize the results
summary(mod.did)

## extracting weigths
summary(mod.did)$W

#########################################################################
# Example 4: DID with Matching on Pre-treatment Outcomes
#########################################################################

## implements matching on pre-treatment outcomes where the maximum
## deviation is specified as 0.5 

mod.Mdid <- wfe(y~ tr+x1+x2, data = Data.obs, treat = "tr",
                unit.index = "unit", time.index = "time", method = "unit",
                qoi = "ate", estimator ="Mdid", hetero.se=TRUE, auto.se=TRUE,
                White = TRUE, White.alpha = 0.05, maxdev.did = 0.5, verbose = TRUE)

## summarize the results
summary(mod.Mdid)

## Note: setting the maximum deviation to infinity (or any value
## bigger than the maximum pair-wise difference in the outcome) will
## return the same result as Example 3.

dev <- 1000+max(Data.obs$y)-min(Data.obs$y)
mod.did2 <- wfe(y~ tr+x1+x2, data = Data.obs, treat = "tr",
                unit.index = "unit", time.index = "time", method = "unit",
                qoi = "ate", estimator ="Mdid", hetero.se=TRUE, auto.se=TRUE,
                White = TRUE, White.alpha = 0.05, maxdev.did = dev, verbose = TRUE)

## summarize the results
summary(mod.did2)
mod.did2$coef[1] ==  mod.did$coef[1]


################################################################################

N <- 10 # number of distinct units
Time <- 15 # number of distinct time

## generate treatment variable
treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
## make sure at least one observation is treated for each unit
while ((sum(apply(treat, 2, mean) == 0) > 0) | (sum(apply(treat, 2, mean) == 1) > 0) |
       (sum(apply(treat, 1, mean) == 0) > 0) | (sum(apply(treat, 1, mean) == 1) > 0)) {
  treat <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
}
treat.vec <- c(treat)

## unit fixed effects
alphai <- rnorm(N, mean = apply(treat, 2, mean))

## geneate two random covariates
x1 <- matrix(rnorm(N*Time, 0.5,1), ncol=N)
x2 <- matrix(rbeta(N*Time, 5,1), ncol=N)
pscore <- matrix(runif(N*Time, 0,1), ncol=N)
x1.vec <- c(x1)
x2.vec <- c(x2)
pscore <- c(pscore)

## generate outcome variable
y <- matrix(NA, ncol = N, nrow = Time)
for (i in 1:N) {
  y[, i] <- alphai[i] + treat[, i] + x1[,i] + x2[,i] + rnorm(Time)
}
y.vec <- c(y)

## generate unit and time index
unit.index <- rep(1:N, each = Time)
time.index <- rep(1:Time, N)

Data.str <- as.data.frame(cbind(y.vec, treat.vec, unit.index, x1.vec, x2.vec))
colnames(Data.str) <- c("y", "tr", "strata.id", "x1", "x2")

Data.obs <- as.data.frame(cbind(y.vec, treat.vec, unit.index, time.index, x1.vec, x2.vec, pscore))
colnames(Data.obs) <- c("y", "tr", "unit", "time", "x1", "x2", "pscore")


############################################################
# Example 1: Stratified Randomized Experiments
############################################################

## run the weighted fixed effect regression with strata fixed effect.
## Note: the quantity of interest is Average Treatment Effect ("ate")
## and the standard errors allow heteroskedasticity and arbitrary
## autocorrelation.


### Average Treatment Effect
ps.ate <- pwfe(~ x1+x2, treat = "tr", outcome = "y", data = Data.str, 
               unit.index = "strata.id", method = "unit", within.unit = TRUE,
               qoi = "ate", hetero.se=TRUE, auto.se=TRUE)
## summarize the results
summary(ps.ate)

### Average Treatment Effect for the Treated
ps.att <- pwfe(~ x1+x2, treat = "tr", outcome = "y", data = Data.str, 
               unit.index = "strata.id", method = "unit", within.unit = TRUE,
               qoi = "att", hetero.se=TRUE, auto.se=TRUE)
## summarize the results
summary(ps.att)


############################################################
# Example 2: Observational Studies with Unit Fixed-effects
############################################################

## run the weighted fixed effect regression with unit fixed effect.
## Note: the quantity of interest is Average Treatment Effect ("ate")
## and the standard errors allow heteroskedasticity and arbitrary
## autocorrelation.

### Average Treatment Effect
ps.obs <- pwfe(~ x1+x2, treat = "tr", outcome = "y", data = Data.obs, 
               unit.index = "unit", time.index = "time",
               method = "unit", within.unit = TRUE,
               qoi = "ate", hetero.se=TRUE, auto.se=TRUE)

## summarize the results
summary(ps.obs)

## extracting weigths
summary(ps.obs)$Weights

### Average Treatment Effect with First-difference

ps.fd <- pwfe(~ x1+x2, treat = "tr", outcome = "y", data = Data.obs,
              unit.index = "unit", time.index = "time",
              method = "unit", within.unit = TRUE,
              qoi = "ate", estimator = "fd", hetero.se=TRUE, auto.se=TRUE)

## summarize the results
summary(ps.fd)


############################################################
# Example 3: Estimation with pre-specified propensity score
############################################################

### Average Treatment Effect with Pre-specified Propensity Scores

mod.ps <- pwfe(treat = "tr", outcome = "y", data = Data.obs, pscore = "pscore",
               unit.index = "unit", time.index = "time",
               method = "unit", within.unit = TRUE,
               qoi = "ate", hetero.se=TRUE, auto.se=TRUE)

## summarize the results
summary(mod.ps)

## End(Not run)


https://diff.healthpolicydatascience.org/
  https://bookdown.org/ccolonescu/RPoE4/indvars.html#the-difference-in-differences-estimator


require(did)

data(mpdta)

## with covariates
out1 <- mp.spatt(lemp ~ treat, xformla=~lpop, data=mpdta,
                 panel=TRUE, first.treat.name="first.treat",
                 idname="countyreal", tname="year",
                 bstrap=FALSE, se=TRUE, cband=FALSE)
## summarize the group-time average treatment effects
summary(out1)
## summarize the aggregated treatment effect parameters
summary(out1$aggte)

## without any covariates
out2 <- mp.spatt(lemp ~ treat, xformla=NULL, data=mpdta,
                 panel=TRUE, first.treat.name="first.treat",
                 idname="countyreal", tname="year",
                 bstrap=FALSE, se=TRUE, cband=FALSE)
summary(out2)

#########################

http://tabletopwhale.com/
  https://www.r-bloggers.com/the-popularity-of-point-and-click-guis-for-r/
  http://r4stats.com/articles/software-reviews/
  https://www.blueskystatistics.com/Default.asp
https://www.jamovi.org/jmv/
  https://jasp-stats.org/
  
  
  ######################

http://davidcard.berkeley.edu/data_sets.html


  