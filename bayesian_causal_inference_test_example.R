require(mvtnorm)

sigma <- 3
rho <- 0.8
len <- 8
Sigma <- matrix(NA, nrow = len, ncol = len)
Sigma[1,] <- rho^(0:(len-1))
Sigma[len,] <- rho^((len-1):0)
for (i in 2:(len-1))
{
  Sigma[i,] <- rho^c((i-1):0, 1:(len-i))
}
Sigma <- sigma^2/(1-rho^2)*Sigma

Y <- rmvnorm(1, mean = rep(2,len), sigma = Sigma)

acf(c(Y))

Qtest <- zapsmall(solve(Sigma))

Q <- matrix(0, nrow = len, ncol = len)
diag(Q) <- c(1, rep(1+rho^2, len-2), 1)
for (i in 1:(len-1))
{
  Q[i,i+1] <- -rho
  Q[i+1,i] <- -rho
}
Q <- Q / sigma^2

all.equal(Q, Qtest)

Sigmatest <- solve(Q)
all.equal(Sigma, Sigmatest)

chol(Q)*t(col(Q))


require(ggplot2)
require(plyr)

createAR1PrecMat <- function(sigma, rho, len)
{
  Q <- matrix(0, nrow = len, ncol = len)
  diag(Q) <- c(1, rep(1+rho^2, len-2), 1)
  for (i in 1:(len-1))
  {
    Q[i,i+1] <- -rho
    Q[i+1,i] <- -rho
  }
  Q <- Q / sigma^2
  return(Q)
}

createAR1CovMat <- function(sigma, rho, len)
{
  Q <- createAR1PrecMat(sigma, rho, len)
  #solve(Q)
  chol2inv(chol(Q))
}

https://kurser.math.su.se/pluginfile.php/19448/mod_folder/content/0/2018/2018_1_report.pdf?forcedownload=1

sigma <- 1
rho <- 0.8
lenpre <- 4
lenpost <- 5
set.seed(1345)
y_0_ctrl_pre <- rmvnorm(10, mean = c(rep(2, lenpre), rep(9, lenpost)), sigma = createAR1CovMat(sigma, rho, lenpre+lenpost))
y_0_test_pre <- rmvnorm(12, mean = c(rep(3, lenpre), rep(2, lenpost)), sigma = createAR1CovMat(sigma, rho, lenpre+lenpost))
y_0_ctrl_post <- rmvnorm(10, mean = rep(1, lenpost), sigma = createAR1CovMat(sigma, rho, lenpost))
y_1_test_post <- rmvnorm(12, mean = rep(10, lenpost), sigma = createAR1CovMat(sigma, rho, lenpost))

df <- data.frame(test_ctrl = c(rep("ctrl",10*4 + 10*5), rep("test",12*4 + 12*5)),
                 time = c(rep(1:9, each=10), rep(1:9, each = 12)),
                 y = c(c(y_0_ctrl_pre[,1:lenpre]), c(y_0_ctrl_post), c(y_0_test_pre[,1:lenpre]), c(y_1_test_post)),
                 id = c(rep(LETTERS[1:10], times = lenpre+lenpost), rep(LETTERS[11:22], times = lenpre+lenpost)))
dfmean <- ddply(df, .(test_ctrl, time), summarize, m = mean(y))
dfmean
dfmissing <- data.frame(test_ctrl = rep(c("test", "ctrl"), each = lenpre+lenpost), 
                        time = rep(1:9, times=2), 
                        m = c(apply(y_0_test_pre, 2, mean), apply(y_0_ctrl_pre, 2, mean)))

ggplot(df, aes(x = time, y = y, group = id, col = test_ctrl)) + geom_line()
  
ggplot(df, aes(x = time, y = y, group = test_ctrl, col = test_ctrl)) + 
  geom_point() +
  geom_line(aes(x = time, y = m), data = dfmissing, col = "black", lty = 2) +
  geom_line(aes(x = time, y = m, group = test_ctrl, col = test_ctrl), data = dfmean)

#########

temp <- rmvnorm(200, mean = rep(1, 100), sigma = createAR1CovMat(1.5, 0.8, 100))

# rho-hat
mean(apply(temp, 1, function(x){
  m <- mean(x)
  len <- length(x)
  sum((x[1:(len-1)] - m)*(x[2:len] - m)) / sum((x-m)^2)
}))

mean(apply(temp, 1, mean))

#############

set.seed(1345)
len <- 4
y_0_ctrl_pre <- rmvnorm(10, mean = rep(2, len), sigma = createAR1CovMat(sigma, rho, len))
set.seed(1345)
len <- 4
y_0_ctrl_pre <- 2 + rmvnorm(10, mean = rep(0, len), sigma = createAR1CovMat(sigma, rho, len))


prospective vs. retrospective (observational)
assigned randomized (stratified) test-control vs. assigned but not completely random test-control vs. self selected treated-non_treated
in-time vs. pre-post
cross-sectional (point in time) vs. longitudinal (across time)
with and without propensity scores


