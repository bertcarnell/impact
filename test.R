# group means, step change
params <- list()
params$n <- 10
params$m <- 20
params$mu1_pre <- rep(1, params$n)
params$mu2_pre <- rep(2, params$m)
params$sigma1_pre <- rep(0.01, params$n)
params$sigma2_pre <- rep(0.01, params$m)
params$mu1_post <- rep(3, params$n)
params$mu2_post <- rep(4, params$m)
params$sigma1_post <- rep(0.01, params$n)
params$sigma2_post <- rep(0.01, params$m)
params$N_pre <- 3
params$N_post <- 4

createSample <- function(m, s, n)
{
  temp <- t(mapply(function(m, s, n){
    rnorm(n, m, s)
  }, m = m, s = s, n = n, USE.NAMES = FALSE))
  dimnames(temp) <- list(NULL, paste("time", 1:n, sep = ""))
  return(temp)
}

experiment <- function(dummy, params)
{
  X1_pre <- createSample(params$mu1_pre, params$sigma1_pre, params$N_pre)
  X2_pre <- createSample(params$mu2_pre, params$sigma2_pre, params$N_pre)
  X1_post <- createSample(params$mu1_post, params$sigma1_post, params$N_post)
  X2_post <- createSample(params$mu2_post, params$sigma2_post, params$N_post)
  
  m1_avg_pre <- mean(apply(X1_pre, 2, mean))
  m2_avg_pre <- mean(apply(X2_pre, 2, mean))
  m1_avg_post <- mean(apply(X1_post, 2, mean))
  m2_avg_post <- mean(apply(X2_post, 2, mean))
  v1_avg_pre <- mean(apply(X1_pre, 2, var))
  v2_avg_pre <- mean(apply(X2_pre, 2, var))
  v1_avg_post <- mean(apply(X1_post, 2, var))
  v2_avg_post <- mean(apply(X2_post, 2, var))
  
  test_stat <- (m1_avg_post - m2_avg_post) - (m1_avg_pre - m2_avg_pre)
  #var_test_stat <- v1_avg_pre / params$n + v2_avg_pre / params$m + v1_avg_post / params$n + v2_avg_post / params$m
  var_test_stat <- 1 / params$N_pre * v1_avg_pre / params$n + 
                   1 / params$N_pre * v2_avg_pre / params$m + 
                   1 / params$N_post * v1_avg_post / params$n + 
                   1 / params$N_post * v2_avg_post / params$m
  # Welch-Satterthwaite
  #degree_free = var_test_stat^2 / ((v1_avg_pre / params$n)^2 / (params$n - 1) +
  #                                 (v2_avg_pre / params$m)^2 / (params$m - 1) +
  #                                 (v1_avg_post / params$n)^2 / (params$n - 1) +
  #                                 (v2_avg_post / params$m)^2 / (params$m - 1))
  degree_free = var_test_stat^2 / ((1 / params$N_pre * v1_avg_pre / params$n)^2 / (params$n - 1) +
                                     (1 / params$N_pre * v2_avg_pre / params$m)^2 / (params$m - 1) +
                                     (1 / params$N_post * v1_avg_post / params$n)^2 / (params$n - 1) +
                                     (1 / params$N_post * v2_avg_post / params$m)^2 / (params$m - 1))
  pt(test_stat / sqrt(var_test_stat), df = degree_free)
}

set.seed(1976)
experiment(1, params)
res <- sapply(1:10000, experiment, params = params)
length(which(res < 0.05)) / 10000
hist(res)


boot_func <- function(d, i, params, X1_post, X2_post)
{
  ind1 <- i[i <= params$n]
  ind2 <- i[i > params$n]
  ind3 <- ind1
  ind4 <- ind2 - params$n

  m1_avg_pre <- mean(apply(d[ind1,], 2, mean))
  m2_avg_pre <- mean(apply(d[ind2,], 2, mean))
  m1_avg_post <- mean(apply(X1_post[ind3,], 2, mean))
  m2_avg_post <- mean(apply(X2_post[ind4,], 2, mean))
  
  test_stat <- (m1_avg_post - m2_avg_post) - (m1_avg_pre - m2_avg_pre)
  return(test_stat)
}

experiment <- function(params)
{
  X1_pre <- createSample(params$mu1_pre, params$sigma1_pre, params$N_pre)
  X2_pre <- createSample(params$mu2_pre, params$sigma2_pre, params$N_pre)
  X1_post <- createSample(params$mu1_post, params$sigma1_post, params$N_post)
  X2_post <- createSample(params$mu2_post, params$sigma2_post, params$N_post)

  m1_avg_pre <- mean(apply(X1_pre, 2, mean))
  m2_avg_pre <- mean(apply(X2_pre, 2, mean))
  m1_avg_post <- mean(apply(X1_post, 2, mean))
  m2_avg_post <- mean(apply(X2_post, 2, mean))
  
  test_stat <- (m1_avg_post - m2_avg_post) - (m1_avg_pre - m2_avg_pre)

  df <- as.data.frame(rbind(X1_pre, X2_pre))
  df$strata <- c(rep(1L, params$n), rep(2L, params$m))
  b1 <- boot::boot(df, boot_func, R=1000, stype = "i", strata = df$strata, 
                   params = params, X1_post = X1_post, X2_post = X2_post)

  return(c(result = test_stat, boot_mean = b1$t0, quantile(b1$t, probs = c(0.025, 0.975))))
}

experiment(params)



