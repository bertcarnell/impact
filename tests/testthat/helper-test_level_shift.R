# Copyright 2019 Robert Carnell

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

  if (n > 1)
  {
    dimnames(temp) <- list(NULL, paste("time", 1:n, sep = ""))
  } else
  {
    temp <- matrix(temp, ncol = 1)
    dimnames(temp) <- list(NULL, "time1")
  }
  return(temp)
}

create_dat <- function(params, X1_pre, X2_pre, X1_post, X2_post)
{
  data.frame(pre_post = c(rep("pre", params$n*params$N_pre),
                          rep("pre", params$m*params$N_pre),
                          rep("post", params$n*params$N_post),
                          rep("post", params$m*params$N_post)),
             time = c(rep(as.character(1:params$N_pre), each = params$n),
                      rep(as.character(1:params$N_pre), each = params$m),
                      rep(as.character((params$N_pre + 1):(params$N_pre + params$N_post)), each = params$n),
                      rep(as.character((params$N_pre + 1):(params$N_pre + params$N_post)), each = params$m)),
             id = c(rep(as.character(1:params$n), times = params$N_pre),
                    rep(as.character((params$n + 1):(params$n + params$m)), times = params$N_pre),
                    rep(as.character(1:params$n), times = params$N_post),
                    rep(as.character((params$n + 1):(params$n + params$m)), times = params$N_post)),
             trt = c(rep("ctrl", params$n*params$N_pre),
                     rep("test", params$m*params$N_pre),
                     rep("ctrl", params$n*params$N_post),
                     rep("test", params$m*params$N_post)),
             val = c(c(X1_pre), c(X2_pre), c(X1_post), c(X2_post)))
}
