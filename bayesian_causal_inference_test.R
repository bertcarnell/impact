require(rstan)

set.seed(1977)
# simulate
N <- 100
x1 <- rnorm(N, 0, 1)
x2 <- rnorm(N, 1, 2)
e <- rnorm(N, 0, 2)
effect <- 30
y_0 <- 1 + 3*x1 + 2*x2 + e
y_1 <- 1 + 3*x1 + 2*x2 + effect + e
w <- rbinom(N, size = 1, prob = 0.5)
y <- w*y_1 + (1-w)*y_0
X <- as.matrix(cbind(rep(1,N), x1, x2))
# separate
y_1_obs <- y[w == 1]
y_0_obs <- y[w == 0]
X_1_obs <- X[w == 1,]
X_0_obs <- X[w == 0,]
N_1_obs <- sum(w == 1)
N_0_obs <- sum(w == 0)
K <- ncol(X)
beta_1_prior_mean <- c(1, 1, 1)
beta_1_prior_sd <- c(10, 10, 10)
beta_0_prior_mean <- c(1, 1, 1)
beta_0_prior_sd <- c(10, 10, 10)
sigma_1_prior_alpha <- 0.01
sigma_1_prior_beta <- 0.01
sigma_0_prior_alpha <- 0.01
sigma_0_prior_beta <- 0.01

test_data <- list(
  N = N,
  K = K,
  y_1_obs = y_1_obs,
  y_0_obs = y_0_obs,
  X_1_obs = X_1_obs,
  X_0_obs = X_0_obs,
  N_1_obs = N_1_obs,
  N_0_obs = N_0_obs,
  beta_1_prior_mean = beta_1_prior_mean,
  beta_1_prior_sd = beta_1_prior_sd,
  beta_0_prior_mean = beta_0_prior_mean,
  beta_0_prior_sd = beta_0_prior_sd,
  sigma_1_prior_alpha = sigma_1_prior_alpha,
  sigma_1_prior_beta = sigma_1_prior_beta,
  sigma_0_prior_alpha = sigma_0_prior_alpha,
  sigma_0_prior_beta = sigma_0_prior_beta
)

stan_char_string <- "
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> N_1_obs;
  int<lower=0> N_0_obs;
  vector[N_1_obs] y_1_obs;
  vector[N_0_obs] y_0_obs;
  matrix[N_1_obs,K] X_1_obs;
  matrix[N_0_obs,K] X_0_obs;
  vector[K] beta_1_prior_mean;
  vector[K] beta_1_prior_sd;
  vector[K] beta_0_prior_mean;
  vector[K] beta_0_prior_sd;
  real<lower=0> sigma_1_prior_alpha;
  real<lower=0> sigma_1_prior_beta;
  real<lower=0> sigma_0_prior_alpha;
  real<lower=0> sigma_0_prior_beta;
}
parameters {
  vector[K] beta_1;
  vector[K] beta_0;
  real sigma_1;
  real sigma_0;
  vector[N_0_obs] y_1_miss;
  vector[N_1_obs] y_0_miss;
}
transformed parameters {
  vector[N] y_1;
  vector[N] y_0;
  matrix[N,K] X_1;
  matrix[N,K] X_0;
  vector[N] mu_1;
  vector[N] mu_0;
  for (i in 1:N_1_obs){
    y_1[i] = y_1_obs[i];
    X_1[i,] = X_1_obs[i,];
  }
  for (i in 1:N_0_obs){
    y_1[N_1_obs + i] = y_1_miss[i];
    X_1[N_1_obs + i,] = X_0_obs[i,];
  }
  mu_1 = X_1 * beta_1;
  for (i in 1:N_0_obs){
    y_0[i] = y_0_obs[i];
    X_0[i,] = X_0_obs[i,];
  }
  for (i in 1:N_1_obs){
    y_0[N_0_obs + i] = y_0_miss[i];
    X_0[N_0_obs + i,] = X_1_obs[i,];
  }
  mu_0 = X_0 * beta_0;
}
model {
  beta_1 ~ normal(beta_1_prior_mean, beta_1_prior_sd);
  beta_0 ~ normal(beta_0_prior_mean, beta_0_prior_sd);
  sigma_1 ~ inv_gamma(sigma_1_prior_alpha, sigma_1_prior_beta);
  sigma_0 ~ inv_gamma(sigma_0_prior_alpha, sigma_0_prior_beta);

  y_1 ~ normal(mu_1, sigma_1);
  y_0 ~ normal(mu_0, sigma_0);
}"

#Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")

fit1 <- stan(model_code = stan_char_string, data = test_data, 
             pars = c("beta_1","beta_0","sigma_1","sigma_0","y_1","y_0"), 
             iter = 2000, warmup = 1000, 
             chains = 2, cores = 2)

print(fit1, pars = c("beta_1","beta_0","sigma_1","sigma_0"))
plot(fit1, pars = c("beta_1","beta_0","sigma_1","sigma_0"), ci_level = 0.5, 
     outer_level = 0.95)
plot(fit1, plotfun="trace", pars = c("beta_1","beta_0","sigma_1","sigma_0"))

temp <- as.array(fit1)
ind1 <- which(grepl("y_1", dimnames(temp)$parameters))
ind0 <- which(grepl("y_0", dimnames(temp)$parameters))
# construct the hypothesis metric
tau <- temp[,,ind1] - temp[,,ind0]
hist(tau)
# take the mean over the chains and interations assuming we have covered the population of X
tau_m <- apply(tau, 3, mean)
mean(tau_m)
quantile(tau_m, probs=c(0.025, .975))


# for the print method...
print(fit1, pars = c("beta_1"))
mean(temp[,,1])
sqrt(var(c(temp[,,1]))/2/1000)
sd(c(temp[,,1]))
quantile(c(temp[,,1]), probs = c(0.025, 0.975))

################################################################################

test_data$rho <- 0.1
test_data$X <- X
test_data$w <- w
test_data$y <- y
test_data$i_1_obs <- which(w == 1)
test_data$i_0_obs <- which(w == 0)

stan_char_string <- "
data {
  int<lower=1> N;
  int<lower=1> N_0_obs;
  int<lower=1> N_1_obs;
  int<lower=1> K;
  int<lower=1, upper=N> i_1_obs[N_1_obs];
  int<lower=1, upper=N> i_0_obs[N_0_obs];
  vector[N] w;
  vector[N] y;
  matrix[N,K] X;
  real<lower=0> rho; //////////////
  vector[K] beta_1_prior_mean;
  vector[K] beta_1_prior_sd;
  vector[K] beta_0_prior_mean;
  vector[K] beta_0_prior_sd;
  real<lower=0> sigma_1_prior_alpha;
  real<lower=0> sigma_1_prior_beta;
  real<lower=0> sigma_0_prior_alpha;
  real<lower=0> sigma_0_prior_beta;
}
parameters {
  vector[K] beta_0;
  vector[K] beta_1;
  real sigma_1;
  real sigma_0;
  vector[N_1_obs] y_0_miss;
  vector[N_0_obs] y_1_miss;
}
transformed parameters {
  matrix[N,2] y_mod;
  matrix[N,2] mu;
  matrix[K,2] beta;
  cov_matrix[2] sigma;

  beta[,1] = beta_0;
  beta[,2] = beta_1;
  sigma[1,1] = square(sigma_0);
  sigma[2,2] = square(sigma_1);
  sigma[1,2] = sigma_0 * sigma_1 * rho;
  sigma[2,1] = sigma_1 * sigma_0 * rho;
  
  mu = X * beta;
  y_mod[,1] = y .* (1-w);
  y_mod[,2] = y .* w;
  for (i in 1:N_0_obs){
    y_mod[i_0_obs[i], 2] = y_1_miss[i];
  }
  for (i in 1:N_1_obs){
    y_mod[i_1_obs[i], 1] = y_0_miss[i];
  }
}
model {
  beta_1 ~ normal(beta_1_prior_mean, beta_1_prior_sd);
  beta_0 ~ normal(beta_0_prior_mean, beta_0_prior_sd);
  sigma_1 ~ inv_gamma(sigma_1_prior_alpha, sigma_1_prior_beta);
  sigma_0 ~ inv_gamma(sigma_0_prior_alpha, sigma_0_prior_beta);
  // constant improper prior on y_1_miss and y_0_miss

  for (i in 1:N)
    y_mod[i,] ~ multi_normal(mu[i,], sigma);
}"

#Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")

fit1 <- stan(model_code = stan_char_string, data = test_data, 
             pars = c("beta_1","beta_0","sigma_1","sigma_0","y_mod"), 
             iter = 2000, warmup = 1000, 
             chains = 2, cores = 2)

print(fit1, pars = c("beta_1","beta_0","sigma_1","sigma_0"))
plot(fit1, pars = c("beta_1","beta_0","sigma_1","sigma_0"), ci_level = 0.5, 
     outer_level = 0.95)
plot(fit1, plotfun="trace", pars = c("beta_1","beta_0","sigma_1","sigma_0"))

temp <- as.array(fit1)
ind0 <- which(grepl("y_mod[[].*[,]1[]]", dimnames(temp)$parameters))
ind1 <- which(grepl("y_mod[[].*[,]2[]]", dimnames(temp)$parameters))
hist(y_0[w==0])
hist(temp[,,ind0[w==0]]) # get y0 observed
hist(temp[,,ind0[w==1]]) # get y0 missing
hist(y_1[w==1])
hist(temp[,,ind1[w==1]]) # get y0 observed
hist(temp[,,ind1[w==0]]) # get y0 missing

# construct the almost-PATE hypothesis metric
tau_apate <- temp[,,ind1] - temp[,,ind0]
hist(tau_apate)
# take the mean over the chains and interations assuming we have covered the population of X
tau_m <- apply(tau_apate, 3, mean)
mean(tau_m)
quantile(tau_m, probs=c(0.025, .975))

# constuct the SATE
mean((y - apply(temp[,,ind0], 3, mean))*w + (apply(temp[,,ind1], 3, mean) - y)*(1-w))

#
y_1_pred <- test_data$X %*% apply(temp[,,1:3], 3, mean)
y_0_pred <- test_data$X %*% apply(temp[,,4:6], 3, mean)
y_pred <- y_0_pred*(1-w) + y_1_pred*w

df <- data.frame(y = y,
                 x1 = x1,
                 x2 = x2,
                 resid = y - y_pred,
                 y_1_pred = y_1_pred,
                 y_0_pred = y_0_pred,
                 w = factor(w))

df_x1 <- data.frame(x1 = seq(-3, 3, length = N))
df_x1$y1 <- cbind(rep(1,N), 
                  seq(-3, 3, length = N), 
                  mean(df$x2)) %*% apply(temp[,,1:3], 3, mean)
df_x1$y0 <- cbind(rep(1,N), 
                  seq(-3, 3, length = N), 
                  mean(df$x2)) %*% apply(temp[,,4:6], 3, mean)

df_x2 <- data.frame(x2 = seq(-3.5, 5.5, length = N))
df_x2$y1 <- cbind(rep(1,N), 
                  mean(df$x1), 
                  seq(-3.5, 5.5, length = N)) %*% apply(temp[,,1:3], 3, mean)
df_x2$y0 <- cbind(rep(1,N), 
                  mean(df$x1), 
                  seq(-3.5, 5.5, length = N)) %*% apply(temp[,,4:6], 3, mean)

ggplot(df, aes(x = y, y = resid, group = w, col = w)) + 
  geom_point()
ggplot(df, aes(x = x1, y = y, group = w, col = w)) + 
  geom_point() +
  geom_line(aes(x = x1, y = y1), df_x1, col = "black") +
  geom_line(aes(x = x1, y = y0), df_x1, col = "black")
ggplot(df, aes(x = x2, y = y, group = w, col = w)) + 
  geom_point() +
  geom_line(aes(x = x2, y = y1), df_x2, col = "black") +
  geom_line(aes(x = x2, y = y0), df_x2, col = "black")
ggplot(df, aes(sample = resid)) + 
  geom_qq(distribution = stats::qnorm) + 
  geom_qq_line(distribution = stats::qnorm)

################################################################################

set.seed(1977)
# simulate
N <- 100
x1 <- rnorm(N, 0, 1)
x2 <- rnorm(N, 1, 2)
e <- rnorm(N, 0, 2)
effect <- 5
y_0 <- round(plogis(-2 + 3*x1 + 2*x2 + e))
y_1 <- round(plogis(-2 + 3*x1 + 2*x2 + effect + e))
w <- rbinom(N, size = 1, prob = 0.5)
y <- w*y_1 + (1-w)*y_0
X <- as.matrix(cbind(rep(1,N), x1, x2))
# separate
y_1_obs <- y[w == 1]
y_0_obs <- y[w == 0]
X_1_obs <- X[w == 1,]
X_0_obs <- X[w == 0,]
N_1_obs <- sum(w == 1)
N_0_obs <- sum(w == 0)
K <- ncol(X)
beta_1_prior_mean <- c(1, 1, 1)
beta_1_prior_sd <- c(10, 10, 10)
beta_0_prior_mean <- c(1, 1, 1)
beta_0_prior_sd <- c(10, 10, 10)

test_data <- list(
  N = N,
  K = K,
  y_1_obs = y_1_obs,
  y_0_obs = y_0_obs,
  X_1_obs = X_1_obs,
  X_0_obs = X_0_obs,
  N_1_obs = N_1_obs,
  N_0_obs = N_0_obs,
  beta_1_prior_mean = beta_1_prior_mean,
  beta_1_prior_sd = beta_1_prior_sd,
  beta_0_prior_mean = beta_0_prior_mean,
  beta_0_prior_sd = beta_0_prior_sd
)
test_data$rho <- 0.1
test_data$X <- X
test_data$w <- w
test_data$y <- as.integer(y)
test_data$i_1_obs <- which(w == 1)
test_data$i_0_obs <- which(w == 0)
test_data$y_1 <- as.integer(y*w)
test_data$y_0 <- as.integer(y*(1-w))

stan_char_string <- "
data {
  int<lower=1> N;
  int<lower=1> N_0_obs;
  int<lower=1> N_1_obs;
  int<lower=1> K;
  int y_1_obs[N_1_obs];
  int y_0_obs[N_0_obs];
  matrix[N_1_obs,K] X_1_obs;
  matrix[N_0_obs,K] X_0_obs;
  vector[K] beta_1_prior_mean;
  vector[K] beta_1_prior_sd;
  vector[K] beta_0_prior_mean;
  vector[K] beta_0_prior_sd;
}
parameters {
  vector[K] beta_0;
  vector[K] beta_1;
}
transformed parameters {
  vector[N_1_obs] mu_1;
  vector[N_0_obs] mu_0;

  mu_1 = X_1_obs * beta_1;
  mu_0 = X_0_obs * beta_0;
}
model {
  beta_1 ~ normal(beta_1_prior_mean, beta_1_prior_sd);
  beta_0 ~ normal(beta_0_prior_mean, beta_0_prior_sd);
  // constant improper prior on y_1_miss and y_0_miss

  y_0_obs ~ bernoulli_logit(mu_0);
  y_1_obs ~ bernoulli_logit(mu_1);
}
generated quantities {
  real y_0_miss[N_1_obs];
  real y_1_miss[N_0_obs];
  real y_0[N_0_obs];
  real y_1[N_1_obs];
  real mu_0_r;
  real mu_1_r;

  for (i in 1:N_1_obs)
  {
    mu_0_r = X_1_obs[i,] * beta_0;
    y_0_miss[i] = inv_logit(mu_0_r);
    mu_1_r = X_1_obs[i,] * beta_1;
    y_1[i] = inv_logit(mu_1_r);
  }
  for (i in 1:N_0_obs)
  {
    mu_1_r = X_0_obs[i,] * beta_1;
    y_1_miss[i] = inv_logit(mu_1_r);
    mu_0_r = X_0_obs[i,] * beta_0;
    y_0[i] = inv_logit(mu_0_r);
  }
}"

fit1 <- stan(model_code = stan_char_string, data = test_data, 
             pars = c("beta_1","beta_0", "y_0_miss", "y_1_miss", "y_0", "y_1"), 
             iter = 2000, warmup = 1000, 
             chains = 2, cores = 2)

print(fit1, pars = c("beta_1","beta_0"))

temp <- as.array(fit1)
ind0miss <- which(grepl("y_0_miss", dimnames(temp)$parameters))
ind1miss <- which(grepl("y_1_miss", dimnames(temp)$parameters))
ind0 <- which(grepl("y_0[[]", dimnames(temp)$parameters))
ind1 <- which(grepl("y_1[[]", dimnames(temp)$parameters))

# construct the almost-PATE hypothesis metric
tau_apate_a <- temp[,,ind1] - temp[,,ind0miss]
tau_apate_b <- temp[,,ind1miss] - temp[,,ind0]
hist(c(tau_apate_a, tau_apate_b))
# take the mean over the chains and interations assuming we have covered the population of X
tau_m <- c(apply(tau_apate_a, 3, mean), apply(tau_apate_b, 3, mean))
mean(tau_m)
quantile(tau_m, probs=c(0.025, .975))

# constuct the SATE
mean(c((y_1_obs - apply(temp[,,ind0miss], 3, mean)), (apply(temp[,,ind1miss], 3, mean) - y_0_obs)))
