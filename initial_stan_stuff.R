library(rstan)

N <- nrow(mtcars)
K <- ncol(mtcars) - 1 + 1
ci_data <- list(
  N = nrow(mtcars),
  y = mtcars$mpg,
  K = ncol(mtcars),
  X = as.matrix(cbind(rep(1, N), mtcars[,-1]), dimnames = NULL),
  scale_beta = rep(1, K),
  sigma_inv_gamma_alpha = 0.01,
  sigma_inv_gamma_beta = 0.01
)

stan_string <- "
data {
  int N;
  vector[N] y;
  int K;
  matrix [N, K] X;
  vector[K] scale_beta;
  real<lower=0> sigma_inv_gamma_alpha;
  real<lower=0> sigma_inv_gamma_beta;
}
parameters {
  vector[K] beta;
  real sigma;
}
transformed parameters {
  vector[N] mu;
  mu = X * beta;
}
model {
  // priors
  beta ~ normal(0., scale_beta);
  sigma ~ inv_gamma(sigma_inv_gamma_alpha, sigma_inv_gamma_beta);
  // likelihood
  y ~ normal(mu, sigma);
}
"

fit1 <- stan(
  model_code = stan_string,  # Stan program
  data = ci_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4,              # number of cores (using 2 just for the vignette)
  refresh = 1000,          # show progress every 'refresh' iterations
  pars = c("sigma", "beta")
)

plot(fit1, plotfun = "trace", pars = c("sigma", "beta"), inc_warmup = TRUE)
plot(fit1, pars = c("sigma", "beta"), ci_level = 0.50, outer_level = 0.95)
print(fit1, pars = c("sigma", "beta"))

lm1 <- lm(mpg ~ ., data = mtcars)
summary(lm1)

fit1a <- as.array(fit1)

apply(fit1a, 3, mean)
coef(lm1)



################################################################################

set.seed(1976)

N <- 100
x1 <- rnorm(N)
x2 <- runif(N)
e <- rnorm(N, 0, 2)
w <- rbinom(N, size = 1, prob = 0.5)
y_0 <- 3 + 2*x1 + 5*x2 + e
y_1 <- 3 + 2*x1 + 5*x2 + 4 + e
y <- w*y_1 + (1-w)*y_0

ci_data <- list(
  N = N,
  K = 3,
  y = y,
  X = as.matrix(cbind(rep(1, N), x1, x2)),
  w = w,
  beta_prior_mean = c(1, 2, 3),
  beta_prior_sigma = c(10, 10, 10),
  sigma_prior_mean = 1,
  effect_prior_mean = 0,
  effect_prior_sigma = 10
)

stanfile <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N,K] X;
  vector[N] y;
  vector[N] w;
  vector[K] beta_prior_mean;
  vector[K] beta_prior_sigma;
  real sigma_prior_mean;
  real effect_prior_mean;
  real effect_prior_sigma;
}
parameters {
  vector[K] beta;
  real sigma;
  real effect;
}
transformed parameters {
  vector[N] mu;
  vector[N] z;
  mu = X * beta;
  z = y + effect * w;
}
model {
  beta ~ normal(beta_prior_mean, beta_prior_sigma);
  sigma ~ exponential(sigma_prior_mean);
  effect ~ normal(effect_prior_mean, effect_prior_sigma);
  z ~ normal(mu, sigma);
}"

library(rstan)
fit1 <- stan(
  model_code = stanfile,  # Stan program
  data = ci_data,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  refresh = 1000          # show progress every 'refresh' iterations
)

plot(fit1, pars = c("beta","sigma","effect"), ci_level = 0.5, outer_level = 0.95)
plot(fit1, plotfun = "trace", pars = c("beta","sigma","effect"), inc_warmup = TRUE)
fit1

################################################################################

set.seed(1976)

N <- 100
x1 <- rnorm(N, 0, 1)
x2 <- rnorm(N, 1, 2)
e <- rnorm(N, 0, 1)
y <- 1 + 3*x1 + 2*x2 + e
miss <- rbinom(N, size = 1, prob = 0.1)
y_obs <- y[miss == 0]
y_miss <- y[miss == 1]
X <- as.matrix(cbind(rep(1,N), x1, x2))
X_obs <- X[miss == 0,]
X_miss <- X[miss == 1,]
N_miss <- sum(miss)
N_obs <- N - sum(miss)
K <- ncol(X)
beta_prior_mean <- c(1, 1, 1)
beta_prior_sd <- c(10, 10, 10)
sigma_prior_mean <- 3

test_data <- list(
  N = N,
  K = K,
  y_obs = y_obs,
  X_obs = X_obs,
  X_miss = X_miss,
  N_obs = N_obs,
  N_miss = N_miss,
  beta_prior_mean = beta_prior_mean,
  beta_prior_sd = beta_prior_sd,
  sigma_prior_mean = sigma_prior_mean
)

stan_char_string <- "
data {
  int N;
  int K;
  int N_obs;
  int N_miss;
  matrix[N_obs,K] X_obs;
  matrix[N_miss,K] X_miss;
  vector[N_obs] y_obs;
  vector[K] beta_prior_mean;
  vector[K] beta_prior_sd;
  real sigma_prior_mean;
}
parameters {
  vector[K] beta;
  real error_sigma;
  vector[N_miss] y_miss;
}
transformed parameters {
  vector[N] y;
  matrix[N,K] X;
  vector[N] mu;
  for (i in 1:N_obs){
    y[i] = y_obs[i];
    X[i,] = X_obs[i,];
  }
  for (i in 1:N_miss){
    y[N_obs + i] = y_miss[i];
    X[N_obs + i,] = X_miss[i,];
  }
  mu = X * beta;
}
model {
  beta ~ normal(beta_prior_mean, beta_prior_sd);
  error_sigma ~ exponential(sigma_prior_mean);
  y ~ normal(mu, error_sigma);
}"

#Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")

fit1 <- stan(model_code = stan_char_string, data = test_data, pars = c("beta","error_sigma"), iter = 2000, warmup = 1000, chains = 1, cores = 1)
fit1

plot(fit1)

################################################################################

set.seed(1976)

N <- 100
x1 <- rnorm(N, 0, 1)
x2 <- rnorm(N, 1, 2)
e <- rnorm(N, 0, 1)
effect <- 4
y_1 <- 1 + 3*x1 + 2*x2 + effect + e
y_0 <- 1 + 3*x1 + 2*x2 + e
w <- rbinom(N, size = 1, prob = 0.5)
y_obs <- y_1*w + y_0*(1-w)
y_miss <- y_1*(1-w) + y_0*w
X <- as.matrix(cbind(rep(1,N), x1, x2))
K <- ncol(X)
beta_prior_mean <- c(1, 1, 1)
beta_prior_sd <- c(10, 10, 10)
sigma_prior_alpha <- 0.01
sigma_prior_beta <- 0.01

test_data <- list(
  N = N,
  K = K,
  y_obs = y_obs,
  X = X,
  N_miss = N_miss,
  beta_prior_mean = beta_prior_mean,
  beta_prior_sd = beta_prior_sd,
  sigma_prior_mean = sigma_prior_mean
)

stan_char_string <- "
data {
int N;
int K;
int N_obs;
int N_miss;
matrix[N_obs,K] X_obs;
matrix[N_miss,K] X_miss;
vector[N_obs] y_obs;
vector[K] beta_prior_mean;
vector[K] beta_prior_sd;
real sigma_prior_mean;
}
parameters {
vector[K] beta;
real error_sigma;
vector[N_miss] y_miss;
}
transformed parameters {
vector[N] y;
matrix[N,K] X;
vector[N] mu;
for (i in 1:N_obs){
y[i] = y_obs[i];
X[i,] = X_obs[i,];
}
for (i in 1:N_miss){
y[N_obs + i] = y_miss[i];
X[N_obs + i,] = X_miss[i,];
}
mu = X * beta;
}
model {
beta ~ normal(beta_prior_mean, beta_prior_sd);
error_sigma ~ exponential(sigma_prior_mean);
y ~ normal(mu, error_sigma);
}"

#Sys.setenv(PATH = paste("C:/Rtools/bin", Sys.getenv("PATH"), sep=";"))
#Sys.setenv(BINPREF = "C:/Rtools/mingw_64/bin")

fit1 <- stan(model_code = stan_char_string, data = test_data, pars = c("beta","error_sigma"), iter = 2000, warmup = 1000, chains = 1, cores = 1)
fit1

plot(fit1)
