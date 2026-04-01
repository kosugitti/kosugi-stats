library(cmdstanr)

stancode <- "
data {
  int<lower=0> J; // number of schools
  array[J] real y; // estimated treatment effects
  array[J] real<lower=0> sigma; // s.e. of effect estimates
}
parameters {
  real mu;
  real<lower=0> tau;
  array[J] real eta;
}
transformed parameters {
  array[J] real theta;
  for (j in 1:J)
  theta[j] = mu + tau * eta[j];
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
"

schools_dat <- list(
  J = 8, y = c(28, 8, -3, 7, -1, 1, 18, 12),
  sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)

modelFile <- write_stan_file(stancode)
model <- cmdstan_model(modelFile)
model$check_syntax()
fit.samp <- model$sample(
  data = schools_dat,
  iter_warmup = 1000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 4,
  refresh = 200
)
fit.samp$print()
