# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 7. Alternatives for ANOVA #2. Rstan Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(MASS)
## MAP関数
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}
## MCMCサンプルをデータフレームにする関数
MCMCtoDF <- function(fit) {
  fit %>%
    rstan::extract() %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    tibble::rowid_to_column("iter") %>%
    dplyr::select(-lp__) %>%
    tidyr::pivot_longer(-iter) -> MCMCsample
  return(MCMCsample)
}

## MCMCデータフレームを要約する関数
MCMCsummary <- function(MCMCsample) {
  MCMCsample %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      EAP = mean(value),
      MED = median(value),
      MAP = map_estimation(value),
      SD = sd(value),
      L95 = quantile(value, prob = 0.025),
      U95 = quantile(value, prob = 0.975),
      Rhat = posterior::rhat_basic(value)
    ) %>%
    mutate(across(where(is.numeric), ~ num(., digits = 3)))
}


# リバース・リバースエンジニアリング -------------------------------------------------------

mu <- c(50, 50)
sd1 <- 10
sd2 <- 5
rho <- 0.7
SIG <- matrix(ncol = 2, nrow = 2)
SIG[1, 1] <- sd1 * sd1
SIG[2, 2] <- sd2 * sd2
SIG[1, 2] <- sd1 * sd2 * rho
SIG[2, 1] <- sd1 * sd2 * rho
N <- 100
X <- mvrnorm(N, mu, SIG)

dataSet <- list(N = N, X = X)

model <- rstan::stan_model("rstan/paired_t.stan")
fit <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


# tidyなデータ ----------------------------------------------------------------

X <- mvrnorm(N, mu, SIG)
tidy_data <- X %>%
  as.data.frame() %>%
  rename(pre = V1, post = V2) %>%
  as_tibble() %>%
  rowid_to_column("ID") %>%
  pivot_longer(-ID) %>%
  mutate(cond = if_else(name == "pre", 1, 2))

dataSet <- list(
  L = NROW(tidy_data),
  N = max(tidy_data$ID),
  IDindex = tidy_data$ID,
  Condition = tidy_data$cond,
  val = tidy_data$value
)

model <- rstan::stan_model("rstan/paired_t_tidy.stan")
fit <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


# 課題 --------------------------------------------------------------------

dat_raw <- data.frame(
  ID = 1:4,
  period1 = c(10, 9, 4, 7),
  period2 = c(5, 4, 2, 3),
  period3 = c(9, 5, 3, 5)
)

tidy_dat <- dat_raw %>%
  pivot_longer(-ID) %>%
  mutate(name = as.factor(name) %>% fct_relevel("period1", "period2")) %>%
  mutate(cond = as.numeric(name))
