# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 5. Alternatives for t-test2. Cmdstanr Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(cmdstanr)
library(bayesplot)
## MAP関数
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}

## MCMCサンプルをデータフレームにする関数
MCMCtoDF <- function(fit) {
  fit$draws() %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    dplyr::select(-lp__, -.draw, -.chain, -.iteration) %>%
    tibble::rowid_to_column("iter") %>%
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


# データなど -------------------------------------------------------------------

groupA <- c(30, 50, 70, 90, 60, 50, 70, 60)
groupB <- c(20, 40, 60, 40, 40, 50, 40, 30)

# Modeling ----------------------------------------------------------------

model <- cmdstanr::cmdstan_model("cmdstan/ttest05.stan")
model$check_syntax()
dataSet <- list(X1 = groupA, X2 = groupB, N1 = 8, N2 = 8)

fit <- model$sample(
  data = dataSet,
  chains = 4,
  iter_sampling = 5000,
  iter_warmup = 1000,
  parallel_chains = 4
)

## 結果
fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# 可視化 ---------------------------------------------------------------------

## MCMCサンプルをデータフレームにする
fit$draws() %>%
  posterior::as_draws_df() %>%
  tibble::as_tibble() %>%
  dplyr::select(-lp__, -.chain, -.iteration) %>%
  tidyr::pivot_longer(-.draw) -> MCMCsample

## 事後分布と事後予測分布
MCMCsample %>%
  dplyr::filter(str_detect(name, pattern = "pred1") | str_detect(name, pattern = "mu1")) %>%
  dplyr::mutate(name2 = if_else(name == "mu1", "mu1", "Xpred1")) %>%
  ggplot(aes(x = value, group = name2)) +
  geom_density() +
  facet_wrap(~name2) +
  xlim(-10, 150)

### 事後予測分布
pred <- MCMCsample %>%
  dplyr::filter(str_detect(name, pattern = "Xpred1")) %>%
  pivot_wider(names_from = name, values_from = value, id_cols = .draw) %>%
  dplyr::select(-.draw) %>%
  as.matrix()

bayesplot::ppc_hist(y = groupA, yrep = pred[sample(nrow(pred), 15), ])


# 優越率と閾上率 -----------------------------------------------------------------

model <- cmdstanr::cmdstan_model("cmdstan/ttest06.stan")
model$check_syntax()
groupA <- c(30, 50, 70, 90, 60, 50, 70, 60)
groupB <- c(20, 40, 60, 40, 40, 50, 40, 30)
dataSet <- list(X1 = groupA, X2 = groupB, N1 = 8, N2 = 8, C = 3)

fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# パラメータリカバリ ---------------------------------------------------------------

mu1 <- 50
diff <- 10
mu2 <- mu1 + diff
sig1 <- 5
sig2 <- 8

set.seed(12345)
N <- 10
X1 <- rnorm(N, mu1, sig1)
X2 <- rnorm(N, mu2, sig2)

## t検定(モーメント法による推定と判定)
t.test(X1, X2)

## ベイズ推定(MCMCによるベイズ推定と差の分布)
dataSet <- list(X1 = X1, X2 = X2, N1 = N, N2 = N)

model <- cmdstanr::cmdstan_model("cmdstan/ttest03.stan")
model$check_syntax()
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


# うまくいかない例 ----------------------------------------------------------------

mu1 <- 50
diff <- 18
mu2 <- mu1 + diff
sig1 <- 10
sig2 <- 15

set.seed(12345)
N <- 3
X1 <- rnorm(N, mu1, sig1)
X2 <- rnorm(N, mu2, sig2)

## t検定(モーメント法による推定と判定)
t.test(X1, X2)

## ベイズ推定(MCMCによるベイズ推定と差の分布)
dataSet <- list(X1 = X1, X2 = X2, N1 = N, N2 = N, C = 3)
model <- cmdstanr::cmdstan_model("cmdstan/ttest03.stan")
model$check_syntax()
fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# 課題 ----------------------------------------------------------------------
potatoA <- c(8.4, 11.3, 8.1, 11.2, 5.8, 6.3, 7.1, 10.9, 7.1, 6.5, 5.0, 3.0, 7.2, 6.5, 6.4, 6.4, 9.3, 8.3)
potatoB <- c(6.7, 7.2, 4.2, 11.0, 7.5, 8.9, 7.0, 8.0, 7.2, 4.2, 6.0, 9.0, 8.6, 9.0, 5.0)
dataSet <- list(X1 = potatoA, X2 = potatoB, N1 = length(potatoA), N2 = length(potatoB))
