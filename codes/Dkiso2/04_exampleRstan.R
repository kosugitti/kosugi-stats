# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 4. Alternatives for t-test. Rstan Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
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


# データなど -------------------------------------------------------------------

groupA <- c(30, 50, 70, 90, 60, 50, 70, 60)
groupB <- c(20, 40, 60, 40, 40, 50, 40, 30)

## t検定

t.test(groupA, groupB, var.equal = TRUE)


# Modeling ----------------------------------------------------------------
model <- rstan::stan_model("rstan/ttest01.stan")
dataSet <- list(X1 = groupA, X2 = groupB, N1 = 8, N2 = 8)

fit <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)


## 結果
fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


## 可視化
plot(fit, pars = c("mu1", "mu2"), show_density = TRUE)







# Welchの補正 ----------------------------------------------------------------

t.test(groupA, groupB, var.equal = FALSE)


# 微妙な差のデータ ----------------------------------------------------------------

groupA <- c(30, 50, 70, 90, 60, 50, 70, 60)
groupB <- c(30, 45, 60, 40, 60, 50, 40, 30)

## t検定

t.test(groupA, groupB, var.equal = FALSE)

## 同じモデルでこのデータで再推定
dataSet <- list(X1 = groupA, X2 = groupB, N1 = 8, N2 = 8)
fit <- rstan::sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# サンプリングから計算 --------------------------------------------------------------

## MCMCサンプルをデータフレームにする
fit %>%
  rstan::extract() %>%
  as.data.frame() %>%
  as_tibble() -> MCMCsample

MCMCsample %>%
  mutate(diff = mu1 - mu2) %>%
  mutate(FLG = if_else(diff > 1, 1, 0)) %>%
  summary()

# Generated quantities ----------------------------------------------------

model <- rstan::stan_model("rstan/ttest03.stan")

groupA <- c(30, 50, 70, 90, 60, 50, 70, 60)
groupB <- c(30, 45, 60, 40, 60, 50, 40, 30)

## 生成量を計算するモデルを実行
dataSet <- list(X1 = groupA, X2 = groupB, N1 = 8, N2 = 8)
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)


fit %>%
  MCMtoDF() %>%
  MCMCsummary()


# 課題 ----------------------------------------------------------------------
potatoA <- c(8.4, 11.3, 8.1, 11.2, 5.8, 6.3, 7.1, 10.9, 7.1, 6.5, 5.0, 3.0, 7.2, 6.5, 6.4, 6.4, 9.3, 8.3)
potatoB <- c(6.7, 7.2, 4.2, 11.0, 7.5, 8.9, 7.0, 8.0, 7.2, 4.2, 6.0, 9.0, 8.6, 9.0, 5.0)
