# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 8. Kappa coefficients. Cmdstan Version

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
    dplyr::select(-lp__, -.iteration) %>%
    dplyr::rename(iter = .draw, chain = .chain) %>%
    tidyr::pivot_longer(-c(chain, iter)) -> MCMCsample
  return(MCMCsample)
}

## MCMCデータフレームを要約する関数
MCMCsummary <- function(MCMCsample, prob = 0.95) {
  lower <- (1 - prob) / 2
  upper <- 1 - lower
  width <- round(prob * 100)
  MCMCsample %>%
    dplyr::arrange(name, chain, iter) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      EAP = mean(value),
      MED = median(value),
      MAP = map_estimation(value),
      SD = sd(value),
      L = quantile(value, prob = lower),
      U = quantile(value, prob = upper),
      Rhat = posterior::rhat(matrix(value, ncol = max(chain)))
    ) %>%
    dplyr::rename(!!paste0("L", width) := L,
                  !!paste0("U", width) := U) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ pillar::num(., digits = 3)))
}

# モデル ---------------------------------------------------------------------

model <- cmdstanr::cmdstan_model("cmdstan/categorical1.stan")
model$check_syntax()
fit <- model$sample(
  data = list(K = 3, X = c(51, 45, 27)),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# Kappa係数
### 定義に則って計算した場合
a <- 55
b <- 16
c <- 14
d <- 35
n <- a + b + c + d
p0 <- (a + d) / n
pe <- ((a + b) * (a + c) + (b + d) * (c + d)) / n / n
(p0 - pe) / (1 - pe)


### モデルによる推定
model <- cmdstanr::cmdstan_model("cmdstan/kappa.stan")
model$check_syntax()
dataSet <- list(Y = c(55, 16, 14, 35))
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
