# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 3. Seven scientist Rstan Version

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
    as.array() %>%
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


# データ ---------------------------------------------------------------------

x <- c(-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056)

# stanのコンパイル
## rstanの場合
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
model <- rstan::stan_model("rstan/sevenScientist.stan")
fit <- rstan::sampling(model, data = list(Y = x))


# トレースプロット ---------------------------------------------------------------

traceplot(fit)

# Stanのオプション指定 ------------------------------------------------------------

fit <- rstan::sampling(model,
  data = list(Y = x),
  chains = 4,
  iter = 5000,
  warmup = 1000
)

# 結果の出力 -------------------------------------------------------------------

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()
