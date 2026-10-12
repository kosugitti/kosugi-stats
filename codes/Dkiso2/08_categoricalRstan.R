# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 8. Kappa coefficients. rstan Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 関数集はサイトの「データ解析応用」ページの「事前準備」からダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく。Stanファイルも同じフォルダに置く
source("00_Appendix2_functions_rstan.R")

# モデル ---------------------------------------------------------------------

model <- rstan::stan_model("categorical1.stan")
fit <- rstan::sampling(model,
  data = list(K = 3, X = c(51, 45, 27)),
  chains = 4,
  iter = 6000,
  warmup = 1000
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
model <- rstan::stan_model("kappa.stan")
dataSet <- list(Y = c(55, 16, 14, 35))
fit <- sampling(model,
  data = dataSet,
  chains = 4,
  iter = 5000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()
