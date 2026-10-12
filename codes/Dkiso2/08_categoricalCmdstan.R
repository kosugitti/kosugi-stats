# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 8. Kappa coefficients. Cmdstan Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(cmdstanr)
library(bayesplot)
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 中身はサイトの「データ解析応用」ページの「事前準備」とテキストの「いんたーみっしょん」を参照
source("https://stats.kosugitti.net/codes/Dkiso2/00_Appendix2_functions.R")

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
