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
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 中身はサイトの「データ解析応用」ページの「事前準備」とテキストの「いんたーみっしょん」を参照
source("https://stats.kosugitti.net/codes/Dkiso2/00_Appendix2_functions_rstan.R")


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
