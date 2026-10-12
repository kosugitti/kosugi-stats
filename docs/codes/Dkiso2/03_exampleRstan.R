# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 3. Seven scientist Rstan Version
## テキスト第5講のコード(テキストは cmdstanr で書いている。ここではそれを rstan で書き直したもの)
## Stanファイル sevenScientist.stan(テキストのコード5.1)をこのコードと同じフォルダに置くこと

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


# データ ---------------------------------------------------------------------

x <- c(-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056)

# stanのコンパイル
## rstanの場合
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
model <- rstan::stan_model("sevenScientist.stan")
fit <- rstan::sampling(model, data = list(Y = x))


# トレースプロット ---------------------------------------------------------------

## テキストのコード5.4に対応(mu のトレースプロット)
traceplot(fit, pars = "mu")

# Stanのオプション指定 ------------------------------------------------------------
## テキストのコード5.5(ウォームアップ1000・サンプル4000)と同じ。rstan の iter はウォームアップを含む数

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
