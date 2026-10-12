# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 3. Seven scientist Cmdstanr Version
## テキスト第5講「ベイジアンアプローチと確率的プログラミング」のコード(番号はテキストのコード番号)
## Stanファイル sevenScientist.stan(テキストのコード5.1)をこのコードと同じフォルダに置くこと

# 環境の準備と共通部分(コード5.2) ----------------------------------------------
rm(list = ls())
library(tidyverse)
library(bayesplot)
# データ
x <- c(-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056)

## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 関数集はサイトの「データ解析応用」ページの「事前準備」からダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく
source("00_Appendix2_functions.R")

# cmdstanrパッケージによる呼び出し(コード5.3) ----------------------------------
library(cmdstanr)
model <- cmdstan_model("sevenScientist.stan")
fit1cmdstan <- model$sample(
  data = list(Y = x),
  chains = 4,
  parallel_chains = 4
)
sample <- fit1cmdstan$draws() %>%
  posterior::as_draws_df() %>%
  as_tibble()
# 出力の読み方(Rhat や有効サンプルサイズ)を確かめる
fit1cmdstan

# トレースプロットの描画(コード5.4) --------------------------------------------
fit1cmdstan$draws("mu") %>% bayesplot::mcmc_trace()

# cmdstanrパッケージによるオプション指定(コード5.5) ----------------------------
fit1cmdstan <- model$sample(
  data = list(Y = x),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 4000
)

# stanの結果を準備し関数に与える(コード5.6) ------------------------------------
fit1cmdstan %>% MCMCtoDF() %>% MCMCsummary()
