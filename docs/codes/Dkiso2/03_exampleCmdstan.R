# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 3. Seven scientist Cmdstanr Version

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(cmdstanr)
library(bayesplot)
## 補助関数(map_estimation・MCMCtoDF・MCMCsummary)を読み込む
## 関数集はサイトの「データ解析応用」ページの「事前準備」からダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく。Stanファイルも同じフォルダに置く
source("00_Appendix2_functions.R")

# データ ---------------------------------------------------------------------

x <- c(-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056)

# stanのコンパイル
## cmdstanの場合
library(cmdstanr)
model <- cmdstanr::cmdstan_model("sevenScientist.stan")
model$check_syntax()
fit <- model$sample(
  data = list(Y = x),
)


# トレースプロット ----------------------------------------------------------------

fit$draws() %>% bayesplot::mcmc_trace()

# Stanのオプション指定 ------------------------------------------------------------

fit <- model$sample(
  data = list(Y = x),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000
)

# 結果の出力 -------------------------------------------------------------------

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()
