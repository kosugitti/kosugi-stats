# 心理学データ解析応用2 補助関数集(cmdstanr版)
# テキスト「いんたーみっしょん；Stanの概略と環境の準備について」で紹介した
# 3つの関数をまとめたもの。第3回以降の課題で繰り返し使う。
# rstan を使う人は 00_Appendix2_functions_rstan.R を読み込むこと。
#
# MCMCsummary の列は EAP, MED, MAP, SD, L95, U95, Rhat(各回のサンプルコードと同じ)。
# 95%以外の区間(たとえば90%)が要るときは，MCMCtoDF の結果から
#   quantile(value, prob = c(0.05, 0.95))
# のように自分で計算すること。
#
# 使い方: 分析コードの冒頭で読み込む
#   source("00_Appendix2_functions.R")
#
# 各章のサンプルコードは単体で動くよう同じ関数を自前で定義しているので，
# そちらを使う場合はこのファイルは要らない。

pacman::p_load(tidyverse, posterior)

# MCMCサンプルを tidy なデータフレームにする -----------------------------
# 引数は cmdstanr の出力オブジェクト。lp__ などの隠し変数は落とす。
MCMCtoDF <- function(fit) {
  fit$draws() %>%
    posterior::as_draws_df() %>%
    tibble::as_tibble() %>%
    dplyr::select(-lp__, -.draw, -.chain, -.iteration) %>%
    tibble::rowid_to_column("iter") %>%
    tidyr::pivot_longer(-iter) -> MCMCsample
  return(MCMCsample)
}

# MAP推定値を返す -------------------------------------------------------
# カーネル密度推定で密度関数を当てがい，密度が最大になる位置を返す。
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}

# MCMCサンプルの要約表を作る --------------------------------------------
# 引数は MCMCtoDF の戻り値。
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
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ pillar::num(., digits = 3)))
}
