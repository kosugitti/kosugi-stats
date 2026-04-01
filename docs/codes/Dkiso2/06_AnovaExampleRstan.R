# 心理学データ解析応用/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## Lesson 6. Alternatives for ANOVA. Rstan Version

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

# サンプルデータ -----------------------------------------------------------------


model <- rstan::stan_model("rstan/BetweenAnova.stan")
Example <- matrix(c(6, 6, 5, 5, 7, 4, 7, 6, 4, 3, 4, 6), ncol = 4, byrow = T)
dataSet <- list(Lv = 3, N = 4, X = Example)

fit <- rstan::sampling(
  model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()

# リバース・リバースエンジニアリング -------------------------------------------------------

N <- 100
Lv <- 5
gm <- 50
sig <- 3

raw_effect <- runif(Lv - 1, -10, 10)
effect <- c(raw_effect, 0 - sum(raw_effect))
mu <- gm + effect

X <- rnorm(N * Lv, mu, sig)
dat <- data.frame(
  Idx = rep(1:Lv, N * Lv),
  value = X
)

model <- rstan::stan_model("rstan/BetweenAnova2.stan")
dataSet <- list(Lv = Lv, L = NROW(dat), idx = dat$Idx, X = dat$value)
fit <- rstan::sampling(
  model,
  data = dataSet,
  chains = 4,
  iter = 6000,
  warmup = 1000
)

fit %>%
  MCMCtoDF() %>%
  MCMCsummary()


# 発展課題 --------------------------------------------------------------------

## 自分のデータを使って，多群の平均値差を推定するモデルを適用してみてください。
## 提出に際しては，Stanモデル，Rコード，使用したデータを添付してください。

## 以下にあるのは，そのようなデータを持っていない人のための，サンプルデータの提供および加工コードです。
## このデータは2020年度の野球選手のデータです。12の球団がありますので，12球団それぞれに属する選手の体重の平均を推定してください。
## なおチームの変数と番号は次のような対応です。
### 1: Carp
### 2: DeNA
### 3: Dragons
### 4: Eagles
### 5: Fighters
### 6: GUabts
### 7: Lions
### 8: Lotte
### 9: Orix
### 10:Softbank
### 11:Swallows
### 12:Tigers

## 検定したい変数はweight，インデックス変数はteam2にあります。

## 以下がサンプルのデータセットです。オブジェクトbaseballからStanで用いるデータセットを作り，推定するコードを書いてください。
baseball <- read_csv("baseball2020.csv") %>%
  ## 変数の選択
  dplyr::select(team, weight) %>%
  ## チーム変数を要因型に
  dplyr::mutate(team = as.factor(team)) %>%
  ## チーム変数の名前を数字に置き換え
  dplyr::mutate(team2 = as.numeric(team)) %>%
  ## 欠測値の削除
  na.omit()
