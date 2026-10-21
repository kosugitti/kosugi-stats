# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第29章 ベイズを実践する
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく
## brms を使うには，先に章末の補遺の手順で Stan の環境を準備しておく
## (このファイルの最後の「補遺」の節を参照)

# brmsによる実用的モデリング --------------------------------------------------

## lm()をbrm()に書き換える

## code 29.1 データの準備とパッケージの読み込み
rm(list = ls())
library(tidyverse)
library(brms)
dat <- read_csv("BaseballDecade.csv",
  na = "NA",
  locale = locale(encoding = "utf8")
) %>%
  filter(Year == "2020年度")
batter <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::select(height, weight, HR, AtBats)

## code 29.2 最尤推定とベイズ推定を並べてみる
## (コンパイルとMCMCに少し時間がかかる)
# 第24講でやった最尤推定
result.LM <- lm(weight ~ height, data = batter)
# 今回のベイズ推定
result.BRM <- brm(weight ~ height, data = batter, seed = 12345,
                  backend = "cmdstanr")
# 最尤推定
summary(result.LM)
# ベイズ推定
summary(result.BRM)


# 補遺・Stan環境の準備 --------------------------------------------------------
## ここはインストールの手順なので，すべてコメントにしてある。
## 必要な人だけ，テキストの手順を読んでから # を外して1回だけ実行する。
## (先に Mac は Xcode，Windows は Rtools を入れておくこと)

## cmdstanr パッケージを入れる
# install.packages("cmdstanr",
#   repos = c("https://stan-dev.r-universe.dev",
#             getOption("repos")))

## cmdstanr を使って Stan(cmdstan)を入れる
# cmdstanr::install_cmdstan()

## Stan の場所を R に教える(パスはインストールの最後に表示されたものを貼る)
# cmdstanr::set_cmdstan_path("ここにパスを貼る")

## code 29.3 Stanサンプルコード
# library(cmdstanr)
# file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
# mod <- cmdstan_model(file)
