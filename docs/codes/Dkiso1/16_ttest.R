# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第16章 二群の平均値差
## effsize パッケージを使うので，入っていない人は先にインストールしておく
## (install.packages("effsize") はコンソールで1回だけ実行する。スクリプトには書かない)

# Rによる実践；対応のあるt検定 ------------------------------------------------

## code 16.1 t検定を行う
rm(list = ls())
X <- c(34, 25, 13, 21, 13, 26, 19, 12, 19, 19)
Y <- c(35, 30, 16, 27, 20, 28, 16, 14, 18, 21)
t.test(Y, X, paired = T, alternative = "greater")


# Rによる実践；対応のないt検定 ------------------------------------------------

## code 16.2 formulaで指定
X <- c(25, 15, 24, 17, 22)
Y <- c(31, 28, 30, 19, 18)
dat <- data.frame(condition = c(rep(1, 5), rep(2, 5)), value = c(X, Y))
dat$condition <- factor(dat$condition,
  labels = c("control", "experimental"))
t.test(value ~ condition, data = dat)

## code 16.3 分散のオプション指定
t.test(value ~ condition, data = dat, var.equal = T)

## code 16.4 効果量を求める
# パッケージの読み込み
library(effsize)
cohen.d(value ~ condition, data = dat, hedges.correction = T)

## code 16.5 power.t.test関数
power.t.test(power = 0.8, delta = 0.7, sig.level = 0.05)

## code 16.6 power.t.testでnを固定
power.t.test(n = 5, delta = 0.7, sig.level = 0.05)


# 補遺 対応のあるt検定の効果量を計算する --------------------------------------

## code 16.7 cohenのd
## 1-4行目: code 16.2 で X, Y, dat を対応のないデータに上書きしているので，
## code 16.1 の事前・事後のデータを作り直す(code 16.8 もこのデータを使う)
X <- c(34, 25, 13, 21, 13, 26, 19, 12, 19, 19)
Y <- c(35, 30, 16, 27, 20, 28, 16, 14, 18, 21)
dat <- data.frame(condition = c(rep(1, 10), rep(2, 10)), value = c(X, Y))
dat$condition <- factor(dat$condition, labels = c("pre", "post"))
cohen.d(X, Y, paired = TRUE)

## code 16.8 cohenのdその2
## (警告メッセージが出るのはテキストどおり)
cohen.d(value ~ condition, data = dat, paired = TRUE)

## code 16.9 対応のある場合のcohen.d
X <- c(34, 25, 13, 21, 13, 26, 19, 12, 19, 19)
Y <- c(35, 30, 16, 27, 20, 28, 16, 14, 18, 21)
dat <- data.frame(condition = c(rep(1, 10), rep(2, 10)), value = c(X, Y))
dat$condition <- factor(dat$condition, labels = c("pre", "post"))
dat$ID <- rep(1:10, 2)
cohen.d(value ~ condition | Subject(ID), data = dat, paired = TRUE)
