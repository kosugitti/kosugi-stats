# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第19章 F検定/分散分析の基礎
## effectsize パッケージを使うので，入っていない人は先にインストールしておく
## install.packages("effectsize")

rm(list = ls())

# Rで一要因群間計画の分析 -----------------------------------------------------

## 変数の型

## code 19.1 オブジェクトに代入
obj <- data.frame(
  list(
    name = c("kosugi", "yamada", "ueda"),
    gender = c(1, 2, 1),
    height = c(167, 170, 180),
    weight = c(60, 70, 80)
  )
)
str(obj)

## code 19.2 因子型にする
obj$gender <- factor(obj$gender, labels = c("male", "female"))
str(obj)

## 一要因Between計画の計算

## code 19.3 データの準備
# データフレームを組み上げる
Example1 <- data.frame(
  id = 1:12,
  condition = rep(1:3, each = 4),
  value = c(6, 6, 5, 5, 7, 4, 7, 6, 4, 3, 4, 6)
)
# 要因型にする
Example1$condition <- factor(Example1$condition,
  labels = c("control", "exp1", "exp2")
)

## 線形モデルとして分析

## code 19.4 lm関数で分析
result.lm1 <- lm(value ~ condition, data = Example1)
summary(result.lm1)

## 帰無仮説検定として分析

## code 19.5 分散分析としてみる
anova(result.lm1)

## code 19.6 aov関数で実行する
result.aov <- aov(value ~ condition, data = Example1)
summary(result.aov)


# Rで下位検定 -----------------------------------------------------------------

## code 19.7 事後比較
TukeyHSD(result.aov)

## code 19.8 効果量の算出
library(effectsize)
epsilon_squared(result.aov)
