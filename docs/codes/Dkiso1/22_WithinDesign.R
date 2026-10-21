# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第22章 RによるWithinデザインの分散分析
## ANOVA君のソース anovakun_489.txt をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

rm(list = ls())

# 一要因Within計画の実際 ------------------------------------------------------

## code 22.1 ソースを読み込む
# プロジェクトフォルダにソースコードをダウンロードしていればこちら
source("anovakun_489.txt")
# ダウンロードしていない場合はこちら(行頭の#を外してネットから読み込む)
# source("https://bit.ly/3WL8VYR")

## code 22.2 データフレームにする
Example3 <- data.frame(
  ID = 1:4,
  Time1 = c(10, 9, 4, 7),
  Time2 = c(5, 4, 2, 3),
  Time3 = c(9, 5, 3, 5)
)

## code 22.3 anovakunで実行
anovakun(Example3[, 2:4], "sA", 3, eps = TRUE)


# 二要因Within計画の実際 ------------------------------------------------------

## code 22.4 二要因内×内デザインのデータと分析
dat <- data.frame(
  id = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  m1_t1 = c(47, 60, 79, 54, 55, 84, 63, 41, 53, 57),
  m1_t2 = c(90, 71, 83, 41, 41, 103, 74, 34, 65, 53),
  m1_t3 = c(44, 63, 48, 39, 58, 20, 96, 62, 44, 83),
  m2_t1 = c(38, 39, 60, 49, 45, 62, 46, 28, 35, 40),
  m2_t2 = c(55, 49, 47, 63, 49, 58, 48, 30, 61, 47),
  m2_t3 = c(52, 59, 50, 63, 54, 56, 57, 64, 52, 72)
)

anovakun(dat[, -1], "sAB", 2, 3, eps = TRUE, gg = TRUE)


# 単純効果の分散分析表の分割 --------------------------------------------------

## A at b1 の単純効果

## code 22.5 A at b1の単純効果検定
SimpleEffectA_at_b1 <- dat[, c(2, 5)]
anovakun(SimpleEffectA_at_b1, "sA", 2)

## B at a2 の単純効果

## code 22.6 B at a2の単純効果検定
SimpleEffectB_at_a2 <- dat[, c(5, 6, 7)]
anovakun(SimpleEffectB_at_a2, "sA", 3, gg = TRUE)
