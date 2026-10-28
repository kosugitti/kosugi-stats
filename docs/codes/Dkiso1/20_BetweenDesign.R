# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第20章 Rによる分散分析
## ANOVA君のソース anovakun_489.txt をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

## code 20.1 ソースを読み込む
source("anovakun_489.txt")

## code 20.2 ANOVA君のソースをネットからとってくる
## (ファイルをダウンロードしていない場合はこちら。上の行と両方実行する必要はない)
# source("https://bit.ly/3WL8VYR")


# 分散分析用の関数の導入 ------------------------------------------------------

## code 20.3 一要因3水準の分散分析をanovakunで実行
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

# anovakun関数で分散分析を実行
anovakun(Example1[, -1], "As", 3, eps = TRUE)


# 分散分析のデータ形式 --------------------------------------------------------

## code 20.4 データの準備
## (テキストではここで library(tidyverse) をしているが，このファイルでは冒頭の準備で読み込んでいる)
MixedDesign <- data.frame(
  id = 1:12,
  condition = rep(1:3, each = 4),
  stimuli1 = c(6, 6, 5, 5, 7, 4, 7, 6, 4, 3, 4, 6),
  stimuli2 = c(8, 3, 2, 5, 9, 7, 1, 6, 4, 9, 2, 3)
)
# 要因型にする
MixedDesign$condition <- factor(MixedDesign$condition,
  labels = c("control", "exp1", "exp2")
)
print(MixedDesign)

## code 20.5 ワイド型からロング型への変換
MixedDesign_long <- MixedDesign %>%
  pivot_longer(
    cols = c(stimuli1, stimuli2),
    names_to = "stimuli",
    values_to = "value"
  )
print(MixedDesign_long)


# 二要因Between計画の実際 ----------------------------------------------------

## code 20.6 データフレームにする
Between2 <- data.frame(
  temp = rep(1:2, each = 6),
  maker = rep(rep(1:2, each = 3), 2),
  value = c(13, 11, 12, 7, 6, 8, 9, 9, 9, 13, 11, 9)
)
Between2$temp <- factor(Between2$temp, labels = c("Hot", "Cold"))
Between2$maker <- factor(Between2$maker, labels = c("A", "B"))

## code 20.7 anovakunで実行
anovakun(Between2, "ABs", 2, 2, eps = TRUE)

## code 20.8 2×3のデータを作りanovakunで実行
Between23 <- data.frame(
  A = rep(1:2, each = 15),
  B = rep(rep(1:3, each = 5), 2),
  value = c(
    46, 44, 32, 40, 49, # a1・b1
    57, 54, 44, 56, 53, # a1・b2
    40, 24, 39, 25, 33, # a1・b3
    50, 34, 42, 30, 43, # a2・b1
    55, 44, 52, 63, 59, # a2・b2
    58, 55, 53, 61, 59 # a2・b3
  )
)
Between23$A <- factor(Between23$A, labels = c("a1", "a2"))
Between23$B <- factor(Between23$B, labels = c("b1", "b2", "b3"))

## 二要因群間計画(2×3)
anovakun(Between23, "ABs", 2, 3, eps = TRUE)


# 分析結果の可視化 ------------------------------------------------------------

## code 20.9 可視化のコード
Between23 %>%
  group_by(A, B) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = B, y = mean, fill = A)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(0.9),
    width = 0.8
  ) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(0.9), width = 0.25
  ) +
  labs(
    title = "要因Aと要因Bによる平均値の比較",
    x = "要因B",
    y = "平均値",
    fill = "要因A"
  ) +
  theme_bw()

## code 20.10 箱ひげ図のコード
Between23 %>%
  ggplot(aes(x = B, y = value, fill = A)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = A),
    position = position_jitterdodge(jitter.width = 0.2),
    size = 2
  ) +
  labs(
    title = "要因Aと要因Bによる測定値の分布",
    x = "要因B",
    y = "測定値",
    fill = "要因A",
    color = "要因A"
  ) +
  theme_bw()

## code 20.11 交互作用の可視化のコード
Between23 %>%
  group_by(A, B) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = B, y = mean, group = A, color = A)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(
    title = "要因Aと要因Bの交互作用",
    x = "要因B",
    y = "平均値",
    color = "要因A"
  ) +
  theme_bw()
