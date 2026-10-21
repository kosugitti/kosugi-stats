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


# 分散分析のデータ形式 --------------------------------------------------------

## code 20.3 データの準備
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

## code 20.4 ワイド型からロング型への変換
MixedDesign_long <- MixedDesign %>%
  pivot_longer(
    cols = c(stimuli1, stimuli2),
    names_to = "stimuli",
    values_to = "value"
  )
print(MixedDesign_long)


# 二要因Between計画の実際 ----------------------------------------------------

## code 20.5 データフレームにする
Between2 <- data.frame(
  temp = rep(1:2, each = 6),
  maker = rep(rep(1:2, each = 3), 2),
  value = c(13, 11, 12, 7, 6, 8, 9, 9, 9, 13, 11, 9)
)
Between2$temp <- factor(Between2$temp, labels = c("Hot", "Cold"))
Between2$maker <- factor(Between2$maker, labels = c("A", "B"))

## code 20.6 anovakunで実行
anovakun(Between2, "ABs", 2, 2, eps = TRUE)

## code 20.7 線形モデルとして実行
result.lm <- lm(value ~ temp * maker, data = Between2)
summary(result.lm)

## code 20.8 分散分析としてみる
anova(result.lm)


# 分析結果の可視化 ------------------------------------------------------------

## code 20.9 可視化のコード
Between2 %>%
  group_by(temp, maker) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = temp, y = mean, fill = maker)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(0.9),
    width = 0.8
  ) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(0.9), width = 0.25
  ) +
  labs(
    title = "温度とメーカーによる評価値の比較",
    x = "温度条件",
    y = "評価値の平均",
    fill = "メーカー"
  ) +
  theme_bw()

## code 20.10 箱ひげ図のコード
Between2 %>%
  ggplot(aes(x = temp, y = value, fill = maker)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = maker), position = position_jitterdodge(jitter.width = 0.2), size = 2) +
  labs(
    title = "温度とメーカーによる評価値の分布",
    x = "温度条件",
    y = "評価値",
    fill = "メーカー",
    color = "メーカー"
  ) +
  theme_bw()

## code 20.11 交互作用の可視化のコード
Between2 %>%
  group_by(temp, maker) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = temp, y = mean, group = maker, color = maker)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(
    title = "温度とメーカーの交互作用",
    x = "温度条件",
    y = "評価値の平均",
    color = "メーカー"
  ) +
  theme_bw()
