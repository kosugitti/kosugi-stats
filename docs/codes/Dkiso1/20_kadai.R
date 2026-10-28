# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第20章 課題用コード
## ANOVA君のソース anovakun_489.txt をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく
## データを作る部分と図を描く部分はすべて書いてある。書きかえないこと
## 自分で書くのは「ここに書く」とある2か所だけ

# 準備 ----------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

## ANOVA君を読み込む(テキストのコード20.1)
source("anovakun_489.txt")
## ファイルをダウンロードしていない場合はこちら(コード20.2)。上の行と両方実行する必要はない
# source("https://bit.ly/3WL8VYR")


# 課題1 一要因3水準の群間計画 --------------------------------------------------------
## 3つの記憶方略(反復・イメージ・精緻化)に6名ずつを割り当て，記憶テストの得点を測った

kadai1 <- data.frame(
  id = 1:18,
  strategy = rep(1:3, each = 6),
  score = c(
    49, 33, 45, 43, 37, 19, # 反復
    43, 40, 51, 45, 46, 43, # イメージ
    54, 54, 31, 69, 62, 58 # 精緻化
  )
)
kadai1$strategy <- factor(kadai1$strategy,
  labels = c("反復", "イメージ", "精緻化")
)

## 課題1 kadai1 を anovakun で分析する。効果量(epsilon^2)も出すこと
## (id の列は渡さないこと。テキストのコード20.3を参考に)
## ↓ここに書く


# 課題2 二要因の群間計画(3×2) ----------------------------------------------------------
## 学習方法(対面・オンライン・ハイブリッド)と復習(なし・あり)を組み合わせた6条件に
## 5名ずつを割り当て，学習後のテストの得点を測った

kadai2 <- data.frame(
  id = 1:30,
  method = rep(1:3, each = 10),
  review = rep(rep(1:2, each = 5), 3),
  score = c(
    53, 37, 49, 47, 41, # 対面・なし
    57, 50, 46, 36, 51, # 対面・あり
    21, 39, 36, 47, 41, # オンライン・なし
    51, 52, 40, 69, 54, # オンライン・あり
    40, 37, 42, 42, 19, # ハイブリッド・なし
    83, 81, 65, 70, 81 # ハイブリッド・あり
  )
)
kadai2$method <- factor(kadai2$method,
  labels = c("対面", "オンライン", "ハイブリッド")
)
kadai2$review <- factor(kadai2$review, labels = c("なし", "あり"))

## 課題2 kadai2 を anovakun で分析する。効果量(epsilon^2)も出すこと
## (id の列は渡さないこと。要因の順番と水準の数に気をつけること)
## ↓ここに書く


# 課題2の可視化(書いてあるので，そのまま実行する) --------------------------------------------

## 棒グラフ(平均と標準誤差。テキストのコード20.9)
kadai2 %>%
  group_by(method, review) %>%
  summarise(
    mean = mean(score),
    sd = sd(score),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = method, y = mean, fill = review)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(0.9),
    width = 0.8
  ) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(0.9), width = 0.25
  ) +
  labs(
    title = "学習方法と復習によるテスト得点の比較",
    x = "学習方法",
    y = "テスト得点の平均",
    fill = "復習"
  ) +
  theme_bw()

## 箱ひげ図(個々のデータを重ねる。テキストのコード20.10)
kadai2 %>%
  ggplot(aes(x = method, y = score, fill = review)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = review),
    position = position_jitterdodge(jitter.width = 0.2),
    size = 2
  ) +
  labs(
    title = "学習方法と復習によるテスト得点の分布",
    x = "学習方法",
    y = "テスト得点",
    fill = "復習",
    color = "復習"
  ) +
  theme_bw()

## 交互作用プロット(テキストのコード20.11)
kadai2 %>%
  group_by(method, review) %>%
  summarise(
    mean = mean(score),
    sd = sd(score),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = method, y = mean, group = review, color = review)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  labs(
    title = "学習方法と復習の交互作用",
    x = "学習方法",
    y = "テスト得点の平均",
    color = "復習"
  ) +
  theme_bw()
