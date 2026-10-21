# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第24章 尤度と線形モデル
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# 準備(第9章のコード9.1と9.2) -----------------------------------------------

## テキストのコード24.1の前の指示どおり，第9章のコード9.1と9.2の前半で
## batter オブジェクト(2020年度の打者の身長と体重)を作っておく
rm(list = ls())
library(tidyverse)
dat <- read_csv("BaseballDecade.csv", na = "NA",
  locale = locale(encoding = "utf8")) %>%
  filter(Year == "2020年度")
batter <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::select(height, weight)


# 回帰分析の最尤推定 ----------------------------------------------------------

## code 24.1 二つの推定法の比較
# 最小二乗法の推定
result.LS <- lsfit(x = batter$height, y = batter$weight)
result.LS$coefficients
# 最尤推定
result.LM <- lm(weight ~ height, data = batter)
summary(result.LM)

## code 24.2 回帰分析の結果オブジェクトをプロットに入れる
par(mfrow = c(2, 2)) # 2x2の配置にする
plot(result.LM)
