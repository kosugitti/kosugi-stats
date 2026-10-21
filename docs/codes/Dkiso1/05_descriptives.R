# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第5章 記述統計量
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# データの読み込み ------------------------------------------------------------

## code 5.1 データの読み込み
rm(list = ls())
library(tidyverse)
dat <- read.csv("BaseballDecade.csv", na.strings = "NA")
dat2011 <- dat %>% filter(Year == "2011年度")

## code 5.2 データの確認
str(dat2011)
head(dat2011)

## code 5.3 記述統計量の算出
summary(dat2011)


# Rによる統計量の算出 ----------------------------------------------------------

## code 5.4 平均値の算出
mean(dat2011$salary)

## code 5.5 平均値の算出(欠測値を除く)
mean(dat2011$HR, na.rm = TRUE)

## code 5.6 中央値の算出
median(dat2011$salary)

## code 5.7 分散と標準偏差の算出
var(dat2011$salary)
sd(dat2011$salary)

## code 5.8 最大値と最小値の算出
max(dat2011$salary)
min(dat2011$salary)

## code 5.9 四分位数の算出
quantile(dat2011$salary)

## code 5.10 任意のパーセンタイルの算出
quantile(dat2011$salary, probs = c(0.3, 0.75))


# Rによるデータの可視化 --------------------------------------------------------

## code 5.11 ヒストグラムの描画
hist(dat2011$salary)

## code 5.12 箱ひげ図の描画
boxplot(dat2011$salary)


# 補遺 データハンドリング ------------------------------------------------------

## tidyverse の read_csv で読み込む
dat <- read_csv("BaseballDecade.csv", na = "NA")

## code 5.13 select関数
dat %>% select(height, weight)

## code 5.14 filter関数
dat %>% filter(Year == "2020年度")

## code 5.15 filter関数
# 身長が180cm以上の人だけ取り出す
dat %>% filter(height >= 180)
# 体重が100kg以下の人だけ取り出す
dat %>% filter(weight <= 100)
# 投手だけ取り出す
dat %>% filter(position == "投手")
# 野手だけ取り出す，野手とは投手でない人のことです
dat %>% filter(position != "投手")
# どちらかの条件を満たす，という場合
dat %>% filter(position == "捕手" | position == "内野手" |
  position == "外野手")

## code 5.16 mutate関数
dat %>% select(AtBats, Hit) %>% mutate(BattingAverage = Hit / AtBats)
