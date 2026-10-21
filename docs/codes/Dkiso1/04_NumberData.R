# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第4章 数値データの種類
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# コンピュータのデータ型と尺度水準の関係 ---------------------------------------

## code 4.1 データの読み込み
rm(list = ls())
dat <- read.csv("BaseballDecade.csv", na.strings = "NA")
str(dat)

## code 4.2 Factor型に変換する
dat$bloodType <- factor(dat$bloodType)
str(dat$bloodType)
levels(dat$bloodType)

## code 4.3 Ord.Factor型に変換する
dat$Year <- factor(dat$Year, ordered = TRUE)
str(dat$Year)
levels(dat$Year)
