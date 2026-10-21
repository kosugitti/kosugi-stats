# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第9章 Rをつかった回帰分析
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# データの読み込み ------------------------------------------------------------

## code 9.1 データの読み込みと記述統計の確認
rm(list = ls())
library(tidyverse)
dat <- read_csv("BaseballDecade.csv", na = "NA",
  locale = locale(encoding = "utf8")) %>%
  filter(Year == "2020年度")
summary(dat)


# 散布図の描画 ----------------------------------------------------------------

## code 9.2 散布図の作成
batter <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::select(height, weight)
g <- ggplot(batter, aes(x = height, y = weight))
g <- g + geom_point()
g

## code 9.3 回帰線を引く
g <- g + geom_smooth(method = "lm", se = FALSE)
g


# 線形回帰の実行 --------------------------------------------------------------

## code 9.4 回帰分析の実行
result <- lm(weight ~ height, data = batter)
summary(result)


# 回帰診断とQ-Qプロット -------------------------------------------------------

## code 9.5 回帰分析の診断
plot(result)


# 残差をデータで確認しておこう ------------------------------------------------

## code 9.6 回帰分析のその他の情報
batter$residuals <- result$residuals
batter$yhat <- result$fitted.values
batter
summary(batter)
plot(batter)
cor(batter)


# 重回帰分析 ------------------------------------------------------------------

## code 9.7 重回帰分析のデータを作って可視化
batter2 <- dat %>%
  dplyr::filter(position != "投手") %>%
  dplyr::select(HR, Hit, salary) %>%
  na.omit()
plot(batter2)
cor(batter2)

## code 9.8 重回帰分析の実行
result2 <- lm(salary ~ HR + Hit, data = batter2)
summary(result2)

## code 9.9 標準化したデータで重回帰分析を実行
batter2z <- scale(batter2) %>% as.data.frame()
summary(batter2z)
result2z <- lm(salary ~ HR + Hit, data = batter2z)
summary(result2z)
