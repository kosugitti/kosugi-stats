# 心理学データ解析/伴走サイトコード -----------------------------------------------------
#  Programmed by kosugitti
#  Licence ; Creative Commons BY-SA license (CC BY-SA) version 4.0
## 第6章 相関
## サンプルデータ BaseballDecade.csv をサイトからダウンロードし，
## このコードと同じプロジェクトフォルダに置いておく

# Rによる相関分析の実践 --------------------------------------------------------

## code 6.1 データの準備
# Rのプロジェクトを立ち上げ，プロジェクトフォルダを開いたうえで
# 以下のコマンドを実行してください
rm(list = ls())
library(tidyverse)
# データを読み込む
dat <- read_csv("BaseballDecade.csv")
# データを一部絞り込みます
dat <- dat[dat$Year == "2020年度", ]
# または
# dat <- dat %>% filter(Year == "2020年度")


# 散布図の作成 ----------------------------------------------------------------

## code 6.2 R baseによる散布図の作成
# 身長と体重の散布図（R base）
plot(dat$height, dat$weight,
  xlab = "身長(cm)", ylab = "体重(kg)",
  main = "身長と体重の散布図"
)

## code 6.3 ggplot2による散布図の作成
# ggplot2による散布図
ggplot(dat, aes(x = height, y = weight)) +
  geom_point() +
  labs(
    x = "身長(cm)", y = "体重(kg)",
    title = "身長と体重の散布図（ggplot2）"
  ) +
  theme_bw()


# ピアソンの相関係数の計算 ----------------------------------------------------

## code 6.4 ピアソンの相関係数の計算
# ピアソンの相関係数の計算
cor(dat$height, dat$weight)
# または
cor(dat$height, dat$weight, method = "pearson")


# スピアマンの順位相関係数の計算 ----------------------------------------------

## code 6.5 年俸が高い野手TOP20の抽出
# データを読み込む
dat <- read_csv("BaseballDecade.csv")

# 野手（投手以外）を選び、年俸で降順にソートしてTOP20を取得
top20_players <- dat %>%
  filter(Year == "2020年度") %>% # 2020年度のデータ
  filter(position != "投手") %>% # 投手以外（野手）を選択
  arrange(desc(salary)) %>% # 年俸の降順でソート
  head(20) %>% # 上位20人を取得
  select(Name, team, salary, HR) # 必要な列だけを選択

## code 6.6 スピアマンの相関係数の計算
cor(top20_players$salary, top20_players$HR, method = "spearman")


# カイ二乗値の計算 ------------------------------------------------------------

## code 6.7 チームと血液型のクロス集計表の作成
# 3チームと血液型変数を抜き出し、それぞれをFactor型にした上で、
# クロス集計表をだす
team_blood <- dat %>%
  filter(Year == "2020年度", team %in% c("Tigers", "Giants", "Carp")) %>%
  select(team, bloodType)

# Factor型に変換
team_blood$team <- factor(team_blood$team)
team_blood$bloodType <- factor(team_blood$bloodType)

# クロス集計表の作成
cross_table <- table(team_blood$team, team_blood$bloodType)
print(cross_table)

## code 6.8 カイ二乗値の計算
chi_sq_test <- chisq.test(cross_table)
print(chi_sq_test)


# クラメールのVの計算 ---------------------------------------------------------

## code 6.9 クラメールのVの計算
chi_sq_value <- chi_sq_test$statistic
n <- sum(cross_table) # サンプルサイズ
r <- nrow(cross_table) # 行数
c <- ncol(cross_table) # 列数
min_rc <- min(r, c) # r と c の小さい方

# クラメールのVの計算
cramers_v <- sqrt(chi_sq_value / (n * (min_rc - 1)))
